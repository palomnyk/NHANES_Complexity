#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# This is a script for random forest for NHANES study
# Should take a table with predictor/explanatory/independant and a table
# with response/outcome/dependant variable.
# Returns accuracy and other metrics and feature importance.
# --------------------------------------------------------------------------
print("Loading external libraries.",flush = True)
# --------------------------------------------------------------------------
from cProfile import label
import math
import os, sys
from matplotlib import markers
import numpy as np
import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.api.types import is_string_dtype
from sklearn.metrics import accuracy_score, roc_auc_score, r2_score
import matplotlib.pyplot as plt
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})
import matplotlib.backends.backend_pdf
import matplotlib.colors as mcolors
from sklearn import model_selection
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
import argparse
import random
import pdb

# QT_DEBUG_PLUGINS=1
# --------------------------------------------------------------------------
print("Reading commmandline input with optparse.", flush = True)
# --------------------------------------------------------------------------

parser = argparse.ArgumentParser(description="This script runs a random forest test on various datasets.")
parser.add_argument("-e", "--pred_path", dest="pred_table", 
				  default = "output/tables/diet_2015.csv",
                  help="path, relative to homedir, to table with predictor/explanatory/independant vars",
				  metavar="pred_table")
parser.add_argument("-m", "--metadata_cols",
                  action="store_false", dest="meta_col",
                  help="Metadata columns to analyse")
parser.add_argument("-o", "--output_label", default="py_rf",
				  dest="output_label",
                  help="base label for output files (additional info will be added to it)")
parser.add_argument("-d", "--homedir",
                  default=os.path.expanduser(os.path.join(".",)),
                  help="path to working dir", dest="homedir", metavar="homedir")
parser.add_argument("-p", "--project", default="string",
                  help="project folder", metavar="project")
parser.add_argument("-a", "--use_all_meta", default=False,
                  help="use all metadata", metavar="use_all_meta")
parser.add_argument("-f", "--metada_fn", 
					default="output/tables/BP_2015.csv", dest="meta_fn",
                  help="path, relative to homedir, to use as metadata.", 
					metavar="meta_fn")
parser.add_argument("-r", "--response_fn", default="", dest="resp_fn",
                  help="Name of file that holds response vars.", 
									metavar="resp_fn")
parser.add_argument("-l", "--delimiter", default="\t",
                  help="File delimiting symbol for metadata. Default is tab.",
									metavar="delim", dest="delim")
parser.add_argument("-i", "--meta_index_col", default=0,
                  help="Name of column to use as row name for metadata",
                  metavar="meta_index_col", dest="meta_index_col")
parser.add_argument("-t", "--title", default=False,
                  help="Title for visualizations",
                  metavar="title", dest="title")

options, unknown = parser.parse_known_args()

print(options)

"""
time python lib/scripts/ml/rand_for_reg-sing_col_resp-single_col_divide.py \
	--response_fn output/tables/BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/d1_diet_2015_match_bp_SYST1_RF.csv \
	--output_label py_rfr_totpop_diet1

time python lib/scripts/ml/rand_for_reg-sing_col_resp-single_col_divide.py \
	--response_fn output/tables/BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/demo_d1_diet_2015_match_bp_SYST1_RF.csv \
	--output_label py_rfr_totpop_demo_diet_cat

time python lib/scripts/ml/rand_for_reg-sing_col_resp-single_col_divide.py \
	--response_fn output/tables/noRx_BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv \
	--output_label py_rfr_totpop_demo_diet_noRx

time python lib/scripts/ml/rand_for_reg-sing_col_resp-single_col_divide.py \
	--response_fn output/tables/PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/PHTHTE_PAQ_noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv \
	--output_label py_rfr_totpop_demo_diet_noRx_PAQ_PHTHTE

time python lib/scripts/ml/rand_for_reg-sing_col_resp-single_col_divide.py \
	--response_fn output/tables/PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/PHTHTE_PAQ_noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv \
	--output_label py_rfr_totpop_demo_diet_noRx_PAQ_PHTHTE \
	--title "2015_demo+diet-Rx+PAQ+PHYTHE"
"""

# --------------------------------------------------------------------------
print("Defining functions", flush = True)
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
home_dir = options.homedir
project = options.project
output_dir = os.path.join(home_dir, "output")
assert os.path.exists(output_dir)

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------

output_label = options.output_label
result_fpath = os.path.join(output_dir, "tables", f"{output_label}_data.csv")
col_names = ["model", "response_var"]
num_cv_folds = 2
n_trees = 500
bar_shown = 20
col_names = col_names + [f"split{x}" for x in range(num_cv_folds)]
pdf_fpath = os.path.join(output_dir, "graphics", f"bp_{output_label}_.pdf")
sum_pdf_fpath = os.path.join(output_dir, "graphics", f"sum_{output_label}_.pdf")
algo_table_fpath = os.path.join(output_dir, "tables", f"algo_{output_label}_.csv")
id_var = "Respondent sequence number."

response_cols = ["Systolic:  Blood pressure (first reading) mm Hg", "Systolic_Hypertension", "Diastolic:  Blood pressure (first reading) mm Hg", "Diastolic_Hypertension"]#TODO: FIX THIS
response_df = pd.read_csv(os.path.join(home_dir, options.resp_fn), \
		sep=",", header=0)
print(response_df.columns)
response_df = response_df.sort_values(by = id_var)
pred_df = pd.read_csv(os.path.join(home_dir, options.pred_table), \
		sep=",", header=0).fillna(0)
print(pred_df)
pred_df = pred_df.sort_values(by = id_var)
id_list = pred_df.loc[:,id_var]
print("PRED")
print(len(list(response_df.index)))
print(pred_df.loc[:,id_var])

seed = 7

# --------------------------------------------------------------------------
print(f"Selecting models.")
# --------------------------------------------------------------------------
models = []
# models.append(('LogR', LogisticRegression(max_iter=1000)))
# models.append(('LDA', LinearDiscriminantAnalysis()))
# models.append(('KNN', KNeighborsClassifier()))
# models.append(('DTREE', DecisionTreeClassifier()))
models.append(('RF', RandomForestRegressor()))
# models.append(('GausNB', GaussianNB()))
# models.append(('SVM', SVC()))

# --------------------------------------------------------------------------
print(f"Building accuracy scores. Results found at {result_fpath}.")
# --------------------------------------------------------------------------
pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_fpath)
with open(result_fpath, "w+") as fl:
	fl.write(",".join(col_names))
	fl.write("\n")
	print(f"There are {len(response_cols)} response columns, {len(models)} models")
	for meta_c in response_cols:
		m_c = meta_c
		print(m_c)
		for name, model in models:
			print(f"{name}")
			# try:
			my_accuracy = []
			responses = []
			predictions = []
			kfold = model_selection.KFold(n_splits=num_cv_folds, random_state=seed, shuffle=True)
			feature_rows = []
			for train, test in kfold.split(pred_df.loc[:,id_var]):
				print(f"train: {len(train)}, test: {len(test)}")
				train = [id_list[x] for x in train]
				test = [id_list[x] for x in test]
				pred_train = pred_df[pred_df[id_var].isin(train)]#selects whole dataframe
				pred_test = pred_df[pred_df[id_var].isin(test)]
				print(pred_train.shape)
				resp_train = response_df.loc[ response_df[id_var].isin(train) , m_c]
				resp_test = response_df.loc[ response_df[id_var].isin(test) , m_c]
				if is_numeric_dtype(resp_train) == True:
					print("going to RandomForestRegressor()")
					clf = RandomForestRegressor(n_estimators=n_trees)
					resp_pred = clf.predict(pred_test)
					clf.fit(pred_train, resp_train)
					print(clf.feature_importances_[1:10])
					print(clf.feature_names_in_[1:10])
					print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
					feature_rows.append(dict(zip(clf.feature_names_in_, clf.feature_importances_)))
					resp_pred = clf.predict(pred_test)
					# my_score = r2_score(resp_test, resp_pred, sample_weight=None)
					my_score = clf.score(pred_test, resp_test, sample_weight=None)
					my_accuracy.append(my_score)
					# print(my_accuracy)

					# print('Mean Absolute Error (MAE):', metrics.mean_absolute_error(y_true, y_pred))
					# print('Mean Squared Error (MSE):', metrics.mean_squared_error(y_true, y_pred))
					# print('Root Mean Squared Error (RMSE):', metrics.mean_squared_error(y_true, y_pred, squared=False))
					# print('Mean Absolute Percentage Error (MAPE):', metrics.mean_absolute_percentage_error(y_true, y_pred))
					# print('Explained Variance Score:', metrics.explained_variance_score(y_true, y_pred))
					# print('Max Error:', metrics.max_error(y_true, y_pred))
					# print('Mean Squared Log Error:', metrics.mean_squared_log_error(y_true, y_pred))
					# print('Median Absolute Error:', metrics.median_absolute_error(y_true, y_pred))
					# print('R^2:', metrics.r2_score(y_true, y_pred))
					# print('Mean Poisson Deviance:', metrics.mean_poisson_deviance(y_true, y_pred))
					# print('Mean Gamma Deviance:', metrics.mean_gamma_deviance(y_true, y_pred))
				else:
					print("going to RandomForestClassifier()")
					clf = RandomForestClassifier(n_estimators=n_trees)
					clf.fit(pred_train, resp_train)
					print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
					feature_rows.append(dict(zip(clf.feature_names_in_, clf.feature_importances_)))
					resp_pred = clf.predict(pred_test)
					predictions.append(clf.predict(pred_test))
					responses.append(pred_test)
					# my_score = r2_score(resp_test, resp_pred, sample_weight=None)
					my_score = clf.score(pred_test, resp_test, sample_weight=None)
					my_accuracy.append(my_score)

			final_acc = ",".join(map(str, my_accuracy))
			print(final_acc)
			msg = f"{name},{m_c},{final_acc}\n"
			print(msg, flush=True)
			fl.write(msg)
			feature_df = pd.DataFrame(feature_rows)
			feature_df = feature_df.reindex(feature_df.mean().sort_values(ascending=False).index, axis=1)
			feature_df.to_csv(os.path.join(output_dir, "tables", f"feat_imp_{output_label}.csv"))
			feature_mean = feature_df.mean(axis=0)
			feature_mean.to_csv(os.path.join(output_dir, "tables", f"ave_feat_imp_{output_label}.csv"))
			plt.barh(feature_mean.index[0:bar_shown], feature_mean[0:bar_shown])
			plt.xlabel(f"Top {bar_shown} Relative Importances")
			plt.xticks(rotation="vertical")
			plt.title(f"Mean R-squared: {round(np.mean(my_accuracy), 3)}")
			plt.suptitle(f"{options.title}, {meta_c}")
			plt.savefig(os.path.join(output_dir, "graphics", f"{output_label}feat_imp.png"),
			   bbox_inches='tight')

print("Saving pdf", flush = True)
pdf.close()