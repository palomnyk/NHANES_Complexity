#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# This is a script for random forest for NHANES study
# Should take a table with predictor/explanatory/independant and a table
# with response/outcome/dependant variable.
# Returns accuracy and other metrics and feature importance.
# Interesting resources for SHAP values:
# 	1: https://shap.readthedocs.io/en/latest/example_notebooks/tabular_examples/tree_based_models/NHANES%20I%20Survival%20Model.html
# 		-Uses NHANES data for example
# --------------------------------------------------------------------------
print("Loading external libraries.",flush = True)
# --------------------------------------------------------------------------
from cProfile import label
import math
import os, sys
import numpy as np
import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.api.types import is_string_dtype
from pandas.api.types import is_bool_dtype
from pandas.api.types import is_categorical_dtype
from sklearn.metrics import accuracy_score, roc_auc_score, r2_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import ConfusionMatrixDisplay
import matplotlib.pyplot as plt
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})
import matplotlib.backends.backend_pdf
import matplotlib.colors as mcolors
import matplotlib
# matplotlib.use('TKAgg')
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
from scipy import stats
import argparse
import random
import pathlib
import pdb
import shap
# import plotnine

# QT_DEBUG_PLUGINS=1
# --------------------------------------------------------------------------
print("Reading commmandline input with optparse.", flush = True)
# --------------------------------------------------------------------------

parser = argparse.ArgumentParser(description="This script runs a random forest test on various datasets.")
parser.add_argument("-e", "--pred_path", dest="pred_table", 
				  default = "Data/unit_test/mtc_predictor.csv",
                  help="path, relative to cwd, to table with predictor/explanatory/independant vars",
				  metavar="pred_table")
parser.add_argument("-o", "--output_label", default="py_rf",
				  dest="output_label",
                  help="base label for output files (additional info will be added to it)")
parser.add_argument("-c", "--response_col", default=False,
                  help="Feature column to use in response var, if empty, all will be used")
parser.add_argument("-f", "--out_folder", default="unit_test",
                  help="path, sub folder in 'output'.", 
				  metavar="out_sub")
parser.add_argument("-r", "--response_fn", default="Data/unit_test/mtc_response.csv",
				  dest="resp_fn",
                  help="Name of file that holds response vars.", metavar="resp_fn")
parser.add_argument("-l", "--delimiter", default="\t",
                  help="File delimiting symbol for metadata. Default is tab.",
				  metavar="delim", dest="delim")
parser.add_argument("-t", "--title", default=False,
                  help="Title for visualizations",
                  metavar="title", dest="title")
parser.add_argument("-v", "--id_var", default="Respondent sequence number",
                  help="String, column name of row variables",
                  metavar="id_var", dest="id_var")

options, unknown = parser.parse_known_args()

print(options)

"""
python lib/scripts/ml/rf_resp_df.py \
	--response_fn Data/resp_vars/cardio_respns_vars.csv \
	--delimeter , \
	--pred_path Data/diet/d1_nutri_food_g_2015.csv \
	--out_folder diet_test \
	--output_label nutr_food_grams \
	--title nutri_food_grams
"""

# --------------------------------------------------------------------------
print("Defining functions", flush = True)
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
output_dir = os.path.join(".", "output", options.out_folder)

pathlib.Path(os.path.join(".",output_dir, "graphics")).mkdir(parents=True, exist_ok=True) 
pathlib.Path(os.path.join(".",output_dir, "tables")).mkdir(parents=True, exist_ok=True) 
assert os.path.exists(output_dir)

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------

output_label = options.output_label
col_names = ["model", "response_var"]
num_cv_folds = 10
n_trees = 1000
bar_shown = 25
col_names = col_names + [f"split{x}" for x in range(num_cv_folds)]
if options.title == False:
	options.title == options.output_label

response_df = pd.read_csv(os.path.join(".",options.resp_fn), \
		sep=",", header=0).replace("TRUE", True).replace("FALSE", False)
# print(response_df.columns)

#set labels for output files and select column to use in response var
if options.response_col == False:
	response_cols = response_df.columns
	resp_col_label = ""
else:
	response_cols = [options.response_col]
	resp_col_label = f"1col{options.response_col}?"
response_df = response_df.sort_values(by = options.id_var)
# print(options.id_var)
pred_df = pd.read_csv(os.path.join(".",options.pred_table), \
		sep=",", header=0, index_col=options.id_var).fillna(0)
# print(pred_df)
pred_df = pred_df.sort_values(by = options.id_var)
id_list = response_df.loc[:,options.id_var]

#output files
output_label = f"{resp_col_label}{options.output_label}"
output_label = output_label.replace("/", "")
result_fpath = os.path.join(output_dir, "tables", f"{output_label}_data.csv")
pdf_fpath = os.path.join(output_dir, "graphics", f"{output_label}_feat_import.pdf")
sum_pdf_fpath = os.path.join(output_dir, "graphics", f"sum_{output_label}.pdf")
algo_table_fpath = os.path.join(output_dir, "tables", f"algo_{output_label}.csv")

seed = 7

# --------------------------------------------------------------------------
print(f"Building accuracy scores. Results found at {result_fpath}.")
# --------------------------------------------------------------------------
pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_fpath)
with open(result_fpath, "w+") as fl:
	fl.write(",".join(col_names))
	fl.write("\n")
	print(f"There are {len(response_cols)} response columns")
	ave_feature_importance = []
	ave_feature_response = []
	for resp_var in response_cols:
		resp_safe_ids = response_df.loc[response_df[resp_var ].notna(), options.id_var]
		intersect_safe_ids = list(set(resp_safe_ids) & set(pred_df.index))
		print(f"num safe ids {len(resp_safe_ids)}")
		# print(resp_safe_ids)
		my_accuracy = []
		responses = []
		predictions = []
		kfold = model_selection.KFold(n_splits=num_cv_folds, random_state=seed, shuffle=True)
		feature_rows = []
		for train, test in kfold.split(intersect_safe_ids):
			print(f"train: {len(train)}, test: {len(test)}")
			train = [intersect_safe_ids[x] for x in train]
			test = [intersect_safe_ids[x] for x in test]
			pred_train = pred_df.loc[pred_df.index.isin(train),:]#selects whole dataframe
			pred_test = pred_df.loc[pred_df.index.isin(test),:]
			# print(pred_train.shape)
			resp_train = response_df.loc[ response_df[options.id_var].isin(train).tolist() , resp_var ].convert_dtypes()
			resp_test = response_df.loc[ response_df[options.id_var].isin(test).tolist() , resp_var ].convert_dtypes()
			print(f"len resp_train {len(resp_train)}, {resp_train.dtype}")
			if is_numeric_dtype(resp_train) and resp_train.dtype.name != "boolean":
				print(f"going to RandomForestRegressor(), {resp_var }")
				clf = RandomForestRegressor(n_estimators=n_trees)
				clf.fit(pred_train, resp_train)
				print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
				feature_rows.append(dict(zip(clf.feature_names_in_, clf.feature_importances_)))
				modl_predict = clf.predict(pred_test)
				# my_score = r2_score(resp_test, modl_predict, sample_weight=None)
				my_score = clf.score(pred_test, resp_test, sample_weight=None)
				my_accuracy.append(my_score)
				# print(my_accuracy)
			else:
				print("going to RandomForestClassifier()")
				clf = RandomForestClassifier(n_estimators=n_trees)
				# print(set(resp_train))
				clf.fit(pred_train, resp_train)
				print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
				feature_rows.append(dict(zip(clf.feature_names_in_, clf.feature_importances_)))
				modl_predict = clf.predict(pred_test)
				predictions.extend(modl_predict)
				responses.extend(resp_test)
				my_score = clf.score(pred_test, resp_test, sample_weight=None)
				my_accuracy.append(my_score)
			final_acc = ",".join(map(str, my_accuracy))
			# print(final_acc)
			msg = f"RF,{resp_var },{final_acc}\n"
			print(msg, flush=True)
			fl.write(msg)
			feature_df = pd.DataFrame(feature_rows)
			# Order the features by importance
			feature_df = feature_df.reindex(feature_df.mean().sort_values(ascending=False).index, axis=1)
			feature_df.to_csv(os.path.join(output_dir, "tables", f"feat_imp_{output_label}.csv"))
			feature_mean = feature_df.mean(axis=0)
			print("feature mean")
			print(feature_mean)
			feature_std = feature_df.std(axis=0)
			ave_feature_importance.append(feature_mean)
			ave_feature_response.append(resp_var )

			try:
				# print(c_statistic_harrell(modl_predict.tolist(), resp_test.tolist()))
				shap_values = shap.TreeExplainer(clf).shap_values(pred_df)
				print(shap_values)
				shap.summary_plot(shap_values, pred_df)
				# plt.title(f"{resp_var}")
				plt.suptitle(f"SHAP Summary, {resp_var}")
				pdf.savefig()
				plt.close()
			except Exception as e:
				print(f"Exception: shap summary {resp_var}")
				print(e)
			try:
				shap_values = shap.TreeExplainer(clf).shap_values(pred_df)
				shap.decision_plot(shap.TreeExplainer(clf).expected_value, shap_values, pred_train)
				plt.suptitle(f"SHAP decision, {resp_var}")
				pdf.savefig()
				plt.close()
			except Exception as e:
				print(f"Exception: shap decision {resp_var}")
				print(e)
			
			if not is_numeric_dtype(resp_train) or resp_train.dtype.name == "boolean":
				plt.barh(y=feature_mean.index[0:bar_shown], width=feature_mean[0:bar_shown])
				plt.xlabel(f"Top {bar_shown} Relative Importances")
				plt.xticks(rotation="vertical")
				plt.title(f"Mean accuracy: {round(np.mean(my_accuracy), 3)}")
				plt.suptitle(f"Feature importance: {resp_var}")
				pdf.savefig(bbox_inches='tight')
				plt.close()
				print("making confusion matrix")
				cnf_matrix = confusion_matrix(responses, predictions)
				# print(str(cnf_matrix))
				disp = ConfusionMatrixDisplay(confusion_matrix=cnf_matrix).plot()
				plt.title(f"{options.title}, {resp_var}")
				# pdf.savefig(bbox_inches='tight')
				pdf.savefig()
				plt.close()

			else:
				print("else")
				plt.barh(feature_mean.index[0:bar_shown], feature_mean[0:bar_shown])
				plt.xlabel(f"Top {bar_shown} Relative Importances")
				plt.xticks(rotation="vertical")
				plt.title(f"Mean R-squared: {round(np.mean(my_accuracy), 3)}")
				plt.suptitle(f"Feature importance: {resp_var}")
				# pdf.savefig(bbox_inches='tight')
				pdf.savefig()
				plt.clf()
	print(f"Saving average feature importance")
	ave_feature_importance = pd.DataFrame(ave_feature_importance)
	ave_feature_importance = ave_feature_importance.reindex(ave_feature_importance.mean().sort_values(ascending=False).index, axis=1)
	# ave_feature_importance = ave_feature_importance.transpose()
	ave_feature_importance.to_csv(os.path.join(output_dir, "tables", f"ave_feat_imp_{output_label}.csv"), index=False)
print("Saving pdf", flush = True)
pdf.close()

print(f"Completed {__file__}", flush = True)
