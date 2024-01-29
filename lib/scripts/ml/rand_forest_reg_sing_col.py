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
import matplotlib.backends.backend_pdf
import matplotlib.colors as mcolors
from sklearn import model_selection
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
import argparse
import random
import pdb


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
parser.add_argument("-t", "--training", default=0.9,
                  help="Percentating of table to use for training. The rest will be used for testing.",
                  metavar="training", dest="training")

options, unknown = parser.parse_known_args()

print(options)

"""
python lib/scripts/ml/random_forest.py \
	--response_fn output/tables/BP_2015_match_diet_SYST1_RF.csv \
	--delimeter , \
	--pred_path output/tables/diet_2015_match_bp_SYST1_RF.csv
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

output_label = "nhanes_total_pop"
result_fpath = os.path.join(output_dir, "tables", f"{output_label}__data.csv")
col_names = ["model", "response_var"]
num_cv_folds = 20
col_names = col_names + [f"split{x}" for x in range(num_cv_folds)]
pdf_fpath = os.path.join(output_dir, "graphics", f"bp_{output_label}_.pdf")
sum_pdf_fpath = os.path.join(output_dir, "graphics", f"sum_{output_label}_.pdf")
algo_table_fpath = os.path.join(output_dir, "tables", f"algo_{output_label}_.csv")
id_var = "Respondent sequence number."

response_column = "Systolic:  Blood pressure (first reading) mm Hg"
response_cols = [response_column]#TODO: FIX THIS
response_df = pd.read_csv(os.path.join(home_dir, options.resp_fn), \
		sep=",", header=0)
print(response_df.columns)
response_df = response_df.sort_values(by = id_var)
pred_df = pd.read_csv(os.path.join(home_dir, options.pred_table), \
		sep=",", header=0)
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
			results = []
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
					clf = RandomForestRegressor()
					clf.fit(pred_train, resp_train)
					print(clf.feature_importances_[1:10])
					print(clf.feature_names_in_[1:10])
					print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
					feature_rows.append(dict(zip(clf.feature_names_in_, clf.feature_importances_)))
					resp_pred = clf.predict(pred_test)
					my_score = r2_score(resp_test, resp_pred, sample_weight=None)
# 				else:
# 					clf = RandomForestClassifier()
# 					clf.fit(pred_train, resp_train)
# 					resp_pred = clf.predict(pred_test)
# 					my_score = clf.score(pred_test, resp_test, sample_weight=None)
				my_accuracy.append(my_score)
				# print(my_accuracy)
			final_acc = ",".join(map(str, my_accuracy))
			print(final_acc)
			msg = f"{name},{m_c},{final_acc}\n"
			print(msg)
			fl.write(msg)
			feature_df = pd.DataFrame(feature_rows)
			# feature_df.sort_values(by=feature_df.rank(axis=1, method="average"),
			# 						   ascending=False)
			feature_df = feature_df.reindex(feature_df.mean().sort_values(ascending=False).index, axis=1)
