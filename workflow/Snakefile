# Author: Aaron Yerke (aaronyerke@d1_cat_2015gmail.com)
# Main snakemake file for running and re-running scripts
# snakemake --dry-run
configfile: "config.yaml"
# print(config)
import os
import pandas as pd

diet_df = pd.read_csv(os.path.join(".","Data", "diet", "diet_test_rf.csv"), \
		sep=",", header=0)

# test_diets = [os.path.join("Data", "diet", x) for x in expand("{diet}", diet=config["test_diets"])]
test_diets = expand("Data/diet/{diet}", diet=diet_df.loc[:,"Diet_orgs"])
print(test_diets)
rule targets:
    input: os.path.join("output", "diet", "tables", "ave_feat_imp_food_simple.csv")

rule create_diets:
    input:
        script = os.path.join( "lib","scripts","data_org","import_org_diet.R"),
        food_codes = os.path.join("Data", "diet", "WWEIA1516_foodcat_FNDDS.xlsx"),
    output:
        test_diets
    params:
        mode = config["bash_or_slurm"]
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/create_diets.slurm
        """

rule create_resp_var_table:
    input:
        os.path.join("Data","respns_vars", "response_features_tables.csv")
    output:
        os.path.join("Data","respns_vars", "cardio_respns_vars.csv")
    params:
        mode = config["bash_or_slurm"]
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/cardio_resp_vars.slurm
        """

rule rf_cardio_cat_g:
    input:
        resp_var = os.path.join("Data","respns_vars", "cardio_respns_vars.csv"),
        diet_data = os.path.join("Data","diet", "d1_cat_g_2015.csv")
    params: mode = config["bash_or_slurm"]
    output:
        os.path.join("output","diet","tables","cat_grams_data.csv"),
        os.path.join("output", "diet", "tables", "ave_feat_imp_cat_grams.csv")
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/rf_cardio_cat_g.slurm
        """
rule rf_cardio_cat:
    input:
        resp_var = os.path.join("Data","respns_vars", "cardio_respns_vars.csv"),
        diet_data = os.path.join("Data","diet", "d1_cat_2015.csv")
    params: mode = config["bash_or_slurm"]
    output:
        os.path.join("output","diet","tables","cat_simp_data.csv"),
        os.path.join("output", "diet", "tables", "ave_feat_imp_cat_simp.csv")
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/rf_cardio_cat.slurm
        """
rule rf_cardio_food_simp:
    input:
        resp_var = os.path.join("Data","respns_vars", "cardio_respns_vars.csv"),
        diet_data = os.path.join("Data","diet", "d1_food_2015.csv")
    params: mode = config["bash_or_slurm"]
    output:
        os.path.join("output","diet","tables","food_simple_data.csv"),
        os.path.join("output", "diet", "tables", "ave_feat_imp_food_simple.csv")
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/rf_cardio_food_simp.slurm
        """
rule rf_cardio_nut_cat:
    input:
        resp_var = os.path.join("Data","respns_vars", "cardio_respns_vars.csv"),
        diet_data = os.path.join("Data","diet", "d1_nutri_cat_2015.csv")
    params: mode = config["bash_or_slurm"]
    output:
        os.path.join("output","diet","tables","nut_cat_simple_data.csv"),
        os.path.join("output", "diet", "tables", "ave_feat_imp_nut_cat_simple.csv")
    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/rf_cardio_food_simp.slurm
        """
