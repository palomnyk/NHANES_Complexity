# Author: Aaron Yerke (aaronyerke@d1_cat_2015gmail.com)
# Main snakemake file for running and re-running scripts
configfile: "config.yaml"
# print(config)
import os

# test_diets = [os.path.join("Data", "diet", x) for x in expand("{diet}", diet=config["test_diets"])]
test_diets = expand("Data/diet/{diet}", diet=config["test_diets"])
# print([os.path.join("Data", "diet", x) for x in test_diets])

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

rule test_diets_rf:
    input:
        script = os.path.join( "lib","scripts","data_org","import_org_diet.R"),
        test_diets = configfile["test_diets"]
    output:

    shell:
        """
        {params.mode} lib/scripts/slurm_scripts/create_diets.slurm
        """