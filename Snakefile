# Author: Aaron Yerke (aaronyerke@d1_cat_2015gmail.com)
# Main snakemake file for running and re-running scripts

import os

rule create_diets:
    input:
        script = os.path.join(".", "lib","scripts","data_org","import_org_diet.R"),
        food_codes = os.path.join(".","Data", "diet", "WWEIA1516_foodcat_FNDDS.xlsx"),
    output:
        os.path.join(".","Data","diet", "d1_nutr_only_2015.csv"),
        os.path.join(".","Data","diet", "d1_food_g_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_food_g_2015.csv"),
        os.path.join(".","Data","diet", "d1_food_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_food_2015.csv"),
        os.path.join(".","Data","diet", "d1_cat_g_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_cat_g_2015.csv"),
        os.path.join(".","Data","diet", "d1_cat_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_cat_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_food_g_cat_g_2015.csv"),
        os.path.join(".","Data","diet", "d1_nutri_food_cat_2015.csv"),
    shell:
        """
        module load r
        Rscript {input.script}
        """
