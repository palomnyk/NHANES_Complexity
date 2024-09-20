# NHANES_Complexity

##Developer notes
### Data aquisition
FNDDS for food codes can be found here: https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/dmr-food-categories/

https://www.ars.usda.gov/ARSUserFiles/80400530/apps/WWEIA1720_foodcat_FNDDS.xlsx

downloaded the mortalilty data from here:
curl -o NHANES_2015_2016_MORT_2019_PUBLIC.dat https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_2015_2016_MORT_2019_PUBLIC.dat

https://www.ars.usda.gov/ARSUserFiles/80400530/apps/WWEIA2017_March2020_foodcat_FNDDS.xlsx

https://www.ars.usda.gov/ARSUserFiles/80400530/apps/WWEIA1516_foodcat_FNDDS.xlsx

### Useful commands
#### Linux
##### Remove files older than 9 days
`find . -mtime +9 -type f -delete`
##### Create a symlink for ~/.local/share/code-server that goes to /project/nhanes_ml/code-server
`ln -s ~/.local/share/code-server .`

#### Snakemake

##### Main script for running snakemake
snakemake --profile workflow/config

`snakemake --dry-run`
##### Drawing diagram of workflow
`snakemake --dag |dot -Tpdf > workflow/reports/dag_2015.pdf`
snakemake --forceall --dag |dot -Tpdf > workflow/reports/dag_2015.pdf

`snakemake --software-deployment-method conda --cores 1 rf_cardio_cat_g`

conda env update --file workflow/config/config.yaml

### Getting V8+ of snakemake was challenging due to conflicting documentation.
#### Useful links:
#### https://stackoverflow.com/questions/77929511/how-to-run-snakemake-8-on-a-slurm-cluster
`pip install snakemake-executor-plugin-cluster-generic`
Run snakemake with this command:

snakemake --profile workflow/config/


old commands:
    Rscript lib/scripts/descriptive/feature_correlations.R \
      --out_subdir diet_test_d1d2 \
      --pred_col "Energy (kcal" \
      --resp_df Data/exam/bodyweight_2009-2020.csv

    Rscript lib/scripts/descriptive/feature_correlations.R \
      --out_subdir diet_test_d1d2 \
      --pred_col "Caffeine (mg"

    Rscript lib/scripts/descriptive/feature_correlations.R \
      --out_subdir diet_test_d1d2 \
      --pred_col "Total sugars (gm" \
      --resp_df Data/exam/bodyweight_2009-2020.csv

Rscript lib/scripts/descriptive/feature_correlations.R \
      --out_subdir diet_test_d1d2 \
      --pred_col "Total fat (gm" \
      --resp_df Data/exam/bodyweight_2009-2020.csv

Rscript lib/scripts/descriptive/feature_correlations.R \
    --out_subdir feature_correlations \
    --pred_col "Total sugars (gm" \
    --resp_df Data/exam/bodyweight_2009-2020.csv \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_correlations.R \
    --out_subdir feature_correlations \
    --pred_col "Energy (kcal" \
    --resp_df Data/exam/bodyweight_2009-2020.csv \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_correlations.R \
    --out_subdir feature_correlations \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_correlations.R \
    --out_subdir feature_correlations \
    --pred_col "Caffeine (mg" \
    --diet_df Data/diet/d1_nutr_only_2015.csv


Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Energy (kcal" \
    --resp_df Data/exam/bodyweight_2009-2020.csv \
    --diet_df Data/diet/d1_nutr_only_2015.csv \
    --outlier_percent 0.03

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Energy (kcal" \
    --resp_df Data/exam/bodyweight_2009-2020.csv \
    --diet_df Data/diet/d1d2_nutr_only_2015.csv \
    --outlier_percent 0.03

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Energy (kcal" \
    --resp_df Data/exam/bodyweight_2009-2020.csv \
    --diet_df Data/diet/multi_year/d1_nutr_only_2009-2020.csv

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Caffeine (mg" \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_corr_points_hypertensive_only.R \
    --out_subdir feature_correlations \
    --pred_col "Caffeine (mg" \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_corr_points_hypertensive_only.R \
    --out_subdir feature_correlations \
    --diet_df Data/diet/d1_nutr_only_2015.csv

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Atwater_gen_energy" \
    --resp_df Data/diet/d1_nutr_only_2015.csv \
    --diet_df Data/diet/d1_nutr_only_2015.csv \
    --outlier_percent 0.03

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "Systolic_mean" \
    --resp_df Data/respns_vars/2009-2020cardio_respns_vars.csv\
    --diet_df Data/respns_vars/2009-2020cardio_respns_vars.csv\
    --outlier_percent 0.01

Rscript lib/scripts/descriptive/feature_corr_points.R \
    --out_subdir feature_correlations \
    --pred_col "LDL-cholesterol (mg/dL" \
    --resp_df Data/respns_vars/2009-2020cardio_respns_vars.csv \
    --diet_df Data/respns_vars/2009-2020cardio_respns_vars.csv \
    --outlier_percent 0.01


python lib/scripts/ml/rf-resp_df.py \
        --response_fn Data/respns_vars/2009-2020cardio_respns_vars.csv \
        --delimeter , \
        --pred_path Data/diet/d1_nutr_only_2015_old.csv  \
        --out_folder feature_correlations \
        --output_label "Atwater_gen_energy" \
        --title "Atwater_gen_energy" \
        --response_col "Atwater_gen_energy"

python lib/scripts/ml/rf-resp_df.py \
        --response_fn Data/respns_vars/2009-2020cardio_respns_vars.csv \
        --delimeter , \
        --pred_path Data/respns_vars/2009-2020cardio_respns_vars_no_diastolic.csv \
        --out_folder feature_correlations \
        --output_label "Diastolic_mean" \
        --title "Diastolic_mean" \
        --response_col "Diastolic_mean"

python lib/scripts/ml/rf-resp_df.py \
        --response_fn Data/respns_vars/2009-2020cardio_respns_vars.csv \
        --delimeter , \
        --pred_path Data/respns_vars/2009-2020cardio_respns_vars_noLDL.csv \
        --out_folder feature_correlations \
        --output_label "LDL-cholesterol (mg/dL" \
        --title "LDL-cholesterol (mg/dL" \
        --response_col "LDL-cholesterol (mg/dL"


conda config --add pkgs_dirs ../conda_from_home/pkgs
conda config --add envs_dirs ../conda_from_home/envs

/project/nhanes_ml/conda_from_home/envs/python_ml_conda/bin/python3.11