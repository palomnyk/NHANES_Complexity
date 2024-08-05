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
`snakemake --dag |dot -Tpdf > workflow/reports/dag_test.pdf`

`snakemake --software-deployment-method conda --cores 1 rf_cardio_cat_g`

conda env update --file workflow/config/config.yaml

### Getting V8+ of snakemake was challenging due to conflicting documentation.
#### Useful links:
#### https://stackoverflow.com/questions/77929511/how-to-run-snakemake-8-on-a-slurm-cluster
`pip install snakemake-executor-plugin-cluster-generic`
Run snakemake with this command:

snakemake --profile workflow/config/
