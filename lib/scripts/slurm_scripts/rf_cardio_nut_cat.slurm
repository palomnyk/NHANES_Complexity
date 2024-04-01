#!/bin/bash

##SBATCH --partition=Pisces                             # Partition/queue requested on server
#SBATCH --job-name=rf_nut_cat_simple                        # Job name
#SBATCH --time=24:00:00                                 # Time limit (hrs:min:sec)
#SBATCH --nodes=1                                       # Number of nodes requested
#SBATCH --ntasks-per-node=1                          		# Number of CPUs (processor cores/tasks)
#SBATCH --mem=50gb                                      # Memory limit
#SBATCH --mail-type=BEGIN,END,FAIL                      # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=aaron.yerke@usda.gov                # Specified email address
#SBATCH --output=./slurmLogs/%x.%j.log                  # Set directory for standard output
#SBATCH --error=./slurmLogs/%x.%j.log                   # Set directory for error log
#SBATCH --get-user-env

### Display the job context
echo Job: $SLURM_JOB_NAME with ID $SLURM_JOB_ID
echo Running on host: `hostname`
echo Using $SLURM_NTASKS processors across $SLURM_NNODES nodes

module purge  # clear module environment
module load miniconda  # load miniconda
conda init bash
source activate python_ml_conda
python lib/scripts/ml/rf-resp_df.py \
	--response_fn Data/respns_vars/cardio_respns_vars.csv \
	--delimeter , \
	--pred_path Data/diet/d1d2_nutri_cat_2015.csv \
	--out_folder diet_test \
	--output_label nut_cat_simple \
	--title nut_cat_simple

echo ""
echo "======================================================"
echo "End Time   : $(date)"
echo "======================================================"
