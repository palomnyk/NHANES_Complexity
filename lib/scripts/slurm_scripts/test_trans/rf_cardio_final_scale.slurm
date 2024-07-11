#!/bin/bash

#SBATCH --partition=long                            	  # Partition/queue requested on server
#SBATCH --job-name=rf_test_all_combine_SCALE        # Job name
#SBATCH --time=240:00:00                                # Time limit (hrs:min:sec)
#SBATCH --nodes=1                                       # Number of nodes requested
#SBATCH --ntasks-per-node=1                          	  # Number of CPUs (processor cores/tasks)
#SBATCH --mem=50gb                                      # Memory limit
#SBATCH --mail-type=BEGIN,END,FAIL                      # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=aaron.yerke@usda.gov                # Specified email address
#SBATCH --output=./slurmLogs/%x.%j.log                  # Set directory for standard output
#SBATCH --error=./slurmLogs/%x.%j.log                   # Set directory for error log

### Display the job context
echo Job: $SLURM_JOB_NAME with ID $SLURM_JOB_ID
echo Running on host: `hostname`
echo Using $SLURM_NTASKS processors across $SLURM_NNODES nodes

module purge  # clear module environment
module load miniconda  # load miniconda
source activate python_ml_conda
python --version
python lib/scripts/ml/rf-resp_df.py \
	--response_fn Data/respns_vars/cardio_respns_vars.csv \
	--delimeter , \
	--pred_path Data/diet_supp_combined_NHANES_SCALE.csv \
	--out_folder final_all_trans \
	--output_label all_combined_d1d2_SCALE \
	--title all_combined_SCALE

echo ""
echo "======================================================"
echo "End Time   : $(date)"
echo "======================================================"
