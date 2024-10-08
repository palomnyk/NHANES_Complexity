#!/bin/bash

##SBATCH --partition=Pisces                             # Partition/queue requested on server
#SBATCH --job-name=Create_multiyear_Diets                         # Job name
#SBATCH --time=010:00:00                                 # Time limit (hrs:min:sec)
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
source activate python_ml_conda
python --version
Rscript ./lib/scripts/data_org/import_org_multi_year_diet.R

echo ""
echo "======================================================"
echo "End Time   : $(date)"
echo "======================================================"
