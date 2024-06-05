# Script for running my rf cardio plots

# pwd

# RF_SLM=$(`rf_cardio*.slurm`)
# # echo ${RF_SLM[*]}

# for ((i=0; i < ${#RF_SLM[@]}; i++)); do

#   # [do something to each element of array]

#   echo "${RF_SLM[$i]}"
# done
sbatch lib/scripts/slurm_scripts/rf_cardio_cat_g.slurm 
sbatch lib/scripts/slurm_scripts/rf_cardio_cat.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_food_g.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_food_simp.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_cat.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_cat_g.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_food_cat_g.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_food_cat_simp.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_food_g.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_food_simp.slurm
sbatch lib/scripts/slurm_scripts/rf_cardio_nut_only.slurm
