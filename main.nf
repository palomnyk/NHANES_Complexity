#! /usr/bin/env nextflow

params.str = 'CAN I GET THIS!!!'

test = "gkjhgjgkj"

process create_diets {
    conda 'r-ggplot2'
    input:
    // path x
    // val my_val = params.str
    output:
    stdout
    script:
    """"

    echo $PWD
    module load r
    Rscript $PWD/lib/scripts/data_org/import_org_diet.R

    """
}
    // Rscript nutrition_data/p1_build_ESHA_csv.R
// process convertToUpper {
//    input:
//    path x
//    output:
//    stdout
//    """
//    cat $x | tr '[a-z]' '[A-Z]'
//    """
// }

println "I will learn THIS $test $params.p1_path!"

workflow {
    create_diets()
}


println "do we get here?"

