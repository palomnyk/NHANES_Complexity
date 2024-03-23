

rule create_diets:
    input:
        script = "lib/scripts/data_org/import_org_diet.R"
    output:
        
    shell:
        """
        module load r
        Rscript {input.script}
        """
        