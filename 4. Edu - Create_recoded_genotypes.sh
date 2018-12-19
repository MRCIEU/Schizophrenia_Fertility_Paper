# This script has been adapted from code provided by Kaitlin Wade and Hannah Sallis


#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -o recode_snps.txt
#PBS -me
#PBS -l nodes=1:ppn=8
#
#---------------------------------------------
#
cd "${HOME}"
#
#---------------------------------------------

export DIR1="**"
export DIR2="**"


# This scripts creates a file with genotypes coded as 0/1/2 for each of the snps 

plink --gen $DIR2/snps-out.gen --sample $DIR1/sample-stats/data.chr01.sample  --remove $DIR2/biobank_exclusion_list_unique_ids.txt --allow-extra-chr --extract $DIR2/education_snps.txt --recodeA --out $DIR2/snps_recoded


 
