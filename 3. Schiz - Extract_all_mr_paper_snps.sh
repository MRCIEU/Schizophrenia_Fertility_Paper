# This script has been adapted from code provided by Kaitlin Wade and Hannah Sallis

#!/bin/bash
#PBS -l walltime=120:00:00
#PBS -o extract_all_mr_paper_snps.txt
#PBS -me
#PBS -l nodes=1:ppn=4
#
#---------------------------------------------
#
cd "${HOME}"
#
#---------------------------------------------

export DIR1="**"
export DIR2="**"
export DIR3="**"

# uses qctool to extract SNPs of interest. Data is written to a .gen file 

for i in 01 02 03 04 05 06 07 08 09 10 11 12 14 15 16 17 18 19 20 22
do
qctool -g $DIR1/data.chr"$i".bgen -og $DIR2/"$i"_snp.dosage.gen -s $DIR3/data.chr01.sample -omit-chromosome -incl-rsids $DIR2/schiz_snps.txt -excl-samples $DIR2/biobank_exclusion_list_unique_ids.txt
done

rm $DIR2/snps-out.gen
touch $DIR2/snps-out.gen

for i in 01 02 03 04 05 06 07 08 09 10 11 12 14 15 16 17 18 19 20 22
do
cat $DIR2/${i}_snp.dosage.gen >> $DIR2/snps-out.gen
done
 
