
#################################################################################
# To be done in Putty before open R
#################################################################################
# Combine all IEU quality check exclusion lists into one (genetic ethnicity, sex mismatches, relatedness, imputed snp and QC)
# Based on DOI: 10.5523/bris.3074krb6t2frj29yh2b03x3wxj exclusions 
# Code provided by Hannah Sallis

# standard exclusions and ancestry
cat data.combined_recommended.txt data.non_white_british.txt > biobank_exclusion_list.txt
wc -l  biobank_exclusion_list.txt
# relatedness
cat data.minimal_relateds.txt >> biobank_exclusion_list.txt
cat data.highly_relateds.txt >> biobank_exclusion_list.txt
wc -l  biobank_exclusion_list.txt

sort biobank_exclusion_list.txt | uniq -cd | wc -l

# To create a file with a single entry per individual
sort biobank_exclusion_list.txt | uniq > biobank_exclusion_list_unique_ids.txt
wc -l biobank_exclusion_list_unique_ids.txt

#################################################################################
# Re-coding SNPs to match exposure GWAS effect allele
#################################################################################
# "create_recoded_snps" script not always counting the effect allele
# I copied the column titles from the .raw file into exel and transposing, to compare the counted allele and effect allele using the IF command
# If I got a 2 for the if command - they did not match
# Note: this is very hard coded for column numbers

## Clear R
rm(list=ls())

# Convert the output of the "create_recoded_snps" shell script (.raw file) into txt and read in
replicationdata <- read.table("snps_recoded.txt", header=T)

# Read in excel file where you have identified mismatches as 2 in column 6
mismatched <- read.csv("mismatched.csv", header=T)

# because I had separated the SNP name and its counted allele into separate cols for IF command, this puts them back together to match the .raw/.txt file again
mismatched$snpstomatch<- paste(mismatched[,1], "_", mismatched[,2], sep = "") 
mismatched2<-mismatched[which(mismatched[,6]==2),]

# Have a look at which snps were mismatches
head(replicationdata[,which(colnames(replicationdata)%in%mismatched2[,9])]) 
matched<- 2-replicationdata[,which(colnames(replicationdata)%in%mismatched2[,9])] 

# replace replication data mismatched columns with new reverse coded ones
replicationdata2 <- subset(replicationdata[,-which(colnames(replicationdata)%in%mismatched2[,9])]) 
replicationdata2 <- lapply(replicationdata2[,1:50], as.numeric) 

# stick matched and replication2 (the subsetted replicationdata without the mismatched SNPS) back together
replicationgenetic <- data.frame(replicationdata2, matched)

# write this to file if you don't want to run it again in future
write.csv(replicationgenetic, file="replicationgenetic.csv", row.names=F, quote=F)

# read it back in if you have created it previously
replicationgenetic <- read.csv("replicationgenetic.csv", header=T)

#################################################################################
# Load in observational data 
#################################################################################
##Reading in observational and calling it "fam"
fam<-read.csv("schiz_replication_neb_pooled_cleaned.csv", header=T)

#################################################################################
# Linking biobank pheno/genotype data IDs using code adapted from Hannah Sallis
#################################################################################

# Take your phenotype id n_eid and rename it id 
install.packages("reshape") # this may ask you what cran mirror
library("reshape")
fam<-rename(fam, c(n_eid="id"))

# Read in linker file so R can match the IDs across two Biobank applications
linker<-read.csv("**.csv", header=T)
fam$FID <- linker$app_id8786[match(fam$id, linker$app_id6326)]
fam$IID <- fam$FID

# PID and MID are set to zero for each individual in the dataset (because there are no trios/duos in the data)
fam$PID <- rep(0, 502604)
fam$MID <- rep(0, 502604)

#################################################################################
# Load additional genetic vars
#################################################################################

# Create dataset with all the necessary variables for analysis and write to file 
famfinal <- fam[c("id", "IID", "FID", "PID", "MID", "sex", "kids_fathered", "split", "schizophrenia", "age_fbirth_multi", "num_kids", "childless", "z_NEB", "NEB", "age_centre")]

# CHIP 
chip <- read.table ("**.txt", colClasses = c(rep("character", 2), rep("NULL", 1), rep("character", 1)))
library(reshape)
chip<-rename(chip, c(V1="IID", V2="FID", V4="chip"))
famfinal <- merge(famfinal, chip, by=c("FID", "IID"), famfinal=T)
famfinal$chip[famfinal$chip=="UKBB"] <- 0
famfinal$chip[famfinal$chip=="UKBL"] <- 1
famfinal$chip <- as.numeric(as.character(famfinal$chip))

# Principal components
PC <- read.table ("**.txt", colClasses = c(rep("character", 12)))
library(reshape)
PC<-rename(PC, c(V1="IID", V2="FID", V3="PC1", V4="PC2", V5="PC3", V6="PC4", V7="PC5", V8="PC6", V9="PC7", V10="PC8", V11="PC9", V12="PC10"))
PC$FID <- as.integer(as.character(PC$FID))
PC$IID <- as.integer(as.character(PC$IID))
PC$PC1 <- as.numeric(as.character(PC$PC1))
PC$PC2 <- as.numeric(as.character(PC$PC2))
PC$PC3 <- as.numeric(as.character(PC$PC3))
PC$PC4 <- as.numeric(as.character(PC$PC4))
PC$PC5 <- as.numeric(as.character(PC$PC5))
PC$PC6 <- as.numeric(as.character(PC$PC6))
PC$PC7 <- as.numeric(as.character(PC$PC7))
PC$PC8 <- as.numeric(as.character(PC$PC8))
PC$PC9 <- as.numeric(as.character(PC$PC9))
PC$PC10 <- as.numeric(as.character(PC$PC10))
famfinal <- merge(famfinal, PC, by=c("FID", "IID"), famfinal=T)

#################################################################################
# Exclusions
#################################################################################

# Creating exclusion object - using list created above
excluded<- read.table("**.txt", col.names = c("FID", "IID"))
excluded$dummy <- 1

# Merging with famfinal and excluding overlaps using a dummy variable
famfinal2<-merge(famfinal, excluded, by.famfinal="FID", all=T) 
famfinal2[c("dummy")][is.na(famfinal2[c("dummy")])] <- 0 # giving all the now missing dummy obs a 0
famfinalEX <- subset(famfinal2, famfinal2$dummy==0) # excluding all the dummy 1

## excluding schizophrenia cases for sensitivity anlaysis
#famfinalEX <- subset(famfinalEX, famfinalEX$schizophrenia==0)

#################################################################################
# Merging genetic and observational
#################################################################################

# merge observational and genetic
finaldata_noschiz <- merge(famfinalEX, replicationgenetic, by.famfinalEX=c("FID", "IID"), famfinalEX=T)
write.csv(finaldata_noschiz, file="schizRep_finaldata.csv", row.names=F, quote=F)
#write.csv(finaldata_noschiz, file="schizRep_finaldata_noschiz.csv", row.names=F, quote=F)






















