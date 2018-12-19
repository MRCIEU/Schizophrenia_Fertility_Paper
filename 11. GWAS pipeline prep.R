rm(list=ls())

#################################################################################
# GWAS PIPELINE PREP 
#################################################################################

#PHENOTYPE FILE - doesnt matter if take this from schiz or education folder
phenotype <- read.csv(file="schizRep_finaldata.csv", header=T) 
phenotypefinal <- phenotype[c("FID", "IID", "NEB", "age_fbirth_multi")]

#Covariate file with all
covariates <- read.table(file="data.covariates.plink.txt", header=T)
age <- phenotype[c("FID", "IID", "age_centre")]
covariates <- merge(covariates, age, by=c("FID", "IID"), all=T)

#Pipeline needs all as numeric
covariates$age_centre <- as.numeric(covariates$age_centre)
phenotypefinal$NEB <- as.numeric(phenotypefinal$NEB)
phenotypefinal$age_fbirth_multi <- as.numeric(phenotypefinal$age_fbirth_multi)

#alternative covariates files
covariates_nochip <- subset(covariates, select=-c(chip))
covariates_noage <- subset(covariates, select=-c(age_centre))
covariates_nochipage <- subset(covariates, select=-c(age_centre, chip))

#Export as space delimated files
write.table(phenotypefinal, "phenotype.txt", sep=" ", row.names=F, quote=F)
write.table(covariates, "covariates.txt", sep=" ", row.names=F, quote=F)
write.table(covariates_nochip, "covariates_nochip.txt", sep=" ", row.names=F, quote=F)
write.table(covariates_noage, "covariates_noage.txt", sep=" ", row.names=F, quote=F)
write.table(covariates_nochipage, "covariates_nochipage.txt", sep=" ", row.names=F, quote=F)

# this was then submitted to the MRC IEU GWAS pipeline

