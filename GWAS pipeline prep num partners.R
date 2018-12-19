rm(list=ls())

nsp <- read.csv("nsp_pooled.csv")
install.packages("reshape") # this may ask you what cran mirror
library("reshape")
linker<-read.csv("**.csv", header=T)
nsp$FID <- linker$app_id8786[match(nsp$id, linker$app_id6326)]
nsp$IID <- nsp$FID
phenotypefinal <- nsp
phenotypefinal <- phenotypefinal[c("FID", "IID", "num_partners", "ten_nsp")]

#################################################################################
# GWAS PIPELINE PREP 
#################################################################################

#Covariate file with all
covariates <- read.table(file="data.covariates.plink.txt", header=T)
age <- phenotype[c("FID", "IID", "age_centre")]
covariates <- merge(covariates, age, by=c("FID", "IID"), all=T)

#Pipeline needs all as numeric
covariates$age_centre <- as.numeric(covariates$age_centre)
phenotypefinal$num_partners <- as.numeric(phenotypefinal$num_partners)
phenotypefinal$ten_nsp <- as.numeric(phenotypefinal$ten_nsp)
phenotypefinal$ten_nsp <- phenotypefinal$ten_nsp +1 # for plink


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
