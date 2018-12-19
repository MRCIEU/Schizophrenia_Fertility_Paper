#################################################################################
# Making genetic risk score for non-linear analysis - Schizophrenia
#################################################################################

# flip the SNPs effect allele to always be absolute/increasing
flipSNP <- data.frame(finaldata$rs1009080_G, finaldata$rs10504857_G, finaldata$rs10803138_A, finaldata$rs10900851_C, finaldata$rs10933068_A, finaldata$rs1106568_G, finaldata$rs11210892_G, finaldata$rs1160682_C, finaldata$rs11682175_C, finaldata$rs11683083_G, finaldata$rs12325245_T, finaldata$rs12421382_T, finaldata$rs12654855_G,
                      finaldata$rs1501357_C, finaldata$rs17049247_G, finaldata$rs17149781_G, finaldata$rs2007044_G, finaldata$rs2053079_G, finaldata$rs2057070_C, finaldata$rs2068012_C, finaldata$rs2296569_G, finaldata$rs2514218_T, finaldata$rs2693698_A, finaldata$rs2851447_G, finaldata$rs2973161_A, finaldata$rs324015_T, finaldata$rs4240748_C,
                      finaldata$rs4391122_A, finaldata$rs4702_G, finaldata$rs6065094_A, finaldata$rs6579959_T, finaldata$rs6704768_G, finaldata$rs715170_T, finaldata$rs7267348_C, finaldata$rs7432375_A, finaldata$rs7730110_C, finaldata$rs7801375_A, finaldata$rs787983_G, finaldata$rs7927176_G, finaldata$rs832187_C, finaldata$rs867743_G, finaldata$rs884808_T,
                      finaldata$rs950169_T, finaldata$rs9636107_G, finaldata$rs9841616_A)

for (i in 1:length(flipSNP)){
  flipSNP[i] <- 2- flipSNP[,i] }

# drop finaldata. from start of flipSNP names
names(flipSNP) = gsub(pattern = "finaldata.", replacement = "", x = names(flipSNP), perl=T)  

# replace finaldata columns with new reverse coded ones in flipSNP
head(finaldata[,which(colnames(finaldata)%in%colnames(flipSNP))]) 
finaldata2 <- subset(finaldata[,-which(colnames(finaldata)%in%colnames(flipSNP))]) 
finaldata2 <- lapply(finaldata2[1:87], as.numeric) 
finaldata3 <- data.frame(finaldata2, flipSNP) 

# make unweighted score
finaldata3$schizscore <- rowSums(finaldata3[,32:132], na.rm=T)
names(finaldata3[32:132]) # check you have used the right vars - this section is hard coded

# making score with missing SNPs filled in with the mean of that snp
for (i in 32:132) {
  finaldata3[is.na(finaldata3[,i]), i] <- mean(finaldata3[,i], na.rm=T) }

finaldata3$schizscoremean <- rowSums(finaldata3[,32:132])

# create concised dataset with score to write to file
nonlinear_data <- finaldata3[c("id", "IID", "FID", "PID", "MID", "sex", "kids_fathered", "split", "schizophrenia", "age_fbirth_multi", "num_kids", "childless", "z_NEB", "NEB", "age_centre", "schizscore", "schizscoremean", "PC1","PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]
write.csv(nonlinear_data, file="nonlinear_data_schiz.csv", row.names=T, quote=F)


