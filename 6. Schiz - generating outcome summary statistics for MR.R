
# Note: sometimes have to exit R inbetween loops
# Note: this is hard coded for column numbers
# After summary stats are generated - stick with exposure GWAS effect estimates (beta_x and SE_x) in a .csv file to become MR dataset
# The data in this CSV file for NEB and AFB is also in supplementary tables of the paper 

#################################################################################
# Data
#################################################################################

# Read in data
finaldata <- read.csv("schizRep_finaldata.csv", header=T) 
finaldataAll <- finaldata 

# Delete rest of objects
ls()
library(gdata)
keep(finaldataAll)
keep(finaldataAll, sure=T)
ls()
# Attach this final dataset to memory
attach(finaldataAll) 

#################################################################################
# Summary stats NEB 
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- lm(finaldataAll$NEB ~ finaldataAll[,i] + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_NEB.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats NEB using Poisson regression as an analysis check
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <-glm(finaldataAll$NEB ~ finaldataAll[,i] + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10, family="poisson", data = finaldataAll)
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_NEB_POISSON.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats NEB with chip
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- lm(finaldataAll$NEB ~ finaldataAll[,i] + chip + sex + age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_chip_NEB.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats Childlessness 
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- glm(finaldataAll$childless ~ finaldataAll[,i] + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10, family = binomial(link = logit), data = finaldataAll) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_500_childless.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats Childlessness with chip
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- glm(finaldataAll$childless ~ finaldataAll[,i] + chip + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10, family = binomial(link = logit), data = finaldataAll) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_500_childless_chip.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats AFB - only females so not adjusted sex, took out adjustment for age as age is outcome
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- lm(finaldataAll$age_fbirth_multi ~ finaldataAll[,i] + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_AFB.csv", sum_stats, quote=F, row.names=F)

#################################################################################
#10) Summary stats AFB with chip - only females so not adjusted sex, took out adjustment for age as age is outcome
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { 
  model <- lm(finaldataAll$age_fbirth_multi ~ finaldataAll[,i] + chip + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}
# plucking out the name of the snps because I used col names (32:132)
snp_names <- names(finaldataAll)[32:132]

# sticking the coef, standrad error, and snp_names
sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_chip_AFB.csv", sum_stats, quote=F, row.names=F)



