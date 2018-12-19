
# Load data
finaldata <- read.csv("schizRep_finaldata.csv", header=T) 
dim(finaldata) # 337106

# Load num_partners
nsp <- read.csv("nsp_pooled.csv")
dim(nsp)

# Merge
finaldata <- merge(finaldata, nsp, by=c("id"), finaldata=T) # 337 104 
finaldataAll <- finaldata

## work with finaldata and delete rest of objects
ls()
library(gdata)
keep(finaldataAll)
keep(finaldataAll, sure=T)
ls()
# attach this final dataset to memory
attach(finaldataAll) 


#################################################################################
# Summary stats num_partners 
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()

for (i in 32:132) { # column numbers so change if ever edit
  model <- lm(finaldataAll$num_partners ~ finaldataAll[,i] + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}

snp_names <- names(finaldataAll)[32:132]


sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_num_partners.csv", sum_stats, quote=F, row.names=F)

#################################################################################
# Summary stats tenth percentile nsp
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()


for (i in 32:132) { 
  model <- glm(finaldataAll$ten_nsp ~ finaldataAll[,i] + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10, family = binomial(link = logit), data = finaldataAll) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}

snp_names <- names(finaldataAll)[32:132]


sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_500_ten_nsp.csv", sum_stats, quote=F, row.names=F)






#################################################################################
# Summary stats num_partners CHIP
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()


for (i in 32:132) { 
  model <- lm(finaldataAll$num_partners ~ finaldataAll[,i] + chip + sex +  age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}

snp_names <- names(finaldataAll)[32:132]


sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_all_num_partners_chip.csv", sum_stats, quote=F, row.names=F)


#################################################################################
# Summary stats tenth percentile nsp CHIP
#################################################################################

beta_y <- c()
SE_y <- c()
df_y <- c()


for (i in 32:132) { 
  model <- glm(finaldataAll$ten_nsp ~ finaldataAll[,i] + sex + chip + age_centre + PC1 + PC2 + PC3 +PC4 +PC5 + PC6 + PC7 + PC8 + PC9 + PC10, family = binomial(link = logit), data = finaldataAll) 
  beta <- model$coefficients[[2]] 
  se <- coef(summary(model))[2, 2] 
  df <- model$df.residual
  c(beta_y, beta)-> beta_y 
  c(SE_y, se) -> SE_y
  c(df_y, df) -> df_y
}

snp_names <- names(finaldataAll)[32:132]

sum_stats <- data.frame(cbind(snp_names, beta_y, SE_y, df_y))

write.csv(file="sum_stats_500_ten_nsp_chip.csv", sum_stats, quote=F, row.names=F)
