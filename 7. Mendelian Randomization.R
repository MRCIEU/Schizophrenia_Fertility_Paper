# This script is adapted from code provided by Jack Bowden, Suzi Gage and Amy Taylor
# Note - have to clear R and load next dataset if want to use a different outcome
# Note - be careful where code uses betaXG (not absolute) and BXG (absolute)
# The data in this CSV "Pick data" for NEB and AFB are also in supplementary tables of the paper 

# Clear R
rm(list=ls(all=TRUE))

# Where you want the graphs to go

#################################################################################
# Pick Data
#################################################################################

# Schizophrenia
#setwd("**")
#data = read.csv ("data_all_NEB_EURonly.csv", header=T)
#data = read.csv ("data_all_NEB_chip_EURonly.csv", header=T)
#data = read.csv ("data_all_childless_EURonly.csv", header=T)
#data = read.csv ("data_all_childless_chip_EURonly.csv", header=T)
#data = read.csv ("data_AFB_500k_EUR.csv", header=T) 
#data = read.csv ("data_AFB_500k_EUR_chip.csv", header=T)

# Education
#setwd("**")  
#data = read.csv ("data_NEB_500.csv", header=T) 
#data = read.csv ("data_NEB_500_chip.csv", header=T) 
#data = read.csv ("data_AFB_500.csv", header=T) 
#data = read.csv ("data_AFB_500_chip.csv", header=T) 
#data = read.csv ("data_childless_500.csv", header=T) 
#data = read.csv ("data_childless_500_chip.csv", header=T) 

#################################################################################
# Prepare data for MR
#################################################################################
head(data)

BetaXG   = as.numeric(data$beta_x) # genetic association with exposure
BetaYG   = as.numeric(data$beta_y) # standard errors 
seBetaYG = as.numeric(data$SE_y) # genetic association with outcome
seBetaXG = as.numeric(data$SE_x) # standard errors

BYG             = BetaYG*sign(BetaXG) # Pre-processing steps to ensure all  
BXG             = abs(BetaXG)         # gene--exposure estimates are positive

# Count the N (dy_y plus 13 as this column is residual degrees of freedom so need to add exposure + no. of vars + 1 = 14 for NEB) 
min(data[,6], na.rm=T) + 14
max(data[,6], na.rm=T) + 14
# Count the N (dy_y plus 13 as this column is residual degrees of freedom so need to add exposure + no. of vars + 1 = 12 for AFB)
min(data[,6], na.rm=T) + 12
max(data[,6], na.rm=T) + 12

#################################################################################
# STATISTICS
#################################################################################

# Mean F stat
F   = BXG^2/seBetaXG^2
mF  = mean(F)
mF 

# I-squared function
Isq = function(y,s){
  k          = length(y)
  w          = 1/s^2; sum.w  = sum(w)
  mu.hat     = sum(y*w)/sum.w  
  Q          = sum(w*(y-mu.hat)^2)
  Isq        = (Q - (k-1))/Q
  Isq        = max(0,Isq)
  return(Isq)
}

# I-sq results
Isq(BXG,seBetaXG) # for an unweighted MR-Egger estimate 
Isq(BXG/seBetaYG,seBetaXG/seBetaYG) # for a weighted MR-Egger estimate

#################################################################################
# IVW approach
#################################################################################

IVWfit      = summary(lm(BYG ~ -1+BXG,weights=1/seBetaYG^2)) 

DF      = length(BYG)-1
IVWBeta = IVWfit$coef[1,1]
SE      = IVWfit$coef[1,2]/IVWfit$sigma
IVW_p   = 2*(1-pt(abs(IVWBeta/SE),DF))
IVW_CI  = IVWBeta + c(-1,1)*qt(df=DF, 0.975)*SE

# Cochran's Heterogeneity statistic
# for IVW method

DF      = length(BYG)-1
phi_IVW = IVWfit$sigma^2
QIVW    = DF*phi_IVW
Qp      = 1-pchisq(QIVW,DF)

phi_IVW
QIVW
Qp

# IVWResults = (point estimate, corrected standard error, 
# 95% Confidence interval, t-statistic, p-value) 
IVWResults = c(IVWBeta,SE,IVW_CI,IVWBeta/SE,IVW_p)
IVWResults_test <- as.data.frame(t(IVWResults))
print(IVWResults)

#################################################################################
# MR-Egger regression (with MAF corrected weights)  
#################################################################################
 
MREggerFit      = summary(lm(BYG ~ BXG,weights=1/seBetaYG^2))

# Inference with correct standard errors

MREggerBeta0   = MREggerFit$coef[1,1]
MREggerBeta1   = MREggerFit$coef[2,1]
SE0            = MREggerFit$coef[1,2]/MREggerFit$sigma
SE1            = MREggerFit$coef[2,2]/MREggerFit$sigma
DF             = length(BYG)-2
MRBeta0_p      = 2*(1-pt(abs(MREggerBeta0/SE0),DF))
MRBeta1_p      = 2*(1-pt(abs(MREggerBeta1/SE1),DF))
MRBeta0_CI     = MREggerBeta0 + c(-1,1)*qt(df=DF, 0.975)*SE0
MRBeta1_CI     = MREggerBeta1 + c(-1,1)*qt(df=DF, 0.975)*SE1

#################################################################################
# Weighted Median
#################################################################################

## Function
weighted.median <- function(betaIV.in, weights.in) {
  betaIV.order = betaIV.in[order(betaIV.in)]
  weights.order = weights.in[order(betaIV.in)]
  weights.sum = cumsum(weights.order)-0.5*weights.order
  weights.sum = weights.sum/sum(weights.order)
  below = max(which(weights.sum<0.5))
  weighted.est = betaIV.order[below] + (betaIV.order[below+1]-betaIV.order[below])*
    (0.5-weights.sum[below])/(weights.sum[below+1]-weights.sum[below])
  return(weighted.est) }

weighted.median.boot = function(betaXG.in, betaYG.in, sebetaXG.in, sebetaYG.in, weights.in){
  med = NULL
  for(i in 1:1000){
    betaXG.boot = rnorm(length(betaXG.in), mean=betaXG.in, sd=sebetaXG.in)
    betaYG.boot = rnorm(length(betaYG.in), mean=betaYG.in, sd=sebetaYG.in)
    betaIV.boot = betaYG.boot/betaXG.boot
    med[i] = weighted.median(betaIV.boot, weights.in)
  }
  return(sd(med)) 
  }

## Analysis
betaIV   = BYG/BXG  
weights  = (seBetaYG/BXG)^-2 
betaWM   = weighted.median(betaIV, weights) 
sebetaWM = weighted.median.boot(BXG, BYG, seBetaXG, seBetaYG, weights) 
t     = betaWM/sebetaWM
p     = 2*(1-pt(abs(t),length(BYG)-1))
WMresults = data.frame(Estimate=betaWM,Std.Error=sebetaWM,t,p)
CI.WM     = betaWM + c(-1,1)*sebetaWM
WMresults_test <- WMresults
WMresults_test$LCI <- betaWM-(sebetaWM*1.96)
WMresults_test$UCI <- betaWM+(sebetaWM*1.96)

#################################################################################

plot(BXG,BYG,pch=19,cex=1.4,main="Scatter plot",xlab="",ylab="")
mtext(side=2,expression(paste("Gene-outcome assoc.")),line=2,cex=1.5)
mtext(side=1,expression(paste("Gene-exposure assoc.")),line=2.8,cex=1.5)

MREggerFit  = lm(BYG ~ BXG,weights=1/seBetaYG^2)
IVWFit      = lm(BYG ~ -1+BXG,weights=1/seBetaYG^2)

lines(BXG,MREggerFit$fitted.values,col="blue",lwd=2)
lines(BXG,IVWFit$fitted.values,col="red")
lines(BXG,betaWM*BXG,col="black",lwd=2)

legend("topleft", c("IVW slope","MR-Egger slope","Weighted Median Slope"),
       col=c("red","blue","black"),lwd=3,cex=1.1,bty="n")

dev.print(device=pdf,file=plotfile[1])

MREggerResults     = matrix(nrow = 2,ncol = 6)
MREggerResults[1,] = c(MREggerBeta0,SE0,MRBeta0_CI,MREggerBeta0/SE0,MRBeta0_p)
MREggerResults[2,] = c(MREggerBeta1,SE1,MRBeta1_CI,MREggerBeta1/SE1,MRBeta1_p)

BetaIV = BYG/BXG
Xrange = range(BetaIV,MRBeta1_CI)
Yrange = range(BXG/seBetaYG)

plot(BetaIV,BXG/seBetaYG,pch=19,cex=1.4,main="Funnel plot",xlab="",ylab="",xlim=Xrange,ylim=Yrange)
lines(rep(IVWBeta,2),c(0,Yrange[2]),col="red",lwd=2)
lines(rep(MREggerBeta1,2),c(0,Yrange[2]),col="blue",lwd=2)
lines(rep(betaWM,2),c(0,Yrange[2]),col="black",lwd=2)
mtext(side=1,expression(paste("Causal estimate")),line=3.5,cex=1.5)
mtext(side=2,expression(paste("MAF-corrected Instrument strength")),line=2,cex=1.5)

points(IVWBeta,10,cex=2,col="red",pch=15)
lines(IVW_CI,rep(10,2),lwd=2,col="red")
points(MREggerBeta1,6,cex=2,col="blue",pch=22)
lines(MRBeta1_CI,rep(6,2),lwd=2,col="blue")

points(betaWM,8,cex=2,col="black",pch=23)
lines(CI.WM,rep(8,2),lwd=2,col="black")


points(BetaIV,BXG/seBetaYG,pch=19,cex=1.4)

legend("topright",c("IVW","MR-Egger","Weighted Median"),lwd=2,
       col=c("red","blue","black"),cex=1,bty="n",pch=c(15,22,23))

dev.print(device=pdf,file=plotfile[2])

#################################################################################
# Summary of results
#################################################################################

# IVW approach
#summary(IVWFit)
# IVW with corrected standard errors - Beta,SE,CI,t,p
IVWResults

# MR-Egger approach
#summary(MREggerFit)
print(MREggerResults) # MREggerResults = (point estimate, corrected standard error, 95% Confidence interval, t-statistic, p-value) for intercept (row 1) and slope (row 2).

# Weighted Median - may be different each time as bootstrapped
WMresults

# Cochran's Q statistic for IVW (and p-value)
QIVW 
Qp 

# Mean F statistic for IVW
mF  

# I^2_GX for MR-Egger
Isq(BXG,seBetaXG) # unweighted
Isq(BXG/seBetaYG,seBetaXG/seBetaYG) # weighted 

# Write results to dataframe
Results <- data.frame(Beta=numeric(), SE=numeric(), LCI=numeric(), UCI=numeric(), t=numeric(), p=numeric()) 
colnames(IVWResults_test) <- c("Beta", "SE", "LCI", "UCI", "t", "p") 
colnames(MREggerResults) <- c("Beta", "SE", "LCI", "UCI", "t", "p")
colnames(WMresults_test) <- c("Beta", "SE", "t", "p","LCI", "UCI")
Results <- rbind(Results, IVWResults_test) 
Results <- rbind(Results, MREggerResults)
Results <- rbind(Results, WMresults_test)
rownames(Results) <- c("IVW", "MR-Egger Intercept", "MR-Egger Slope", "Weighted Median")
Results <- format(round(Results[,1:6],4)) 
Results <- Results[,c(1,3,4,5,6,2)] 
Results_word <- Results[,c("Beta", "LCI", "UCI", "p")] # table just of beta and CI's for word

#################################################################################
# Plots of results 
#################################################################################

# FUNNEL PLOT:
plot(BetaIV,BXG/seBetaYG,pch=19,cex=1.4,main="SchizNEB",xlab="",ylab="")
lines(rep(IVWBeta,2),c(0,Yrange[2]),col="red",lwd=2)
#lines(rep(MREggerBeta1,2),c(0,Yrange[2]),col="blue",lwd=2) (took out Mr-Egger as we did not include it in the paper)
legend("topright",c("IVW"),lwd=2,
       col=c("red"),cex=1.6,bty="n",pch=c(15,22))
mtext(side=1,expression(paste("Causal estimate  ", hat(beta)[j])),line=3.5,cex=1.5)
mtext(side=2,expression(paste("Instrument strength   ",hat(gamma)[j]^"C")),line=1.5,cex=1.5)

points(IVWBeta,0.2*Yrange[2],cex=2,col="red",pch=15)
lines(IVW_CI,rep(0.2*Yrange[2],2),lwd=2,col="red")
#points(MREggerBeta1,0.1*Yrange[2],cex=2,col="blue",pch=22)
#lines(MRBeta1_CI,rep(0.1*Yrange[2],2),lwd=2,col="blue")

# PLOT
plot(BXG,BYG,pch=19,cex=1.4,main="SchizNEB",xlab="",ylab="")
mtext(side=2,expression(paste("Gene-outcome coeff:  ", hat(Gamma)[j])),line=2,cex=1.5)
mtext(side=1,expression(paste("Gene-exposure coeff:   ",hat(gamma)[j])),line=2.8,cex=1.5)

MREggerFit      = lm(BYG ~ BXG,weights=1/seBetaYG^2)
lines(BXG,MREggerFit$fitted.values,col="blue")

IVWFit = lm(BYG ~ -1+BXG,weights=1/seBetaYG^2)
lines(BXG,IVWFit$fitted.values,col="red")

legend("topleft",c("IVW","MR-Egger"),lwd=3,
       col=c("red","blue"),cex=1.6,bty="n")


