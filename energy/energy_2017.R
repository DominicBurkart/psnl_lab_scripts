library(lme4)
library(galts)
library(BLR)

# preliminary verification of nonsignificance between control / ex --------
d = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/regr1_test.csv")
num <- sapply(d, is.numeric)
d <- d[-c(442,443,444),]
d[num] <- lapply(d[num], scale)

interactions = glm(`avg.power.use_x` ~ `Condition` * `avg.power.use_y` * `cdd_x` * `cdd_y` , data=d)
summary(interactions) #good news! All the predictors we want to be significant are, and treatment isn't.
summary(glm(`avg.power.use_x` ~ `Condition` + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d)) 
#with interactions has the superior model fit.
plot(interactions) #unequal variance issue.
lmtest::bptest(interactions) # BP = 30.942, df = 15, p-value = 0.008943 with teterboro
                             # BP = 33.228, df = 15, p-value = 0.004364 without teterboro

# Try log transformation.
interactions = glm(log10(`avg.power.use_x`) ~ Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
summary(glm(log10(`avg.power.use_x`) ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d)) #AIC slightly worse here as well
lmtest::bptest(interactions) #BP = 10.254, df = 15, p-value = 0.8034 with teterboro
                             #BP = 7.4217, df = 15, p-value = 0.9449 without teterboro
#log transforming works, but renders weather insignificant as a predictor (no face validity of results, possible power issue)

# # Bayesian linear regression is specifically reliable to this violation and last year's data works well as the prior.
# # PRIOR INFO:
# # Weather (CDD) has inconsistent distributions across months this year and last.
# d = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/regr1_test.csv")
# d <- d[,c("avg.power.use_x", "Treated", "avg.power.use_y", "cdd_x", "cdd_y")]
# d <- d[complete.cases(d),]
# y <- d[,c("avg.power.use_x")]
# #Xr <- d[,c("Treated", "cdd_x", "cdd_y")]
# X <- d[,c("Treated", "cdd_x", "cdd_y","avg.power.use_y")]
# h2 <- 0.5
# DF <- 5
# Vy <- var(y)
# Se <- (1 - h2) * Vy * (DF - 2)
# MSx <- sum(apply(FUN=var, MARGIN=2, X=X))
# Sr <- Vy * h2 * (DF - 2) / MSx
# prior <- list(varE = list(df=DF, S=Se),
#               varBR= list(df=DF, S=Sr))
# blr <- BLR(
#   y = y,
#   XR = X, #gaussian prior
# #  XL = Xl, #LASSO prior (good for more exponential data)
#   nIter = 10000,
#   burnIn = 10, #value of performing burnin is ambiguous.
#   prior = prior,
#   saveAt = "energy2017blr_"
# )
# 
# str(blr)
# 
# rm(list=ls())
# d = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/regr1_test.csv")
# d <- d[,c("avg.power.use_x", "Treated", "avg.power.use_y", "cdd_x", "cdd_y")]
# d <- d[complete.cases(d),]
# # Extract phenotype
# y<-d[,"avg.power.use_x"]
# X <- d[,c("Treated", "cdd_x", "cdd_y","avg.power.use_y")]
# # Defines the prior
# DF<-5
# Vy<-var(y)
# h2<-0.5
# Se<-Vy*(1-h2)*(DF-2)
# MSx<-sum(apply(FUN=var,MARGIN=2,X=X)) 
# Sr<-Vy*h2*(DF-2)/MSx 
# prior=list( varE=list(df=DF,S=Se) , varBR=list(df=DF,S=Sr ) )
# # Fits the model 
# fm<-BLR(y=y,XR=X, 
#         nIter=12000,burnIn=2000,prior=prior,saveAt="box4_")
# # Plot of squared-estimated effects 
# plot(fm$bR^2,col=2,ylab=expression(paste(beta[j]^2)),main="BRR")
# # Plot of estimated genetic values versus phenotypes 
# plot( fm$yHat~y,col=2,ylab="Genomic Value", 
#       xlab="Phenotype", ylim=c(-3,3),xlim=c(-3,3))
# abline(b=1,a=0,lty=2) ; abline(h=0,lty=2,v=0) 
# reg<-lm(fm$yHat~y)$coef ; abline(a=reg[1],b=reg[2],col=4); 
# print(reg) 


interactions = lmer(`avg.power.use_x` ~ 1 + Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
summary(interactions) #good news! All the predictors we want to be significant are, and treatment isn't.
summary(glm(`avg.power.use_x` ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d)) 
#with interactions has the superior model fit.
plot(interactions) #unequal variance issue.
lmtest::bptest(interactions) 


# monthly instantaneous regressions ---------------------------------------

# June --------------------------------------------------------------------


d = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/regr1_testJun2017.csv")
num <- sapply(d, is.numeric)
d = d[-148,]
d[num] <- lapply(d[num], scale)

interactions = lm(`avg.power.use_x` ~ Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
summary(interactions)
summary(glm(`avg.power.use_x` ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d))
#with interactions has the superior model fit.
plot(interactions)
lmtest::bptest(interactions) #BP = 42.348, df = 15, p-value = 0.0001986 with teterboro
                             #BP = 42.684, df = 15, p-value = 0.0001762 without teterboro


# Try log transformation.
interactions = glm(log10(`avg.power.use_x`) ~ Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
summary(glm(log10(`avg.power.use_x`) ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d)) #AIC outperforms model w/ interactions
lmtest::bptest(interactions) #BP = 13.745, df = 15, p-value = 0.545 with teterboro
                             #BP = 9.7561, df = 15, p-value = 0.835 without teterboro

# July --------------------------------------------------------------------

d = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/regr1_testJul2017.csv")

num <- sapply(d, is.numeric)
d = d[-148,]
d[num] <- lapply(d[num], scale)

interactions = glm(`avg.power.use_x` ~ Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
summary(interactions)
summary(glm(`avg.power.use_x` ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d))
#with interactions has the superior model fit here too.
plot(interactions)
lmtest::bptest(interactions) #BP = 33.012, df = 15, p-value = 0.004676 with teterboro
                             #BP = 32.908, df = 15, p-value = 0.004834 without teterboro


# Try log transformation.
interactions = glm(log10(`avg.power.use_x`) ~ Treated * `avg.power.use_y` * `cdd_x` * `cdd_y`, data=d)
noints = glm(log10(`avg.power.use_x`) ~ Treated + `avg.power.use_y` + `cdd_x` + `cdd_y`, data=d) #AIC outperforms model w/ interactions
lmtest::bptest(interactions) #BP = 9.9958, df = 15, p-value = 0.82 with teterboro
                             #BP = 10.169, df = 15, p-value = 0.809 without teterboro
lmtest::bptest(noints)

