library(sqldf)
library(lme4)
library(rdrobust)
library(MuMIn)

df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/summer_data.csv")

condnames <- unique(df$Condition)
x = aov(avg.power.use ~ Condition, data=df)
summary(x) # F of 2.754 P of 0.0275 due to outliers / conflation of condition with weather

l <- vector("list", length(condnames))
i = 1
for (c in unique(df$Condition)){
  l[[i]] = sqldf(paste('  select avg("avg.power.use") from df where Condition=', "'", c, "' ", sep=""))
  i = i + 1
}
barplot(unlist(l))

l <- vector("list", length(condnames))
i = 1
for (c in unique(df$Condition)){
  l[[i]] = sqldf(paste('  select sum("avg.power.use") from df where Condition=', "'", c, "' ", sep=""))
  i = i + 1
}
barplot(unlist(l))

l <- vector("list", length(condnames))
i = 1
for (c in unique(df$Condition)){
  l[[i]] = sqldf(paste('  select median("avg.power.use") from df where Condition=', "'", c, "' ", sep=""))
  i = i + 1
}
barplot(unlist(l))

m <- vector("list", length(condnames))
s <- vector("list", length(condnames))
i = 1
for (c in unique(df$Condition)){
  m[[i]] = sqldf(paste('  select avg("avg.power.use") from df where Condition=', "'", c, "' ", sep=""))
  s[[i]] = sqldf(paste('  select stdev("avg.power.use") from df where Condition=', "'", c, "' ", sep=""))
  i = i + 1
}
segments(barplot(unlist(m)), unlist(m) - unlist(s), barplot(unlist(m)), unlist(m) + unlist(s))

print(condnames)


df2 = sqldf(' select * from df order by "avg.power.use" desc')
#see the first three vals are clear outliers (teterboro)
df2 = df2[4:nrow(df2),]

summary(aov(scale(avg.power.use) ~ Condition, data=df2)) #F of 0.386 P of 0.819 (even with unaccounted-for colinearity with municipality)
summary(aov(scale(avg.power.use) ~ scale(cdd) * Condition, data=df2)) # F of 0.982 P of 0.417 (even with unaccounted-for colinearity with municipality)
summary(lmer(scale(avg.power.use) ~ scale(cdd) + Condition + (1 | MunicipalCityCode), data=df2)) # See anova (following line)
anova(lmer(scale(avg.power.use) ~ scale(cdd) + Condition + (1 | MunicipalCityCode), data=df2)) # F of 0.3695

summary(lm(avg.power.use ~ Condition, data=df2)) #smallest P: 0.250
summary(lm(avg.power.use ~ MunicipalCityCode + Condition, data=df2)) #Municipality / Conditions are too colinear without collapsing by town
summary(lm(avg.power.use ~ MunicipalCityCode + cdd + Condition, data=df2)) #Too colinear

df3 = sqldf(' select avg("avg.power.use"),avg("cdd"),"Condition","MunicipalCityCode" from df2 group by "MunicipalCityCode" ')
colnames(df3) = c("power", "cdd", "condition", "city")
summary(aov(power ~ condition, data = df3)) #F of 0.285 P of 0.887

df3 = sqldf(' select median("avg.power.use"),avg("cdd"),"Condition","MunicipalCityCode" from df2 group by "MunicipalCityCode" ')
colnames(df3) = c("power", "cdd", "condition", "city")
summary(aov(power ~ condition, data = df3)) #F of 0.181 P of 0.948

df3 = sqldf(' select sum("avg.power.use"),sum("cdd"),"Condition","MunicipalCityCode","Treated" from df2 group by "MunicipalCityCode" ')
colnames(df3) = c("power", "cdd", "condition", "city")
summary(aov(power ~ condition, data = df3)) #F of 0.285 P of 0.887

df2$pseg.month = scale(df2$pseg.month)


rm(list=ls())
library(rdd)
df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
month = df$NumericMonth
#month = scale(df$NumericMonth)
cdd = scale(df$cdd)
treated = df$Treated
condition = df$Condition
town = df$MunicipalCityCode
rd = RDestimate(power ~ month | cdd, cutpoint = 9, cluster=town, verbose = T )
summary(rd)
plot(rd)
rd = RDestimate(power ~ month | cdd + town, cutpoint = 9, verbose = T)
covs = c(cdd)
rdplot(y=power, x=month, covs=covs, p = 1, c = 8, x.label = "Month", y.label= "Power Use", title = "With weather", verbose=T)
plot(rd)
summary(rd)
rdplot(y = power, x = month, c = 8, p = 1)

rd = RDestimate(power ~ month)
rdplot(y=power, x=month, c = 0, x.label = "Month", y.label= "Power Use", title = "Without weather")
plot(rd)
summary(rd)

lm(formula = form, data = subset(data, w > 0), weights = w)


town = df$MunicipalCityCode
condition = df$Condition
month = df$pseg.month
month_lengths = scale(df$pseg.month.length)
h = lmer(power ~ cdd + condition + (1|month)+(1|town))
summary(h)
car::Anova(h)


rdrobust(y=power, x=month)

npdf = data.frame(power, condition, month, cdd)

bw = npregbw(power ~ condition + month + cdd, data=npdf)
plot(bw)

library(devtools)
#install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
library("RDDtools")

df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]

power = scale(df$avg.power.use)
month = df$NumericMonth
cdd = scale(df$cdd)
covars = data.frame(cdd)
#covars = data.frame(scale(df$cdd), df$Condition)
rdat = RDDdata(y = power, x = month, cutpoint = 8, covar = covars)
summary(rdat)
plot(rdat)
rline = RDDreg_lm(rdat, covariates = "power~month+cdd")
plot(rline)
summary(rline)


rm(list=ls())
df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
month = df$NumericMonth
smonth = scale(month)
#month = scale(df$NumericMonth)
cdd = scale(df$cdd)
treated = df$Treated
condition = df$Condition
town = df$MunicipalCityCode
library(rdd)
df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
cdd = scale(df$cdd)
month = df$NumericMonth
town = df$MunicipalCityCode
x = RDestimate(power ~ month | cdd, cluster = town, cutpoint = 9, verbose = T, bw = 10)
plot(x)
summary(x)

p = rdplot(y=power, x=month, covs = cdd, p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "With weather")
rdplot(y=power, x=month, p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "Without weather")
x = RDestimate(power ~ month | cdd, cluster = town, cutpoint = 8, verbose = T, bw=8)
summary(x)
plot(x)

rm(list=ls())
df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
month = df$NumericMonth
smonth = scale(month)
#month = scale(df$NumericMonth)
cdd = scale(df$cdd)
treated = df$Treated
condition = df$Condition
town = df$MunicipalCityCode
library(RDDtools)
d = RDDdata(y=power,x=month, covar = c(cdd, town), cutpoint = 8)
l = RDDreg_lm(RDDobject = d)
summary(d)
plot(l)
summary(l)
dens_test(l)

rm(list=ls())
df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[!df$MunicipalCityCode == "teterboro boro",]
df = df[complete.cases(df),]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
month = df$NumericMonth
smonth = scale(month)
nom_month = df$end.month
cdd = scale(df$cdd)
treated = df$Treated
condition = df$Condition
town = df$MunicipalCityCode
rdrobust( power, month, covs = cdd)
rdrobust( power, month) 
rdrobust( power, smonth)

lens = scale(df$pseg.month.length)

p8 = rdplot(y=power, x=month, p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 Without weather", y.lim = c(-0.4, 0.4))
p8 = rdplot(y=power, x=month, p = 1, c = 6, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 Without weather", y.lim = c(-0.4, 0.4))
p8 = rdplot(y=power, x=month, p = 1, c = 4, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 Without weather", y.lim = c(-0.4, 0.4))

p8 = rdplot(y=power, x=month, covs = cdd, p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 With CDD", y.lim = c(-0.4, 0.4))
p6 = rdplot(y=power, x=month, covs = cdd, p = 1, c = 6, x.label = "Month", y.label= "Power Use (Z scored)", title = "6 With CDD", y.lim = c(-0.4, 0.4))
p4 = rdplot(y=power, x=month, covs = cdd, p = 1, c = 4, x.label = "Month", y.label= "Power Use (Z scored)", title = "4 With CDD", y.lim = c(-0.4, 0.4))

p8 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 With CDD and month length", y.lim = c(-0.4, 0.4))
p6 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 6, x.label = "Month", y.label= "Power Use (Z scored)", title = "6 With CDD and month length", y.lim = c(-0.4, 0.4))
p4 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 4, x.label = "Month", y.label= "Power Use (Z scored)", title = "4 With CDD and month length", y.lim = c(-0.4, 0.4))

p8 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 8, x.label = "Month", y.label= "Power Use (Z scored)", title = "8 With CDD and month length", y.lim = c(-0.4, 0.4))
p6 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 6, x.label = "Month", y.label= "Power Use (Z scored)", title = "6 With CDD and month length", y.lim = c(-0.4, 0.4))
p4 = rdplot(y=power, x=month, covs = data.frame(cdd,lens), p = 1, c = 4, x.label = "Month", y.label= "Power Use (Z scored)", title = "4 With CDD and month length", y.lim = c(-0.4, 0.4))

cor.test(power, cdd)

# New Analysis Scheme -----------------------------------------------------

rm(list=ls())

df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/formatted_data.csv")
df = df[complete.cases(df),]
df = df[!df$MunicipalCityCode == "teterboro boro",]
df$avg.power.use = as.numeric(df$avg.power.use)
power = scale(df$avg.power.use)
month = df$NumericMonth
smonth = scale(month)
#month = scale(df$NumericMonth)
cdd = scale(df$cdd)
treated = df$Treated
condition = df$Condition
town = df$MunicipalCityCode
nom_month = df$end.month
library(RDDtools)
d = RDDdata(y=power, x=month, covar = data.frame(Z1 = cdd, Z2 = town, Z3 = nom_month), cutpoint = 8)
l = RDDreg_lm(RDDobject = d, bw = 100,)
plot(l)
summary(l)


# basic MLM ---------------------------------------------------------------
library(lmerTest)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months.csv")
customers = scale(nov22data$customers)
route = as.factor(nov22data$route)
kwh = scale(nov22data$kwh)
thi = scale(nov22data$thi)
condition = nov22data$Condition
city = nov22data$MunicipalCityCode
treatment = nov22data$Treated
month = nov22data$month
psegmonth = nov22data$psegMonth
year = nov22data$year

r = lmer(kwh ~ treatment + (1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month) )
summary(r)
plot(r)
anova(r)

r = lmer(kwh ~ treatment + customers + (1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month) )
summary(r)
plot(r)
anova(r)

r = lmer(kwh ~ treatment + ( 1 | customers) + (1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month) )
summary(r)
plot(r)
anova(r)

#insig
r = lmer(kwh ~ (1 | thi) + treatment + customers + ( 1 | city))
summary(r) # See anova (following line)
anova(r)
plot(r)

r = lmer(kwh ~ thi + treatment + customers + ( 1 | city))
summary(r) # See anova (following line)
anova(r)
plot(r)

r = lmer(kwh ~ treatment + customers + (1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month) + ( 1 | year))
summary(r)
plot(r)
anova(r)

r = lmer(kwh ~ treatment + customers + (1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month) )
summary(r)
plot(r)
anova(r)

r = lmer(kwh ~ treatment + customers + ( 1 | route ) + ( thi | city) + (1 | month) )
summary(r)
plot(r)
anova(r)

#insig
r = lmer(kwh ~ treatment + customers + ( 1 | route ) + ( thi | city) + ( thi | month) )
summary(r)
plot(r)
anova(r)

r = lmer(kwh ~ treatment + ( customers | thi ) + ( 1 | route ) + ( 1 | city) + ( 1 | month) )
summary(r)
plot(r)
anova(r)

# treatment effects -------------------------------------------------------

library(lmerTest)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months.csv")
customers = scale(nov22data$customers)
route = as.factor(nov22data$route)
kwh = scale(nov22data$kwh)
thi = scale(nov22data$thi)
condition = nov22data$Condition
city = nov22data$MunicipalCityCode
treatment = nov22data$Treated
month = nov22data$month
psegmonth = nov22data$psegMonth
year = as.factor(nov22data$year)
wave = as.factor(nov22data$Start) #Levels: 07/20/17 (wave 1) and 08/01/17 (wave 2)

# reassert matchedness on DV
months = unique(nov22data$psegMonth)
for (m in months){
  library(Hmisc)
  conditions = unique(nov22data$Condition)
  dfs = list()
  i = 1
  for (con in conditions){
    munis = unique(nov22data[nov22data$Condition == con,]$MunicipalCityCode)
    dfs[[i]] = subset(nov22data, MunicipalCityCode %in% munis & (psegMonth == m))
    dfs[[i]]$condition = con
    i = i + 1
  }
  
  for_anova = rbind(dfs[[2]], dfs[[3]], dfs[[4]], dfs[[5]], dfs[[6]])
  for_anova$per_cap = impute(for_anova$kwh / for_anova$customers, 0)
  for_anova = for_anova[, c("kwh", "per_cap", "condition", "year")]
  colnames(for_anova) = c("kwh", "kwh_per_capita", "condition", "year")
  
  a = aov(kwh ~ condition, data=for_anova)
  summary(a)
  plot(a)
  a = aov(kwh_per_capita ~ condition, data=for_anova)
  summary(a)
  plot(a)
}

# All periods outside of the months are labeled control
x = nov22data$Condition[1] #known Nan
c = nov22data$Condition
condition = replace(c, c==x, "Co")

r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)

library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list( array = c(1.114e-02, 1.096e-02, 1.018e-02, 1.152e-02), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

wave1 = nov22data[nov22data$Start == "07/20/17",]
customers = scale(wave1$customers)
route = as.factor(wave1$route)
kwh = scale(wave1$kwh)
thi = scale(wave1$thi)
condition = wave1$Condition
city = wave1$MunicipalCityCode
treatment = wave1$Treated
month = wave1$month
psegmonth = wave1$psegMonth
year = as.factor(wave1$year)
wave = as.factor(wave1$Start) #all wave 1

# All periods outside of the months are labeled control
x = nov22data[nov22data$Start == "07/20/17",]$Condition[1] #Nan
c = nov22data[nov22data$Start == "07/20/17",]$Condition
condition = replace(c, c==x, "Co")

r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")


wave2 = nov22data[nov22data$Start == "08/01/17",]
customers = scale(wave2$customers)
route = as.factor(wave2$route)
kwh = scale(wave2$kwh)
thi = scale(wave2$thi)
condition = wave2$Condition
city = wave2$MunicipalCityCode
treatment = wave2$Treated
month = wave2$month
psegmonth = wave2$psegMonth
year = as.factor(wave2$year)
wave = as.factor(wave2$Start) #all wave 2

# All periods outside of the months are labeled control
x = nov22data[nov22data$Start == "08/01/17",]$Condition[1] #Nan
c = nov22data[nov22data$Start == "08/01/17",]$Condition
condition = replace(c, c==x, "Co")

r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")


#reset to waves 1 + 2 
customers = scale(nov22data$customers)
route = as.factor(nov22data$route)
kwh = scale(nov22data$kwh)
thi = scale(nov22data$thi)
condition = nov22data$Condition
city = nov22data$MunicipalCityCode
treatment = nov22data$Treated
month = nov22data$month
psegmonth = nov22data$psegMonth
year = as.factor(nov22data$year)
wave = as.factor(nov22data$Start) #Levels: 07/20/17 (wave 1) and 08/01/17 (wave 2)

#randomized condition (should yield insignificant results)
x = nov22data$Condition[1] #Nan
c = nov22data$Condition
condition = replace(c, c==x, "Co")
set.seed(50)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

set.seed(5)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

set.seed(1)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

set.seed(7)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

set.seed(3)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
#anova(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

# reset condition
condition = nov22data$Condition
x = nov22data$Condition[1] #Nan
c = nov22data$Condition
condition = replace(c, c==x, "Co")

r = lmer(kwh ~ condition + (1 | wave ) + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
c = coef(r)
library(plotly)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

# Do fixed effect of customers instead of random effect; see interactions
r = lmer(kwh ~ condition * customers + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
# Interactions are positive, but I don't have any interpretation here.
# Smaller municipalities are more susceptible to 
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust", "Social:Customers", "Temporal:Customers", "Prospection:Customers","Trust:Customers"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1], c$city$`conditionSo:customers`[1], c$city$`conditionTe:customers`[1], c$city$`conditionPr:customers`[1], c$city$`conditionTr:customers`[1]),
  #error_y = list( array = c(), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")



#random thi slopes by city. drop route to reduce # of parameters.
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | route) + ( thi | city) + ( 1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$city$conditionPr
so = c$city$conditionSo
te = c$city$conditionTe
tr = c$city$conditionTr
thi_slopes = c$city$thi
sc = 10#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "All Conditions' Random Slopes", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in thi_slopes){
  abline(a = 0, b = l, col=rgb(0, 1, 1, 0.4))
}
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}

r = lmer(kwh ~ condition + ( thi | customers) + ( 1 | route) + ( 1 | city) + ( 1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$customers$conditionPr
so = c$customers$conditionPr
te = c$customers$conditionPr
tr = c$customers$conditionPr
thi_slopes = c$customers$thi
sc = 10#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "THI on Customers Random Slopes", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in thi_slopes){
  abline(a = 0, b = l, col=rgb(0, 1, 1, 0.4))
}
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}


r = lmer(kwh ~ condition + ( customers | thi) + ( 1 | route) + ( 1 | city) + ( 1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$thi$conditionPr
so = c$thi$conditionPr
te = c$thi$conditionPr
tr = c$thi$conditionPr
thi_slopes = c$thi$customers
sc = 20#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-10, 0, 10), type = "n", main = "Customers on THI Random Slopes", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in thi_slopes){
  abline(a = 0, b = l, col=rgb(0, 1, 1, 0.4))
}
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}

# does not converge
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route) + ( 1 | city) + ( condition | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$month$conditionPr
so = c$month$conditionPr
te = c$month$conditionPr
tr = c$month$conditionPr
sc = 20#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-10, 0, 10), type = "n", main = "Condition Effects by Month", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}

# only random effects: random condition slopes on month
r = lmer(kwh ~ ( 1 | customers) + ( 1 | thi) + ( 1 | route) + ( 1 | city) + ( condition | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$month$conditionPr
so = c$month$conditionPr
te = c$month$conditionPr
tr = c$month$conditionPr
sc = 20#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-10, 0, 10), type = "n", main = "Condition Effects by Month", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}

# UNRUN only random effects: random condition slopes on city 
r = lmer(kwh ~ ( 1 | customers) + ( 1 | thi) + ( 1 | route) + ( condition | city) + ( 1 | month))
summary(r)
plot(r)
anova(r)
c = coef(r)
pr = c$month$conditionPr
so = c$month$conditionPr
te = c$month$conditionPr
tr = c$month$conditionPr
sc = 20#scale of x axis in graphs
plot(c(-sc, 0, sc), c(-10, 0, 10), type = "n", main = "Condition Effects by Month", xlab = "", ylab= "")
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Prospection Random Slopes", xlab = "", ylab= "")
for (l in pr){
  abline(a = 0, b = l, col=rgb(0, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Social Proof Random Slopes", xlab = "", ylab= "")
for (l in so){
  abline(a = 0, b = l, col=rgb(1, 1, 0, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Temporal Discounting Random Slopes", xlab = "", ylab= "")
for (l in te){
  abline(a = 0, b = l, col=rgb(1, 0, 1, 0.4))
}
#plot(c(-sc, 0, sc), c(-1, 0, 1), type = "n", main = "Trust Random Slopes", xlab = "", ylab= "")
for (l in tr){
  abline(a = 0, b = l, col=rgb(0, 0, 0, 0.4))
}

r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route:city) + ( 1 | month))
summary(r)
plot(r)
anova(r)

#better fit, but higher intercorrelation of fixed effects and weird changes in significance
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route:city:month))
summary(r)
plot(r)
anova(r)

#not recommended (maximally random effects preferred, larger residual)
r = lmer(kwh ~ condition + customers + thi + ( 1 | route:month:city))
summary(r)
plot(r)
anova(r)


#rerun analysis only including routes with at least 100 households:
library(lmerTest)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months.csv")
nov22data = nov22data[nov22data$customers > 100,] #halves data; from 24440 cases to 12437
customers = scale(nov22data$customers)
route = as.factor(nov22data$route)
kwh = scale(nov22data$kwh)
thi = scale(nov22data$thi)
condition = nov22data$Condition
city = nov22data$MunicipalCityCode
treatment = nov22data$Treated
month = nov22data$month
psegmonth = nov22data$psegMonth
year = as.factor(nov22data$year)

# All periods outside of the months are labeled control
x = nov22data$Condition[1] #Nan
c = nov22data$Condition
condition = replace(c, c==x, "Co")

r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | route ) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

# disclude potential outliers -------------------------------------------------------
rm(list=ls())
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months.csv")
customers = scale(nov22data$customers)
route = as.factor(nov22data$route)
kwh = scale(nov22data$kwh)
thi = scale(nov22data$thi)
condition = nov22data$Condition
city = nov22data$MunicipalCityCode
treatment = nov22data$Treated
month = nov22data$month
psegmonth = nov22data$psegMonth
year = as.factor(nov22data$year)
wave = as.factor(nov22data$Start) #Levels: 07/20/17 (wave 1) and 08/01/17 (wave 2)

#impute NaNs with "Control" condition
condition = nov22data$Condition
x = nov22data$Condition[1] #Nan
c = nov22data$Condition
condition = replace(c, c==x, "Co")


# model at the municipality level -----------------------------------------

rm(list=ls())
library(sqldf)
library(lmerTest)
library(plotly)
library(MuMIn)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months.csv")

# All periods outside of the months are labeled control
x = nov22data$Condition[1] #known Nan
c = nov22data$Condition
condition_imputed = replace(c, c==x, "Co")
df = data.frame(nov22data,condition_imputed)
df$Treated = replace(df$Treated, df$Treated==unique(df$Treated)[3], "No")

conts = c("customers", "kwh", "thi")
cats = c("Treated", "condition_imputed", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")

sql_cmd = paste("select ", paste(cats, collapse = ", "), ", avg(", paste(conts, collapse = "), avg("), "), sum(", paste(conts, collapse="), sum("), ") from df group by MunicipalCityCode, psegMonth")
munidf = sqldf(sql_cmd)

#cool! but we want to have the average THI for each muni/month combo weighted by the number of customers read.
for_weighted = c("thi") #from df
weight_by_route = c("customers")
munidf$weighted_thi = NA
for (m in unique(df$psegMonth)){
  for (t in unique(df$MunicipalCityCode)){
    tdf = df[df$psegMonth == m & df$MunicipalCityCode == t, c(for_weighted, weight_by_route)]
    for (wc in for_weighted) { #next line is only designed to change THI (only relevant var at analysis time)
      munidf[munidf$psegMonth == m & munidf$MunicipalCityCode == t,]$weighted_thi  = sum(tdf[,wc] * tdf[,weight_by_route] / sum(tdf[,weight_by_route]))
    }
  }
}

kwh = scale(munidf$`sum(kwh)`) #effect is smaller but significant with logged data
thi = scale(munidf$weighted_thi)
customers = scale(munidf$`sum( customers)`)

treated = munidf$Treated
condition = munidf$condition_imputed
month = munidf$month
city = munidf$MunicipalCityCode

#significant results at municipal level
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
plot(r)
c = coef(r)
r_month = r

r.squaredGLMM(r) # 0.00492265 0.99101013 

kwh = scale(log(munidf$`sum(kwh)`))
thi = scale(munidf$weighted_thi)
customers = scale(munidf$`sum( customers)`)

treated = munidf$Treated
condition = munidf$condition_imputed
month = munidf$month
city = munidf$MunicipalCityCode

plotmeans(kwh  ~ condition, xlab="Condition",
          ylab="KWH", main="Group Means with 95% CI", connect = F)

plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list( array = c(0.03802, 0.03858, 0.03766, 0.03662), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

#still detects customers 
r = lmer(kwh ~ condition + customers + ( 1 | thi) + ( 1 | city) + (1 | munidf$psegMonth))
summary(r)

r = lmer(kwh ~ treated + (1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | munidf$psegMonth))
summary(r)

wave1 = munidf[munidf$Start == "07/20/17",]

kwh1 = scale(wave1$`sum(kwh)`)
thi1 = scale(wave1$weighted_thi)
customers1 = scale(wave1$`sum( customers)`)

treated1 = wave1$Treated
condition1 = wave1$condition_imputed
month1 = wave1$month
city1 = wave1$MunicipalCityCode

r = lmer(kwh1 ~ condition1 + ( 1 | customers1) + ( 1 | thi1) + ( 1 | city1) + (1 | month1))
summary(r)
plot(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city1$condition1So[1],coef(r)$city1$condition1Te[1], coef(r)$city1$condition1Pr[1], coef(r)$city1$condition1Tr[1]),
  error_y = list( array = c(0.05454, 0.05603, 0.05562, 0.05304), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

wave2 = munidf[munidf$Start == "08/01/17",]

kwh2 = scale(wave2$`sum(kwh)`)
thi2 = scale(wave2$weighted_thi)
customers2 = scale(wave2$`sum( customers)`)

treated2 = wave2$Treated
condition2 = wave2$condition_imputed
month2 = wave2$month
city2 = wave2$MunicipalCityCode

r = lmer(kwh2 ~ condition2 + ( 1 | customers2) + ( 1 | thi2) + ( 1 | city2) + (1 | month2))
summary(r)
plot(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city2$condition2So[1],coef(r)$city2$condition2Te[1], coef(r)$city2$condition2Pr[1], coef(r)$city2$condition2Tr[1]),
  error_y = list( array = c(0.05737, 0.05741, 0.05457, 0.05738), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

#insig when IV randomized
set.seed(50)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

#insig when IV randomized
set.seed(5)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

#insig when IV randomized
set.seed(1)
condition = sample(condition)
r = lmer(kwh ~ condition + ( 1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  name = "Treatment Effects",
  type = "bar")

#reset condition
condition = munidf$condition_imputed

# reassert matchedness on DV
months = unique(munidf$psegMonth)
to_join = list()
for (m in months){
  library(Hmisc)
  conditions = unique(munidf$Condition)
  dfs = list()
  i = 1
  for (con in conditions){
    munis = unique(munidf[munidf$Condition == con,]$MunicipalCityCode)
    dfs[[i]] = subset(munidf, MunicipalCityCode %in% munis & (psegMonth == m))
    dfs[[i]]$condition_for_matchedness = con
    i = i + 1
  }
  to_join[[m]] = rbind(dfs[[2]], dfs[[3]], dfs[[4]], dfs[[5]], dfs[[6]])
}
joined_df = do.call(rbind, to_join)
joined_df = joined_df[! joined_df$psegMonth %in% c("Jul2017","Aug2017","Sep2017","Oct2017"),]
a = aov(`sum(kwh)` ~ condition_for_matchedness, data=joined_df)
summary(a)

summary(lm(`sum(kwh)` ~ condition_for_matchedness, data=joined_df))
anova(lm(`sum(kwh)` ~ condition_for_matchedness, data=joined_df))

library(gplots)
attach(mtcars)
plotmeans(`sum(kwh)` ~ condition_for_matchedness, xlab="Condition",
          ylab="KWH", main="Group Means with 95% CI", data = joined_df, connect = F)

kwh = scale(joined_df$`sum(kwh)`)
thi = scale(joined_df$weighted_thi)
customers = scale(joined_df$`sum( customers)`)
condition = joined_df$condition_for_matchedness
city = joined_df$MunicipalCityCode
month = joined_df$month

r = lmer(kwh ~ condition + (1 | thi) + ( 1 | customers) + ( 1 | month ) + (1 | city))
summary(r)

c = coef(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list(array = c(0.22596, 0.22755, 0.22921, 0.22921), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

wave1 = joined_df[joined_df$Start == "07/20/17",]
kwh = scale(wave1$`sum(kwh)`)
thi = scale(wave1$weighted_thi)
customers = scale(wave1$`sum( customers)`)
condition = wave1$condition_for_matchedness
city = wave1$MunicipalCityCode
month = wave1$month

r = lmer(kwh ~ condition + (1 | thi) + ( 1 | customers) + ( 1 | month ) + (1 | city))
summary(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list(array = c(0.30095, 0.30508, 0.30509, 0.30506), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")

wave2 = joined_df[joined_df$Start == "08/01/17",]
kwh = scale(wave2$`sum(kwh)`)
thi = scale(wave2$weighted_thi)
customers = scale(wave2$`sum( customers)`)
condition = wave2$condition_for_matchedness
city = wave2$MunicipalCityCode
month = wave2$month

r = lmer(kwh ~ condition + (1 | thi) + ( 1 | customers) + ( 1 | month ) + (1 | city))
summary(r)
plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list(array = c(0.345669, 0.345660, 0.335925, 0.345671), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")
