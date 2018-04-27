rm(list=ls())
library(sqldf)
library(lmerTest)
library(plotly)
library(MuMIn)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months_interaction.csv")

# All periods outside of the months are labeled control
x = nov22data$Condition[1] #known Nan
c = nov22data$Condition
condition_imputed = replace(c, c==x, "Co")
df = data.frame(nov22data,condition_imputed)
df$Treated = replace(df$Treated, df$Treated==unique(df$Treated)[3], "No")
df = df[df$customers > 0,]
df = nov22data[nov22data$customers > 0,]

conts = c("customers", "kwh", "thi")
cats = c("Treated", "condition_imputed", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")
cats = c("Treated", "condition", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")


sql_cmd = paste("select ", paste(cats, collapse = ", "), ", avg(", paste(conts, collapse = "), avg("), "), sum(", paste(conts, collapse="), sum("), ") from df group by MunicipalCityCode, psegMonth")
munidf = sqldf(sql_cmd)

#cool! but we want to have the average THI for each muni/month combo weighted by the number of customers read.
for_weighted = c("thi", "route") #from df
weight_by_route = c("customers")
out_names = c("weighted_thi", "weighted_route")
munidf[, out_names] = NA
for (m in unique(df$psegMonth)){
  for (t in unique(df$MunicipalCityCode)){
    tdf = df[df$psegMonth == m & df$MunicipalCityCode == t, c(for_weighted, weight_by_route)]
    for (w in 1:length(for_weighted)) {
        munidf[munidf$psegMonth == m & munidf$MunicipalCityCode == t, out_names[w]]  = sum(tdf[,for_weighted[w]] * tdf[,weight_by_route] / sum(tdf[,weight_by_route]))
    }
  }
}
# ^ this is an atrocity, but let's move on. We want the number of days for each muni/month.

#munidf = munidf[munidf$Condition != "Co",]

#munidf = munidf[munidf$`sum( customers)` > 0,]

#munidf = munidf[munidf$Start == "08/01/17",]

munidf = munidf[order(munidf$Condition),]
kwh = scale(munidf$`sum(kwh)`) #effect is smaller but significant with logged data
thi = scale(munidf$weighted_thi)
customers = scale(munidf$`sum( customers)`)

treated = munidf$Treated
condition = munidf$condition_imputed
condition = munidf$Condition
month = munidf$month
city = munidf$MunicipalCityCode

r = lmer(kwh ~ treated * condition + thi + ( 1 | city) + (1 | month))
summary(r)
plot(resid(r), col = condition, main="Residuals for unweighted analysis (post interaction analysis)")
plot(r)

r = lmer(kwh ~ thi + ( 1 | city))

plot(resid(r), col = condition, main="Residuals for unweighted analysis (pre interaction analysis)")

length(resid(r)) == length(city)

munidf$res = resid(r)

library(ggplot2)
ggplot(munidf,aes(x=res,group=Condition,fill=Condition))+
  geom_histogram(position="identity",alpha=0.25,binwidth=0.025)+theme_bw()

library('lattice')
histogram(~ res | Condition, data = munidf)

plotmeans(res ~ Condition, data = munidf, main="Residuals of Weighted Model")

condition = relevel(condition, "Pr")
r = lmer(kwh ~ treated * condition + thi + ( 1 | city) + (1 | month))
summary(r)


r = lmer(kwh ~ treated * condition + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
plot(resid(r), col = condition, main="Residuals for unweighted analysis (post interaction analysis)")


c = coef(r)
r_month = r




r = lmer(kwh ~ treated + customers +  thi + ( 1 | city) + (1 | month))

r.squaredGLMM(r) # 0.00492265 0.99101013 

condition = relevel(condition, "Co")
cust_weights = (customers - ( min(customers) - 1) ) / (max(customers) - (min(customers) - 1)) # the -1 subtraction avoids the smallest value being transformed to zero
r = lmer(kwh ~ treated * condition + thi + ( 1 | city), weights = cust_weights)
summary(r)
munidf$res = resid(r)

condition = relevel(condition, "Tr")
r = lmer(kwh ~ treated * condition + thi + ( 1 | city) + (1 | month), weights = cust_weights)
summary(r)

condition = relevel(condition, "Pr")
r = lmer(kwh ~ treated * condition + thi + ( 1 | city) + (1 | month), weights = cust_weights)
summary(r)

condition = relevel(condition, "Te")
r = lmer(kwh ~ treated * condition + thi + ( 1 | city) + (1 | month), weights = cust_weights)
summary(r)


plot(r)
c = coef(r)
r_month = r

r.squaredGLMM(r) # 0.005261858 0.999908720 

plot_ly(
  x = c("Social Proof", "Temporal", "Prospection", "Trust"),
  y = c(coef(r)$city$conditionSo[1],coef(r)$city$conditionTe[1], coef(r)$city$conditionPr[1], coef(r)$city$conditionTr[1]),
  error_y = list( array = c(0.04013, 0.03774, 0.03746, 0.03662), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")


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

#approach temporal effect analyses
during = munidf[munidf$psegMonth %in% c("Aug2017", "Sep2017"),]
before = munidf[! munidf$psegMonth %in% c("Aug2017", "Sep2017"),]
during$time = "during"
before$time = "before"
munidf = rbind(during, before)

kwh = scale(log(munidf$`sum(kwh)`))
thi = scale(munidf$weighted_thi)
customers = scale(munidf$`sum( customers)`)

treated = munidf$Treated
condition = munidf$condition_imputed
month = munidf$month
city = munidf$MunicipalCityCode
time = munidf$time

r = lmer(kwh ~ condition + time + ( 1 | customers) + ( 1 | thi) + ( 1 | city) + (1 | month))
summary(r)
plot(r)

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
condition1 = wave1$Condition
month1 = wave1$month
city1 = wave1$MunicipalCityCode

r = lmer(kwh1 ~ treated1 * condition1 + customers1 + thi1 + ( 1 | city1) + (1 | month1))
summary(r)

condition1 = relevel(condition1, "So")
r = lmer(kwh1 ~ treated1 * condition1 + customers1 + thi1 + ( 1 | city1) + (1 | month1))
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
condition2 = wave2$Condition
month2 = wave2$month
city2 = wave2$MunicipalCityCode

r = lmer(kwh2 ~ treated2 * condition2 +  customers2 +  thi2 + ( 1 | city2) + (1 | month2))
summary(r)

condition2 = relevel(condition2, "Tr")
r = lmer(kwh2 ~ treated2 * condition2 +  customers2 +  thi2 + ( 1 | city2) + (1 | month2))
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

gnames = c("Control Wave 1", "Prospection Wave 1", "Social Wave 1", "Temporal Wave 1", "Trust Wave 1", "Control Wave 2", "Prospection Wave 2", "Social Wave 2", "Temporal Wave 2", "Trust Wave 2")
kwh = scale(joined_df$`sum(kwh)`)
thi = scale(joined_df$weighted_thi)
customers = scale(joined_df$`sum( customers)`)
condition = interaction(joined_df$condition_for_matchedness, joined_df$Start)
levels(condition) = gnames
city = joined_df$MunicipalCityCode
month = joined_df$month
wave = joined_df$Start


r = lmer(kwh ~ condition + (1 | thi) + ( 1 | customers) + ( 1 | month ) + (1 | city))
summary(r)
coefs = coef(r)
plot_ly(
  x = gnames,
  y = coefs$city$
  error_y = list(array = c(), color = '#000000'),
  name = "Treatment Effects",
  type = "bar")



# short tail formatting ---------------------------------------------------

rm(list=ls())
library(sqldf)
library(lmerTest)
library(plotly)
library(MuMIn)
library(dplyr)
library(gplots)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/nov22data_naive_months_interaction.csv")

# All periods outside of the months are labeled control
x = nov22data$Condition[1] #known Nan
c = nov22data$Condition
condition_imputed = replace(c, c==x, "Co")
df = nov22data[nov22data$customers > 0,]

conts = c("customers", "kwh", "thi")
cats = c("Treated", "condition_imputed", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")
cats = c("Treated", "condition", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")

sql_cmd = paste("select ", paste(cats, collapse = ", "), ", avg(", paste(conts, collapse = "), avg("), "), sum(", paste(conts, collapse="), sum("), ") from df group by MunicipalCityCode, psegMonth")
munidf = sqldf(sql_cmd)

#cool! but we want to have the average THI for each muni/month combo weighted by the number of customers read.
for_weighted = c("thi", "route") #from df
weight_by_route = c("customers")
out_names = c("weighted_thi", "weighted_route")
munidf[, out_names] = NA
for (m in unique(df$psegMonth)){
  for (t in unique(df$MunicipalCityCode)){
    tdf = df[df$psegMonth == m & df$MunicipalCityCode == t, c(for_weighted, weight_by_route)]
    for (w in 1:length(for_weighted)) {
      munidf[munidf$psegMonth == m & munidf$MunicipalCityCode == t, out_names[w]]  = sum(tdf[,for_weighted[w]] * tdf[,weight_by_route] / sum(tdf[,weight_by_route]))
    }
  }
}

short = inner_join(munidf[munidf$year == "2017",], munidf[munidf$year == "2016",], by = c("month", "MunicipalCityCode"))

antis1 = anti_join(munidf[munidf$year == "2017",], munidf[munidf$year == "2016",], by = c("month", "MunicipalCityCode"))
antis2 = anti_join(munidf[munidf$year == "2016",], munidf[munidf$year == "2017",], by = c("month", "MunicipalCityCode"))

short = short[order(short$Condition.x),]

unique(short$month)

customers = short$`sum( customers).x`
short$cust_weights = (customers - ( min(customers) - 1) ) / (max(customers) - (min(customers) - 1)) # the -1 subtraction avoids the smallest value being transformed to zero
short$cust_weights = short$cust_weights * 2 # deal with gradient issue: tiny weights break the lmer function.
short$Condition.x = relevel(short$Condition.x, "Co")

#Analysis B
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)
plot(resid(r), col = short$Condition.x, main="Residuals for linear covariate analysis B")
plot(r)
plotmeans(resid(r) ~ short$Condition.x, data = munidf, main="Residuals for linear covariate analysis B")

#Analysis A
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 | MunicipalCityCode), data=short, weights = cust_weights)
summary(r)
plot(resid(r), col = short$Condition.x, main="Residuals for linear covariate analysis A")
plot(r)
plotmeans(resid(r) ~ short$Condition.x, data = munidf, main="Residuals for linear covariate analysis A")

#Analysis C
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)
plot(resid(r), col = short$Condition.x, main="Residuals for linear covariate analysis C")
plot(r)
plotmeans(resid(r) ~ short$Condition.x, data = munidf, main="Residuals for linear covariate analysis C")

#Analysis D
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)
plot(resid(r), col = short$Condition.x, main="Residuals for linear covariate analysis D")
plot(r)
plotmeans(resid(r) ~ short$Condition.x, data = munidf, main="Residuals for linear covariate analysis D")


short$Condition.x = relevel(short$Condition.x, "Pr")

#Analysis B
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis A
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 | MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis C
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)

#Analysis D
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)


short$Condition.x = relevel(short$Condition.x, "Te")

#Analysis B
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis A
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 | MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis C
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)

#Analysis D
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)


short$Condition.x = relevel(short$Condition.x, "Tr")

#Analysis B
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis A
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 | MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis C
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)

#Analysis D
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)

short$Condition.x = relevel(short$Condition.x, "So")

#Analysis B
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis A
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 | MunicipalCityCode), data=short, weights = cust_weights)
summary(r)

#Analysis C
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + (scale(`sum(kwh).y`) + scale(`weighted_thi.x`) |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)

#Analysis D
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`sum(kwh).y`) + scale(`weighted_thi.x`) + ( 1 |  MunicipalCityCode) + (1 | month), data=short, weights = cust_weights)
summary(r)



# "plot everything"  ----------------------------------------------------

library(gplots)
#Significant difference between temporal and prospection conditions (prospection higher)
plotmeans(scale(short$`sum(kwh).x`)  ~ short$Condition.x, xlab="Condition",
          ylab="KWH", main="Group Means with 95% CI (Just 2017 kW/h)", connect = F)

#Control above 0, all other scores at or below 0
plotmeans(scale(short$`sum(kwh).x`) - scale(short$`sum(kwh).y`)  ~ short$Condition.x, xlab="Condition",
          ylab="KWH", main="Group Means with 95% CI (2017 minus 2016)", connect = F)

#Some influential towns
plotmeans(scale(short$`sum(kwh).x`) - scale(short$`sum(kwh).y`)  ~ short$MunicipalCityCode, xlab="Condition",
          ylab="KWH", main="Delta Municipality Power with 95% CI (2017 minus 2016)", connect = F)

#resequence chronologically and plot
m_order = c(8, 6, 14, 1, 16, 12, 10, 3, 20, 19, 18,5)
n_order = c(9, 7, 15, 2, 17, 13, 11, 4, 21)
o = c(m_order, n_order)

#less power used than usual this year during intervention period.
munidf$psegMonth = factor(munidf$psegMonth, levels(munidf$psegMonth)[o])
plotmeans(scale(munidf$`sum(kwh)`)  ~ munidf$psegMonth, xlab="Condition",
          ylab="KWH", main="Monthly Power with 95% CI (2017 and 2016)", connect = F)

plotmeans(scale(munidf$`sum(thi )`)  ~ munidf$psegMonth, xlab="Condition",
          ylab="Standardized THI", main="Monthly Hot Weather with 95% CI (2017 and 2016)", connect = F)


