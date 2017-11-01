library(sqldf)
library(lme4)
library(rdd)

df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/summer_data.csv")

condnames <- unique(df$Condition)
x = aov(avg.power.use ~ Condition, data=df)
summary(x) # F of 2.754 P of 0.0275 due to outliers.

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

RDestimate(avg.power.use ~ pseg.month | cdd, data=df2)


