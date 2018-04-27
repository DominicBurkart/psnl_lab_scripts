rm(list=ls())
library(sqldf)
library(lmerTest)
library(plotly)
library(MuMIn)
library(dplyr)
library(gplots)
library(coefplot)
nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/feb9data_naive_months_interaction.csv")
#nov22data = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/feb9data_naive_months.csv")
# All periods outside of the months are labeled control
x = nov22data$Condition[1] #known Nan
c = nov22data$Condition
nov22data$condition_imputed = replace(c, c==x, "Co")
df = nov22data[nov22data$customers > 0,]
bad_df= df[!complete.cases(df),] #full of route 22s with kwh per customer values WAY above norm. Contacting Malcolm.
df = df[df$route != "22",]
df = na.omit(df)

conts = c("customers", "kwh", "thi")
cats = c("Treated", "condition_imputed", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start")
#cats = c("Treated", "condition", "Condition", "year", "month", "MunicipalCityCode", "psegMonth", "Start", "Tripartite")

sql_cmd = paste("select ", paste(cats, collapse = ", "), ", avg(", paste(conts, collapse = "), avg("), "), sum(", paste(conts, collapse="), sum("), ") from df group by MunicipalCityCode, psegMonth")
munidf = sqldf(sql_cmd)
munidf = munidf[complete.cases(munidf),]

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

#reorder the factors for months in the chronological order (doesn't change correspondence with other data)
short$month = factor(short$month, levels(short$month)[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)])

#add three months datafield (August, September, October)
short$threemonths = NA
short[short$month %in% c("Aug", "Sep", "Oct", "Nov", "Dec"), "threemonths"] = "after"
short[short$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"), "threemonths"] = "before"
short = na.omit(short)

short$threemonths = relevel(as.factor(short$threemonths), "before")

# NO interaction + NO 2016 + weighted
r = lmer(scale(`sum(kwh).x`) ~  condition_imputed.x + scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="POST interaction + NO 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="POST NO interaction + NO 2016 + weighted", connect = T)
r = lmer(scale(`sum(kwh).x`) ~ scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), data=short, weights = cust_weights)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="PRE interaction + NO 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="PRE NO interaction + NO 2016 + weighted")
plot(resid(r), col = short$Condition.x, main="NO interaction + NO 2016 + weighted")

# NO interaction + 2016 + weighted
r = lmer(scale(`sum(kwh).x`) ~  condition_imputed.x + scale(`weighted_thi.x`) + scale(`sum(kwh).y`)+ (1 |  MunicipalCityCode), data=short)#, weights = cust_weights)
summary(r)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="POST NO interaction + 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="POST NO interaction + 2016 + weighted", connect = T)
r = lmer(scale(`sum(kwh).x`) ~ scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), data=short, weights = cust_weights)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="PRE NO interaction + 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="PRE NO interaction + 2016 + weighted")
plot(resid(r), col = short$Condition.x, main="NO interaction + 2016 + weighted")

# interaction + NO 2016 + weighted
contrasts(short$threemonths) = c(-1, 1)

r = lmer(log(`sum(kwh).x`) ~  threemonths * Condition.x + scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), 
         data=short, weights = cust_weights)
summary(r)
plot(fitted(r) ~ short$`sum(kwh).x`)
plot(resid(r) ~ fitted(r))
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="POST interaction + NO 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="POST interaction + NO 2016 + weighted", connect = T)
r = lmer(scale(`sum(kwh).x`) ~ scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), data=short, weights = cust_weights)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="PRE interaction + NO 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="PRE interaction + NO 2016 + weighted")
plot(resid(r), col = short$Condition.x, main="interaction + NO 2016 + weighted")

# interaction + 2016 + weighted
r = lmer(scale(`sum(kwh).x`) ~  Treated.x * Condition.x + scale(`weighted_thi.x`) + scale(`sum(kwh).y`)+ (1 |  MunicipalCityCode), data=short, weights = cust_weights)
summary(r)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="POST interaction + 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="POST interaction + 2016 + weighted", connect = T)
r = lmer(scale(`sum(kwh).x`) ~ scale(`weighted_thi.x`)  + (1 |  MunicipalCityCode), data=short, weights = cust_weights)
plotmeans(resid(r) ~ interaction(short$Condition.x, short$threemonths), data = munidf, main="PRE interaction + 2016 + weighted", connect = F)
plotmeans(resid(r) ~ short$month, data = munidf, main="PRE interaction + 2016 + weighted")
plot(resid(r), col = short$Condition.x, main="interaction + 2016 + weighted")
