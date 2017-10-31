df = read.csv("/Users/dominicburkart/Documents/princeton/energy project/energy_analysis/summer_data.csv")

#david this is absolutely absurd. i feel like a neanderthal. a cromagnon. a denisovan.
library(sqldf)

condnames <- unique(df$Condition)
            

x = aov(avg.power.use ~ Condition, data=df)
summary(x)

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
print(condnames)
