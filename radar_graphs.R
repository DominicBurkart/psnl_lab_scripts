# Library
library(fmsb)

vs = list( c(.05, -.2, 0.40, 0.15, 0.1, -0.22), #man to man
           c(.05, -.15, .4, .1,.1,-.2), #woman to woman
           c(.03, -.22, .5, -.1, .05, -.25), #woman to man
           c(.04, -.18, .3, .1, .06, -.19), #man to woman
           c(.2, .22, -.1, .15, .10, .22), #friend
           c(-0.05, -.3, .3, .05, -.11, -.2), #stranger
           c(-.2, -.14, -.2, .23, -.3, -.4), #online
           c(.3, .2, .4, -.2, 0.3, -.4)) #offline
ts = list("Man to Man", "Woman to Woman", "Woman to Man", "Man to Woman", "Friend", "Stranger", "Online Chat", "Face-to-Face")
# Create data: note in High school for Jonathan:
for (i in seq(length(vs))){
  data=as.data.frame(matrix( vs[[i]], ncol=6))
  colnames(data)=c("Social Values", "Intimacy", "Inanity", "Individual Values", "Self-Time", "Social Invisibility")
  data=rbind(rep(0.5,10) , rep(-0.5,10) , data)
  radarchart(data, title = ts[[i]])
}


# The default radar chart proposed by the library:
radarchart(read.csv("/Users/dominicburkart/Downloads/sharing potential results - friend.csv"), title = "Friend")
f = read.csv("/Users/dominicburkart/Downloads/sharing potential results - friend.csv")

df = data.frame()
colnames(df) = c("Social Values", "Intimacy", "Inanity", "Individual Values", "Self-Time", "Social Invisibility")

