df<-read.csv("Datasets/Marks.csv")
df
summary(df)
m1<-45
m2<-35

df$dis<-sqrt((m1-df$m1)^2+(m2-df$m2)^2)
df
df<-df[order(df$dis),]
df
k<-6
nn<-head(df,k)
t<-names(which(table(nn$Grade)==max(table(nn$Grade))))
cat("grade:",t[[1]][1])
