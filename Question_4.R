dataset = read.csv("Datasets/Advertising.csv")
df = data.frame(dataset$TV, dataset$Sales)
df
colnames(df) = c("TV", "Sales")

#without predefined functions

TV = mean(df$TV)
Sales = mean(df$Sales)

num = sum((TV - df$TV)*(Sales - df$Sales))
den = sum((TV - df$TV)^2)
b1 = num/den

b0 = Sales - b1*TV
b0
b1

df$pred_Sales = b0 + b1*df$TV
df

plot(df$TV, df$Sales, col = "red")
lines(df$TV, df$pred_Sales, col = "blue")

#using prebuilt functions

model=lm(Sales~TV,data=df)
test=data.frame(TV=df$TV)
df$pred=predict(model,test)
plot(df$TV,df$Sales)
lines(df$TV,df$pred)
model