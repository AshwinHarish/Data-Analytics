dataset = read.csv("Datasets/Advertising.csv")
df = data.frame(dataset$TV, dataset$Radio, dataset$Newspaper, dataset$Sales)
df
colnames(df) = c("TV", "Radio", "Newspaper", "Sales")

#without predefined functions

TV = mean(df$TV)
Radio = mean(df$Radio)
Newspaper = mean(df$Newspaper)
Sales = mean(df$Sales)

num1 = sum((TV - df$TV)*(Sales - df$Sales))
den1 = sum((TV - df$TV)^2)
b1 = num1/den1

num2 = sum((Radio - df$Radio)*(Sales - df$Sales))
den2 = sum((Radio - df$Radio)^2)
b2 = num2/den2

num3 = sum((Newspaper - df$Newspaper)*(Sales - df$Sales))
den3 = sum((Newspaper - df$Newspaper)^2)
b3 = num3/den3

b0 = Sales - b1*TV - b2*Radio - b3*Newspaper
b0
b1
b2
b3

df$pred_Sales = b0 + b1*df$TV +b2*df$Radio +b3*Newspaper
df

#using prebuilt functions

model=lm(Sales~TV+Radio+Newspaper,data=df)
test=data.frame(TV=df$TV, Radio=df$Radio, Newspaper=df$Newspaper)
df$pred=predict(model,test)
model
df