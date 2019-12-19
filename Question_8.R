library(MASS)
df = read.csv("Datasets/Bank.csv")
df$def = gsub("N",0,df$def)
df$def = gsub("Y",1,df$def)
df$def = as.integer(df$def)
df
index = sample(1:nrow(df), round(nrow(df)*0.8))
df_train = df[index,]
df_test = df[-index,]
model = qda(def~cred_card+balance, data = df_train)
summary(model)
pred = predict(model, newdata = df_test)
pred$class
df_test$def
table(df_test$def, pred$class)
