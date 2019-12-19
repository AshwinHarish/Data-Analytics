df = read.csv("Datasets/Bank.csv")
df$def = gsub("N",0,df$def)
df$def = gsub("Y",1,df$def)
df$def = as.integer(df$def)
df
index = sample(1:nrow(df), round(nrow(df)*0.8))
df_train = df[index,]
df_test = df[-index,]
model = glm(def~cred_card+balance, data = df_train, family = "binomial")
df$pred = predict(model,type="response", newdata = df_test)
df
