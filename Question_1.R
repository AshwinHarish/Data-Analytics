dataset = read.csv("Datasets/dataset_faculty.csv")
df = data.frame(dataset$experience, dataset$publications)
df
colnames(df) = c("experience", "publications")

#without predefined functions

experience = mean(df$experience)
publications = mean(df$publications)

num = sum((publications - df$publications)*(experience - df$experience))
den = sum((experience - df$experience)^2)
b1 = num/den

b0 = publications - b1*experience
b0
b1

df$pred_publication = b0 + b1*df$experience
df

plot(df$experience, df$publications, col = "red")
lines(df$experience, df$new_publications, col = "blue")

#using prebuilt functions

model=lm(publications~experience,data = df)
test=data.frame(experience=df$experience)
df$pred=predict(model,test)
plot(test$experience,df$publications,col="blue")
lines(test$experience,df$pred,col="red")