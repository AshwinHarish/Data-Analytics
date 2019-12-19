dataset = read.csv("Datasets/dataset_faculty.csv")
df = data.frame(dataset$experience, dataset$programs, dataset$publications)
df
colnames(df) = c("experience", "programs", "publications")

#without predefined functions

experience = mean(df$experience)
publications = mean(df$publications)
programs = mean(df$programs)

num1 = sum((publications - df$publications)*(experience - df$experience))
den1 = sum((experience - df$experience)^2)
b1 = num1/den1

num2 = sum((publications - df$publications)*(programs - df$programs))
den2 = sum((programs - df$programs)^2)
b2 = num2/den2

b0 = publications - b1*experience - b2*programs
b0
b1
b2

df$pred_publication = b0 + b1*df$experience + b2*df$programs
df

#using prebuilt functions

test=data.frame(experience=df$experience,programs=df$programs)
model=lm(publications~experience+programs,data=df)
df$pred = predict(model, newdata=test)
df