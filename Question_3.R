data <- read.csv('Datasets/loan_dataset.csv')
train = data[1:nrow(data)-1,]
test = data[nrow(data),]
k = 3

distances = c()
train$dist = sqrt((train$cred_card-test[1,2])^2 + (train$balance-test[1,3])^2)
train <- train[order(train$dist),]

count = 0
for(i in 1:k){
  if(train[i, 4]=='Y') 
    count = count + 1 
  else 
    count = count - 1
}

if(count>=0) print("Yes") else print("No")
