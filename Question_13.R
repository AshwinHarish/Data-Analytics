dataset = read.csv("Datasets/Marks.csv")
df = data.frame(dataset$m1, dataset$m2, dataset$m3)
df
colnames(df) = c("m1", "m2", "m3")
X1 = X2 = Y = X1_bar = X2_bar = Y_bar = data.frame()
MLR = function(x1, x2,  y){
    if(x1==1){
        X1_bar = mean(df$m1)
        X1 = df$m1
    }
    if(x1==2){
        X1_bar = mean(df$m2)
        X1 = df$m2
    }
    if(x1==3){
        X1_bar = mean(df$m3)
        X1 = df$m3
    }
    if(x2==1){
        X2_bar = mean(df$m1)
        X2 = df$m1
    }
    if(x2==2){
        X2_bar = mean(df$m2)
        X2 = df$m2
    }
    if(x2==3){
        X2_bar = mean(df$m3)
        X2 = df$m3
    }
    if(y==1){
        Y_bar = mean(df$m1)
        Y = df$m1
    }
    if(y==2){
        Y_bar = mean(df$m2)
        Y = df$m2
    }
    if(y==3){
        Y_bar = mean(df$m3)
        Y = df$m3
    }

    num1 = sum((X1-X1_bar)*(Y-Y_bar))
    den1 = sum((X1-X1_bar)^2)
    b1 = num1/den1

    num2 = sum((X2-X2_bar)*(Y-Y_bar))
    den2 = sum((X2-X2_bar)^2)
    b2 = num2/den2

    b0 = Y_bar - b1*X1_bar - b2*X2_bar
    b0
    b1
    b2

    pred_Y = b0 + b1*X1 + b2*X2
    Y
    pred_Y
    
    data = data.frame(X1,X2,Y,pred_Y)
    colnames(data) = c("X1","X2","Y_Actual","Y_Predicted")
    data
    
}

MLR(1,2,3)
MLR(2,3,1)
MLR(1,3,2)
