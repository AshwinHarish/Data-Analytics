dataset = read.csv("Datasets/Marks.csv")
df = data.frame(dataset$m1, dataset$m2, dataset$m3)
df
colnames(df) = c("m1", "m2", "m3")
X = Y = X_bar = Y_bar = data.frame()
SLR = function(x, y){
    if(x==1){
        X_bar = mean(df$m1)
        X = df$m1
    }
    if(x==2){
        X_bar = mean(df$m2)
        X = df$m2
    }
    if(x==3){
        X_bar = mean(df$m3)
        X = df$m3
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

    num = sum((X-X_bar)*(Y-Y_bar))
    den = sum((X-X_bar)^2)
    b1 = num/den

    b0 = Y_bar - b1*X_bar
    b0
    b1

    pred_Y = b0 + b1*X
    Y
    pred_Y

    plot(X, Y, col = "red")
    lines(X, pred_Y, col = "blue")
    
    data = data.frame(X,Y,pred_Y)
    colnames(data) = c("X","Y_Actual","Y_Predicted")
    data
    
}

SLR(1,2)
SLR(1,3)
SLR(2,1)
SLR(2,3)
SLR(3,1)
SLR(3,2)
