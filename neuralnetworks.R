# Set a seed
set.seed(500)
library(MASS)
data <- new3

# Check that no data is missing
apply(data,2,function(x) sum(is.na(x)))

# Train-test random splitting 
index <- sample(52:nrow(data),)

# min-max scale
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Train-test random splitting 
train_ <- scaled[index,]
test_ <- scaled[-index,]
library(neuralnet)

# NN training
n <- names(train_)
f<-as.formula(paste("property.tax ~", paste(n[!n %in% "property.tax"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),threshold=0.001,linear.output=F,stepmax=1e6,err.fct="sse", algorithm="backprop",act.fct = "tanh", learningrate=0.01)

# Visual plot of the model
plot(nn)
print(nn)

# Predict
valnet <- compute(nn,test_[,1:5])

# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- valnet$net.result*(max(data2$property.tax)-min(data2$property.tax))+min(data2$property.tax)
test.r <- (test_$property.tax)*(max(data2$property.tax)-min(data2$property.tax))+min(data2$property.tax)

# Plot predictions
par(mfrow=c(1,2))

plot(test$property.tax,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')


# Neural net cross validation
set.seed(450)
cv.error <- NULL
k <- 10

# Initialize progress bar
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
    index <- sample(1:nrow(data),round(0.9*nrow(data)))
    train.cv <- scaled[index,]
    test.cv <- scaled[-index,]
    
    nn <- neuralnet(f,data=train_,hidden=c(5,3),threshold=0.001,linear.output=F,stepmax=1e6,err.fct="sse", algorithm="backprop",act.fct = "tanh", learningrate=0.01)

    
    pr.nn <- compute(nn,test.cv[,1:5])
    pr.nn <- pr.nn$net.result*(max(data$property.tax)-min(data$property.tax))+min(data$property.tax)
    
    test.cv.r <- (test.cv$property.tax)*(max(data$property.tax)-min(data$property.tax))+min(property.tax)
    
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    
    pbar$step()
}


