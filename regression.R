t <- rep(0,length(z))
for (i in 1:length(z)) t[i] = mean(z[-i]) 
jke <- mean(t)
n <- length(z)
jkv <- (n-1)/n * sum( (t-jke)^2 ) 
jkbce <- n * mean(z) - (n-1) * jke

dff <- data.frame(x.motor.fuels.tax, x.property.tax)
dff <- data.frame(scale(dff)) 


#holdout <- sample (1:21, 7, replace = F)


    
    #Getting an output vector to store RMSE on each of the b iterations
    output <- rep(0,21)
		
    #The loop for repeated iterations
    for(i in 1:21){

        #Getting the observations for the holdout sample
        holdout <- sample(1:1071,357,replace=F)
        
        #Fitting the model on the training dataset
        fit.train <- lm(dff[,1] ~ dff[,2] ,newdata=dff[-holdout,])
        
        #Getting the predicted values for the test dataset
        predict.test <- predict(fit.train,newdata=dff[holdout,])

        #Getting resid^2 for the test dataset
        resid2 <- ((dff[holdout,2] - predict.test)^2
        
        #Computing RMSE and placing result into output vector
        output[i] <- sqrt(mean(resid2))
    }

output
rmse <- mean(output)
rmse






for(i in 1:21)
  {
  
  #Getting the observations for the holdout sample
  holdout <- sample(1:21,7,replace=F)
  
  #Fitting the model on the training dataset
  fit.train <- lm(y ~ x.motor.fuels.tax ,data=dff[-holdout,])
  
  #Getting the predicted values for the test dataset
  predict.test <- predict(fit.train,newdata=dff[holdout,])
  
  #Getting resid^2 for the test dataset
  resid2 <- ((dff[holdout,2] - predict.test))^2
             
             #Computing RMSE and placing result into output vector
            output[i] <- sqrt(mean(resid2))
}

