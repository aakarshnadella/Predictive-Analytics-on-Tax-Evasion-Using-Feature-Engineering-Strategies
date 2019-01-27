
#RFE Algorithm

set.seed(7)
library(mlbench)
library(caret)
data1 <- read.csv("new.csv")
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(data1[-(1:2),1:29], data1[-(1:2),30], sizes = c(1:29), rfeControl = control)
print(results)
predictors(results)
plot(results, type = c("g","o"))
