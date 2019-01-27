R code
Install and load the gptk package
# Load in the required libraries for data manipulation and multivariate normal distribution
require(MASS)
require(plyr)
require(reshape2)
require(ggplot2)
set.seed(12345)
#Normalize of data
tax<- read.csv(“data.csv”)
attach(tax)
pt.mean<- mean(pt)
pt.sd <- sd(pt)
year.mean<- mean(Year)
year.sd <- sd(Year)
tax<- within(tax, pt<- (pt - pt.mean)/pt.sd)
tax<- within(tax, Year <- (Year - year.mean)/year.sd)
detach(tax)
train<- tax[2:20,]
test<- tax[1,]
xtrain<- train$Year
ytrain<- train$pt
xtest<- test$Year
ytest<- test$pt
# Calculate the covariance matrix sigma using a simplified version of the squared exponential function.
# Parameters:
#	X1, X2 = vectors
# 	l = the scale length parameter

# Returns a covariance matrix
calcSigma<- function(X1,X2,l=1) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
for (i in 1:nrow(Sigma)) {
for (j in 1:ncol(Sigma)) {
Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
return(Sigma)
}
# 1. Plot some sample functions from the Gaussian process
# Define the points at which we want to define the functions
x.star<- seq(-2,2,len=50)

# Calculate the covariance matrix
sigma<- calcSigma(x.star,x.star)
# Generate a number of functions from the process
n.samples<- 3
values<- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  # each column represents a sample from a multivariate normal distribution with zero mean and covariance sigma
values[,i] <- mvrnorm(1, rep(0, length(x.star)), sigma)
}
values<- cbind(x=x.star,as.data.frame(values))
values<- melt(values,id="x")

# Plot the result
fig2a <- ggplot(values,aes(x=x,y=value)) +
geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
geom_line(aes(group=variable)) +
theme_bw() +
scale_y_continuous(lim=c(-2.5,2.5), name="output, f(x)") +
xlab("input, x")
f <- data.frame(xtrain,ytrain)

# Calculate the covariance matrices using the same x.star values as above
k1 <- calcSigma(xtrain, xtrain)
k3 <- calcSigma(xtrain, x.star)
k2 <- calcSigma(x.star, xtrain)
k4 <- calcSigma(x.star, x.star)
sigma.n<- 0.1

# Recalculate the mean and covariance functions
f.bar.star<- k2%*%solve(k1 + sigma.n^2*diag(1, ncol(k1)))%*%f$ytrain
cov.f.star<- k4 - k2%*%solve(k1 + sigma.n^2*diag(1, ncol(k1)))%*%k3

# Recalculate the sample functions
values<- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
values[,i] <- mvrnorm(1, f.bar.star, cov.f.star)
}
values<- cbind(x=x.star,as.data.frame(values))
values<- melt(values,id="x")
graph<- ggplot(values, aes(x=x,y=value,colour=variable,group=variable)) + geom_line()
