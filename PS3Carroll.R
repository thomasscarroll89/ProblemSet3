#Tommy Carroll
#Problem Set 3

####Problem 1####
#Dimensions are 20 by 5 by 1000, so we need 20*5*1000 = 100,000 random numbers. I use the rnorm function 
#to generate these numbers, rounding to the nearest whole number. 
Data <- array(round(rnorm(100000, mean=500, sd=100), 0), dim=c(20,5,1000))

####Problem 2####
Beta <- matrix(c(1,2,0,4,0), ncol=1)

#Create a function to multiply a data matrix by the vector of betas.  
beta.fun <- function(x){
  x %*% Beta + rnorm(1, mean=25, sd=10)
}
#Apply our function to each 20-by-5 matrix of the Data array; set MARGIN equal to 3 to run this function
#along the third dimension of the array. 
Y <- apply(Data, MARGIN=3, FUN=beta.fun)
dim(Y) #shows the dimensions are 20 by 1 by 1000 

####Problem 3####
function.test <- function(i){
  coef(lm(Y[,i] ~ Data[,,i]))
}

coefficient.estimates <- sapply(1:1000, function.test)

####Problem 4####
intercept.density <- density(coefficient.estimates[1,])
x1.density <- density(coefficient.estimates[2,])
x2.density <- density(coefficient.estimates[3,])
x3.density <- density(coefficient.estimates[4,])
x4.density <- density(coefficient.estimates[5,])
x5.density <- density(coefficient.estimates[6,])

plot(intercept.density)
plot(x1.density)
plot(x2.density)
plot(x3.density)
plot(x4.density)
plot(x5.density)

####Problem 5####
t.statistic.extraction <- function(i){
  summary(lm(Y[,i] ~ Data[,,i]))$coefficients[,3]
}

t.statistics <- sapply(1:1000, t.statistic.extraction)

####Problem 6####
