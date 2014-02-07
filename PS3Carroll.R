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
dim(Y)

Y.test <- array(Y, dim=c(20, 1, 1000))
testing <- merge(Y.test, Data)

dim(Y.test)
dim(Data)


function1 <- function(x){
  x[,]
}

temp <- apply(Data, MARGIN=3, function1)

merge(Y, Data, all=TRUE)

library(abind)
abind(Data, Y)
Data[1:20,1:5,]
apply(Data, MARGIN=3, )

apply(Data, FUN=[,,])
summary(lm(Y[,1] ~ Data[,,1]))
lm(Y[,3] ~ Data[,,3])
Y
Data
Data2 <- merge(Data[,,], Y)
Data3 <- cbind(Y[,1], Data[,,1])
lapply(Data, function(i) x[,,1])

function3 <- function(x, x1, x2, x3, x4, x5){
  coef(lm(x ~ x1 + x2 + x3 + x4 + x5))
}

apply(Y, MARGIN=2, function3, x1=Data[,1,1], x2=Data[,2,1], x3=Data[,3,1], x4=Data[,4,1], x5=Data[,5,1])

apply(Y, MARGIN=2, FUN=lm, formula=Y ~ Data[,1,1] + Data[,2,1] + Data[,3,1] + Data[,4,1] + Data[,5,1])
x <- Data[,1,1]
y <- Data[,]

