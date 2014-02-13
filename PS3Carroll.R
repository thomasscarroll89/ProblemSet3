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
  x %*% Beta + rnorm(20, mean=25, sd=10)
}
#Apply our function to each 20-by-5 matrix of the Data array; set MARGIN equal to 3 to run this function
#along the third dimension of the array. 
Y <- apply(Data, MARGIN=3, FUN=beta.fun)
dim(Y) #shows the dimensions are 20 by 1 by 1000 

####Problem 3####
function.test <- function(i){ #create a function to extract the coefficients from a linear regression,
  #regressing each column of Y on its respected 20-by-5 X matrix.  
  coef(lm(Y[,i] ~ Data[,,i]))
}

coefficient.estimates <- sapply(1:1000, function.test) #run the function 1000 times

####Problem 4####
#Use the density() function to calculate the data needed to plot the density graphs. Do this for all 6 
#beta coefficients. 
intercept.density <- density(coefficient.estimates[1,])
x1.density <- density(coefficient.estimates[2,])
x2.density <- density(coefficient.estimates[3,])
x3.density <- density(coefficient.estimates[4,])
x4.density <- density(coefficient.estimates[5,])
x5.density <- density(coefficient.estimates[6,])

#plot each density object
plot(intercept.density)
plot(x1.density)
plot(x2.density)
plot(x3.density)
plot(x4.density)
plot(x5.density)

####Problem 5####
t.statistic.extraction <- function(i){ #create function that extracts the t-values from a summary of an lm object
  summary(lm(Y[,i] ~ Data[,,i]))$coefficients[,3]
}

t.statistics <- sapply(1:1000, t.statistic.extraction) #run the function 1000 times

####Problem 6####
#Each regression has 20 observations and 6 parameters are being estimated, so there are 14 degrees of 
#freedom. 

cutoff <- qt(0.975, df=14) #Tells us that out t-statistic should be at least 2.145 to claim the 
#result is significant at the 0.05 level

function.sum <- function(x){ #selfmade function which calculates the total number of times that an element
  #in a vector (x) is greater than or equal to some cutoff number (calculated
  #above). 
  sum(x >= cutoff)
}

apply(t.statistics, MARGIN=1, FUN=function.sum) #apply this function to get the total number of times our
#t statistic is statistically significant

#As we can see from the output, the intercept and the first, second, and fourth variables are all statistically 
#significant, meaning that for almost all 1000 regressions they had a t statistic greater than the cutoff. 
#Thus the output from apply shows totals around 975 (give or take) for all 4 of these variables. However
#the 3rd and 5th variables were almost never significantly different from 0. This makes sense since the 
#beta vector we used to generate out y values had the 3rd and 5th coefficients set to 0, so they shouldn't 
#be calculated as being different from 0 most of the time. 


####Problem 7####
#The following code was run on a MAC, rather than my PC. 
library('doMC')
library('multicore')
library('foreach')
library(plyr)

#I choose to replicate my code where I extract the t statistics
system.time(lapply(1:1000, t.statistic.extraction)) #returns an elapsed time of 1.303.

registerDoMC(cores=4)
system.time(laply(1:1000, t.statistic.extraction, .parallel=TRUE)) #returns an elapsed time of 0.765

#Thus overall we appear to save 1.303 - 0.765 = 0.538 seconds. 

####Page 2####

####Part 1####
OutOfStep <- read.table("https://pages.wustl.edu/montgomery/incumbents2.txt")
rng <- sample(0:1, size=6687, replace=TRUE)
training <- OutOfStep[which(rng==1),]
testing <- OutOfStep[which(rng==0),]

model1 <- lm(voteshare ~ chalspend + incspend, data=training)
summary(model1)$coefficients
data1 <- cbind(1, testing$chalspend, testing$incspend)
prediction1 <- data1 %*% coef(model1) 

model2 <- lm(voteshare ~ chalspend + incspend + urban + age65 + milpop + unemployed, data=training)
summary(model2)$coefficients
data2 <- cbind(1, testing$chalspend, testing$incspend, testing$urban, testing$age65, testing$milpop, testing$unemployed)
prediction2 <- data2 %*% coef(model2)

model3 <- lm(voteshare ~ chalspend + incspend + urban + unemployed + south + incparty + inparty, data=training)
summary(model3)$coefficients
data3 <- cbind(1, testing$chalspend, testing$incspend, testing$urban, testing$unemployed, testing$south, testing$incparty, testing$inparty)
prediction3 <- data3 %*% coef(model3)

####Part 2####
#The following function calculates the 6 statistics from the homework: RMSE, MAD, RMSLE, MAPE, MEAPE, and MRAE. 
#It takes 3 arguments: outcomes is a vector of length n containing the true values for a particular outcome
#variable. Predictions is an n-by-k matrix, where each column contains the predicted values from a different model.
#Naive.forecast is a vector of length n containing the naive estimates of the outcome. 

model.statistics <- function(outcomes, predictions, naive.forecast){
  n <- nrow(predictions)
  k <- ncol(predictions)
  
  #calculate absolute error, e (i.e. the residuals)
  function1 <- function(x){ #create a function to calculate RMSE for a single vector
    abs(outcomes.test - x)
  }
  abs.error <- apply(predictions, 2, function1) #apply the function to predictions matrix by column
  #abs.error is an n-by-k matrix containing absolute error; each column corresponds to a different model
  
  #calculate absolute percentage error, a
  function2 <- function(i){
    (abs.error[,i]/abs(outcomes))*100
  }
  abs.percent.error <- sapply(1:k, function2)
  #abs.percent.error is an n-by-k matrix containing absolute percentage error; each column corresponds to a different model
  
  #calculate vector b, the difference between naive estimates and outcome
  b <- abs(outcomes - naive.forecast)
  
  #RMSE FORMULA
  rmse.calculation <- function(x){
    sqrt(sum(na.omit(x)^2)/n)
  } 
  rmse <- apply(abs.error, 2, rmse.calculation)
  
  #MAD FORMULA
  mad.calculation <- function(x){
    median(na.omit(x))
  }
  mad <- apply(abs.error, 2, mad.calculation)
  
  #RMSLE FORMULA
  rmsle.calculation <- function(x){
    temp <- cbind(outcomes, x)
    temp <- na.omit(temp)
    sqrt((sum((log(temp[,2] + 1) - log(temp[,1] + 1))^2))/n)
  }
  rmsle <- apply(predictions, 2, rmsle.calculation)
  
  #MAPE FORMULA
  mape.calculation <- function(x){
    sum(na.omit(x))/n
  }
  mape <- apply(abs.percent.error, 2, mape.calculation)
  
  #MEAPE FORMULA
  meape.calculation <- function(x){
    median(na.omit(x))
  }
  meape <- apply(abs.percent.error, 2, meape.calculation)
  
  #MRAE FORMULA
  mrae.calculation <- function(x){
    temp <- cbind(b, x)
    temp <- na.omit(temp)
    median(temp[,2]/temp[,1])
  }
  mrae <- apply(abs.error, 2, mrae.calculation)
  
  #Creating the output 
  output <- cbind(rmse, mad, rmsle, mape, meape, mrae)
  return(output)
}

####Part 3####
model.statistics2 <- function(outcomes, predictions, naive.forecast=NULL, RMSE=TRUE, MAD=TRUE, 
                              RMSLE=TRUE, MAPE=TRUE, MEAPE=TRUE, MRAE=TRUE){
  n <- nrow(predictions)
  k <- ncol(predictions)
  
  #calculate absolute error, e (i.e. the residuals)
  function1 <- function(x){ #create a function to calculate RMSE for a single vector
    abs(outcomes.test - x)
  }
  abs.error <- apply(predictions, 2, function1) #apply the function to predictions matrix by column
  #abs.error is an n-by-k matrix containing absolute error; each column corresponds to a different model
  
  #calculate absolute percentage error, a
  function2 <- function(i){
    (abs.error[,i]/abs(outcomes))*100
  }
  abs.percent.error <- sapply(1:k, function2)
  #abs.percent.error is an n-by-k matrix containing absolute percentage error; each column corresponds to a different model
  
  #calculate vector b, the difference between naive estimates and outcome
  if(is.null(naive.forecast)==FALSE){ #tests to make sure that naive.forecast was specified in the 
    #arguments; otherwise the next line of code wouldn't run
    b <- abs(outcomes - naive.forecast)
  }
  
  #RMSE FORMULA
  rmse.calculation <- function(x){
    sqrt(sum(na.omit(x)^2)/n)
  } 
  rmse <- apply(abs.error, 2, rmse.calculation)
  
  #MAD FORMULA
  mad.calculation <- function(x){
    median(na.omit(x))
  }
  mad <- apply(abs.error, 2, mad.calculation)
  
  #RMSLE FORMULA
  rmsle.calculation <- function(x){
    temp <- cbind(outcomes, x)
    temp <- na.omit(temp)
    sqrt((sum((log(temp[,2] + 1) - log(temp[,1] + 1))^2))/n)
  }
  rmsle <- apply(predictions, 2, rmsle.calculation)
  
  #MAPE FORMULA
  mape.calculation <- function(x){
    sum(na.omit(x))/n
  }
  mape <- apply(abs.percent.error, 2, mape.calculation)
  
  #MEAPE FORMULA
  meape.calculation <- function(x){
    median(na.omit(x))
  }
  meape <- apply(abs.percent.error, 2, meape.calculation)
  
  #MRAE FORMULA
  if(is.null(naive.forecast)==FALSE){ #tests to make sure that naive.forecast was specified in the arguments
    mrae.calculation <- function(x){
      temp <- cbind(b, x)
      temp <- na.omit(temp)
      median(temp[,2]/temp[,1])
    }
    mrae <- apply(abs.error, 2, mrae.calculation)
  }  
  
  #Creating the output 
  output <- matrix(NA, nrow=k)
  row.names(output) <- paste(rep("Model",k), 1:k)
  
  if(RMSE==TRUE){
    output <- cbind(output, rmse)
  }
  if(MAD==TRUE){
    output <- cbind(output, mad)
  }
  if(RMSLE==TRUE){
    output <- cbind(output, rmsle)
  }
  if(MAPE==TRUE){
    output <- cbind(output, mape)
  }
  if(MEAPE==TRUE){
    output <- cbind(output, meape)
  }
  if(MRAE==TRUE & is.null(naive.forecast)==FALSE){
    output <- cbind(output, mrae)
  }
  
  if(ncol(output)>1){ #checks to make sure that output was updated at least once (i.e. at least one of the
    # six statistics was calculated and added to the output matrix). If so, it drops the 
    #first column, which was just a bunch of NAs. It then returns the output. 
    output <- output[,-1]
    return(output)
  } else(warning("No statistics were calculated. Set one of the statistics arguments to TRUE."))
  
}

####Part 4####
#We need to define our vector of naive forecasts, which the homework tells us is simply a lagged observation. 
#So we create this vector by simply dropping the last observation for voteshare in the testing dataset, and 
#then adding NA to the beginning of this vector. 
naive.temp <- testing$voteshare[-3344]
naive <- c(NA, naive.temp)

#Next we run the function
predict.matrix <- cbind(prediction1, prediction2, prediction3) #create a matrix of predictions for all 3 models

#First, example calculating all 6 statistics
model.statistics2(outcomes=testing$voteshare, predictions=predict.matrix, naive.forecast=naive)

#Second, example where baseline model is not provided (i.e. MRAE is set to FALSE)
model.statistics2(outcomes=testing$voteshare, predictions=predict.matrix, naive.forecast=naive, 
                  MRAE=FALSE)

#Third, example where none of the statistics are calculated
model.statistics2(outcomes=testing$voteshare, predictions=predict.matrix, naive.forecast=naive, 
                  RMSE=FALSE, MAD=FALSE, RMSLE=FALSE, MAPE=FALSE, MEAPE=FALSE, MRAE=FALSE)
