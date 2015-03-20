#Linear Regression
#This function takes your data and outputs a linear regression matrix
#using the Moore-Penrose Pseudoinverse. For a mathematical explanation,
#see C.M. Bishop's 'Pattern Recognition and Machine Learning' Ch. 3.1
#The output matrix is a value to assign to each variable, including a
#constant in position 1.
#**********************************************************************
#Install package to get pseudoinverse function
install.packages('corpcor')
library(corpcor)
#Make design matrices for each selection
#Make target vector T
Get_Wml <- function (target, variables){
  T <- as.matrix(target)
  V <- as.matrix(variables)
  var_len <- dim(V)[1]
  #Calculate Wml
  W0 <- replicate(var_len,1)
  phi <- cbind(W0, V)
  pseudoinverse(phi) %*% T
}
#As an example, your output could look this this:
[1,]  0.05667593
[2,] -3.74628572
[3,]  0.28119794
#In which case, your model would look lik this:
Model <- function(data) 0.05667593 -3.74628572*data$V1 +0.28119794*data$V2


