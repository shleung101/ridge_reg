### Q3 
library(MASS)
source("ridge_func.R")

# # Dimension 
# d <- 1 
# true_beta <- matrix(1, nrow = d, ncol = 1)
# var_cov <- diag(d)
# sigma2 <- 1 
# lambda <- 1 
# 
# ntrain <- 100 
# ntest <- 100 


train_test_func <- function(true_beta, var_cov, ntrain, ntest){
  trained_reg <- training(true_beta, var_cov, ntrain)
  
  test_reg <- testing(true_beta, var_cov, trained_reg$bhat_ols, trained_reg$bhat_ridge, ntest)
  
  # Return a 2xn matrix where 1st row OLS, 2nd row Ridge 
  return(c(test_reg$TE_test_ols, test_reg$TE_test_ridge))
}


get_test_error <- function(d){
  # Parameters 
  true_beta = matrix(1, nrow = d, ncol = 1)
  var_cov = diag(d)
  ntrain = 100
  ntest = 100
  
  
  # Setting up ntest number of rounds 
  rounds <- matrix(NA, nrow = ntest, ncol = 1) 
  # rounds <- matrix(1:ntest, nrow = ntest, ncol = 1) 
  
  
  
  ###### ISSUE: Cannot feed in arguments correctly, sapply not detecting the arguments
  
  # Saving testing errors into a matrix 
  testing_err <- sapply(rounds, train_test_func, 
                        true_beta = matrix(1, nrow = d, ncol = 1), 
                        var_cov = diag(d), 
                        ntrain = 100, 
                        ntest = 100)
  
  # Summing over rows to get OLS and Ridge Risk Error 
  risk_ddim <- rowSums(testing_err)/ntest
  
  return(risk_ddim)

}


### need to iterate over each dimension d 

# Save d in dimension vector 
# Call training
# Save beta values 
# Feed beta estimates in testing 


d_elements <- c(1, seq(10, 90, by = 10), 98)
dim_vec <- matrix(d_elements)
risk_vec <- matrix(NA, nrow = nrow(dim_vec), ncol = 1)


sapply(d_elements, get_test_error)







# Testing for d=1 


rounds <- matrix(1:ntest, nrow = ntest, ncol = 1) 
testing_err <- sapply(rounds, train_test_func)
risk_ddim <- rowSums(testing_err)/ntest

# Save in Risk vector 








########## Debug 
d=2
true_beta = matrix(1, nrow = d, ncol = 1)
var_cov = diag(d)
ntrain = 100
ntest = 100


# Setting up ntest number of rounds 
rounds <- matrix(NA, nrow = ntest, ncol = 1) 
# rounds <- matrix(1:ntest, nrow = ntest, ncol = 1) 


train_test_func(true_beta, 
                var_cov , 
                ntrain = 100, 
                ntest = 100)

## mvnorm has issues 


###### ISSUE: Cannot feed in arguments correctly, sapply not detecting the arguments

# Saving testing errors into a matrix 
testing_err <- sapply(rounds, train_test_func, 
                      true_beta = matrix(1, nrow = d, ncol = 1), 
                      var_cov = diag(d), 
                      ntrain = 100, 
                      ntest = 100)











