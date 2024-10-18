### Q3 
library(MASS)
library(tidyverse)
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
  # Train model 
  trained_reg <- training(true_beta, var_cov, ntrain = ntrain)
  
  # Test sampling from the same dist (but different seed obv)
  test_reg <- testing(true_beta, var_cov, bhat_ols = trained_reg$bhat_ols, bhat_ridge = trained_reg$bhat_ridge, ntest = ntest)
  
  # Return a 2xn matrix where 1st row OLS, 2nd row Ridge 
  return(c(test_reg$TE_test_ols, test_reg$TE_test_ridge))
}


get_test_error <- function(d){
  # Parameters 
  true_beta = matrix(1, nrow = d, ncol = 1)
  var_cov = diag(d)
  ntrain = 100
  ntest = 500
  total_rounds = 1000 # should be 1000
  
 
  # Run train_test_func 1000x
  # Saving testing errors into a matrix 
  testing_err <- replicate(total_rounds, train_test_func(true_beta, var_cov, ntrain = ntrain, ntest = ntest))
  
  
  # Summing over rows to get OLS and Ridge Risk Error 
  risk_ddim <- rowSums(testing_err)/ncol(testing_err)
  
  return(risk_ddim)

}


################# Implementation ################# 

# Define the different dimensions I want to iterate over 
d_elements <- c(1, seq(10, 90, by = 10), 98)

risk_mat <- sapply(d_elements, get_test_error)

dim_risk_df <- data.frame(t(rbind(d_elements, risk_mat)))

colnames(dim_risk_df) <- c("Dimension", "OLS_Risk", "Ridge_Risk")



dim_risk_df %>% pivot_longer(cols = ends_with("Risk"), 
                             names_to = "Type", 
                             values_to = "Test_Error") %>% 
  ggplot(aes(x = Dimension, y = Test_Error, color = Type)) + 
    geom_point(size = 2) + 
    geom_line(size = 1) + 
    labs(title = "OLS vs Ridge Risk Error", y = "Risk") + 
    theme(plot.title = element_text(hjust = 0.5, size = 20))









