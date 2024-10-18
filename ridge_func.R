library(MASS)

############## Training Function ############ 
# Let k = # of covariates 
# Let n = # of observations 

# need dimensions 

# # Inputs: 
# - beta    (kx1)
# - cov_var (kxk)
# - sigma2  (nx1) 
# - lambda  (scalar)
# - ntrain = n

training <- function(true_beta, var_cov, sigma2 = 1, lambda = 1, ntrain){
  
  #### Check for dimensions #### 
  
  # Matching k 
  if(nrow(true_beta)!= ncol(var_cov) ) {
    stop("Error: Dimensions of covariates don't match.")
  }
  
  # Checking for pd var_cov matrix
  if (!is.matrix(var_cov) || nrow(var_cov) != ncol(var_cov)) {
    stop("Error: The matrix must be square.")
  }
  
  tryCatch({
    chol(var_cov)}, 
    error = function(e) {
      message(paste("Error: Matrix is not positive definite."))
      return(NA)}    )
  
  
  
  
  #### Draw training samples #####
  
  # Draw training samples + save in X matrix   
  X_train <- mvrnorm(ntrain, mu = matrix(0, nrow = nrow(var_cov), ncol = 1),  Sigma = var_cov)
 
  
  # Draw sigma2 
  eps <- rnorm(ntrain, mean = 0, sd = sqrt(sigma2))
  
  # Calculate Y vector 
  Y_train <- X_train %*% true_beta + eps
  
  


  #### Calculate Beta Estimates #### 
  
  bhat_ols <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% Y_train
  
  bhat_ridge <- solve(t(X_train) %*% X_train + lambda*diag(nrow(var_cov))) %*% t(X_train) %*% Y_train
  
  
  
  #### Calculate Training Error #### 
  
  # OLS Training Error 
  TE_ols <- (1/ntrain)*sum((Y_train - X_train %*% bhat_ols)^2)
  
  # Ridge Training Error 
  TE_ridge <- (1/ntrain)*sum((Y_train - X_train %*% bhat_ridge)^2)
  
  
  train_output <- list("bhat_ols" = bhat_ols, "bhat_ridge" = bhat_ridge, "X_train_mat" = X_train, "Y_train_mat" = Y_train, "TE_train_ols" = TE_ols, "TE_train_ridge" = TE_ridge)
  
  
  return(train_output)
}





############### Testing Function ################

testing <- function(true_beta, var_cov, sigma2 = 1, bhat_ols, bhat_ridge, ntest){
  
  ##### Draw testing sample ##### 
  # Draw training samples + save in X matrix   
  X_test <- mvrnorm(ntest, mu = matrix(0, nrow = nrow(var_cov), ncol = 1),  Sigma = var_cov)
  
  
  # Draw sigma2 
  eps <- rnorm(ntest, mean = 0, sd = sqrt(sigma2))
  
  # Calculate Y vector 
  Y_test <- X_test %*% true_beta + eps
  
  
  ##### Calculate testing error using beta estimates ##### 
  
  # OLS Testing Error 
  TE_test_ols <- (1/ntest)*sum((Y_test - X_test %*% bhat_ols)^2)
  
  # Ridge Testing Error 
  TE_test_ridge <- (1/ntest)*sum((Y_test - X_test %*% bhat_ridge)^2)
  
  
  test_output <- list("TE_test_ols" = TE_test_ols, "TE_test_ridge" = TE_test_ridge)
  
  return(test_output)
}




##################### LOOCV Functions ####################### 

loocv_error <- function(X_test, Y_test, lambda){
  
  # LOOCV Test observations is of size 1 
  test_x <- X_test[i, drop = FALSE]
  train_y <- Y_test[i]
    
  
  # LOOCV Train observation is of size n-1 
  train_x <- X_test[-i, drop = FALSE]
  train_y <- Y_test[-i]
  
  loocv_bhat <- solve(t(train_x) %*% train_x + lambda %*% diag(nrow(train_x))) %*% t(train_x) %*% train_y
  
  # Predicted Value of the 1 observation left out 
  y1_hat <- test_x1 %*% loocv_bhat
  
  # Calculating Error 
  error <- test_y1 - y1_hat
  
  return(error)
  
}





lambda_calc <- function(true_beta, var_cov, sigma2 = 1, ntest){
  
  ##### Draw testing sample ##### 
  # Draw training samples + save in X matrix   
  X_test <- mvrnorm(ntest, mu = matrix(0, nrow = nrow(var_cov), ncol = 1),  Sigma = var_cov)
  
  
  # Draw sigma2 
  eps <- rnorm(ntest, mean = 0, sd = sqrt(sigma2))
  
  # Calculate Y vector 
  Y_test <- X_test %*% true_beta + eps
  
  
  
  ###### Finding Lambda ######
  # Search over lambda grid, find the most optimal 
  lambda_grid <- 10^seq(-4, 4, length = 100)
  
  # Create empty vector that stores LOOCV Error for each lambda 
  lambda_loocv_err <- numeric(100)
  
  for (lambda in lambda_grid) {
    
    # Num of Iterations for LOOCV
    total_iter <- 1:n 
    error <- sapply(total_iter, loocv_error, X_test = X_test, Y_test = Y_test, lambda = lambda)
    
    loocv_err <- sum(error)/ntest
    
    lambda_loocv_err[i] <- loocv_err
  }

  
}








X_train 



lambda_grid <- 10^seq(-4, 4, length = 100)





var_cov1 <- matrix(c(2, 1, 1, 2), nrow = 2) 
bet1 <- matrix(c(4, 5), nrow = 2)

asdf <- training(bet1, var_cov1, 1, 1, 100)


# Y1 <- asdf$Y_mat 
# X1 <- asdf$X_mat
# b_ols <- asdf$bhat_ols
# 
# (1/100)*sum((Y1 - X1 %*% b_ols)^2)

testing(bet1, var_cov1, 1,asdf$bhat_ols, asdf$bhat_ridge, 100)

