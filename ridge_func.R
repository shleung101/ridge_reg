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

training <- function(true_beta, var_cov, sigma2, lambda, ntrain){
  
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
  set.seed(69)
  
  # Draw training samples + save in X matrix   
  X <- mvrnorm(ntrain, mu = matrix(0, nrow = nrow(var_cov), ncol = 1),  Sigma = var_cov)
 
  
  # Draw sigma2 
  eps <- rnorm(ntrain, mean = 0, sd = sqrt(sigma2))
  
  # Calculate Y vector 
  Y <- X %*% true_beta + eps
  
  


  #### Calculate Beta Estimates #### 
  
  bhat_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  bhat_ridge <- solve(t(X) %*% X + lambda*diag(nrow(var_cov))) %*% t(X) %*% Y
  
  
  
  #### Calculate Training Error #### 
  
  # OLS Training Error 
  TE_ols <- (1/ntrain)*sum((Y - X %*% bhat_ols)^2)
  
  # Ridge Training Error 
  TE_ridge <- (1/ntrain)*sum((Y - X %*% bhat_ridge)^2)
  
  
  train_output <- list("bhat_ols" = bhat_ols, "bhat_ridge" = bhat_ridge, "X_mat" = X, "Y_mat" = Y, "TE_ols" = TE_ols, "TE_ridge" = TE_ridge)
  
  
  return(train_output)
}


var_cov1 <- matrix(c(2, 1, 1, 2), nrow = 2) 
bet1 <- matrix(c(4, 5), nrow = 2)

asdf <- training(bet1, var_cov1, 1, 1, 100)


# Y1 <- asdf$Y_mat 
# X1 <- asdf$X_mat
# b_ols <- asdf$bhat_ols
# 
# (1/100)*sum((Y1 - X1 %*% b_ols)^2)





