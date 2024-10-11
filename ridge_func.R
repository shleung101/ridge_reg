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
  Y = X %*% true_beta + eps
  
  
  return(dim(Y))

  #### Calculate Beta Estimates #### 
  
}


var_cov1 <- matrix(c(2, 1, 1, 2), nrow = 2) 
bet1 <- matrix(c(4, 5), nrow = 2)

training(bet1, var_cov1, 1, 1, 100)


