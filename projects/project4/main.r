## Benjamin Capon - S2746256
## Tom Davies - S2210543
## Cyrus Seyrafi - S2766504
## The code was completed collaboratively mainly during workshops with all 3 team
## members consistently present and contributing ideas, pushing to Github on Tom's
## laptop (to avoid git conflicts). All team members pulled their weight by making
## further contributions to the code in their own time at home when necessary. Coding,
## commenting/editing, and breaking down the problems were done in different ratios
## within the group but the time committed to the project and overall proportion of
## contributions are seen as equal amongst group members.
## This script implements a Linear Mixed Model, given by the formula y = Xβ + Zb + ε.
## The model follows a similar format to the standard linear model (y = Xβ + ε),
## but will include the Zb term to incorporate fixed but random effects. b will
## contain the random effects, following a normal distribution with mean 0 and
## covariance matrix φ_θ parameterised by an unknown vector parameter θ, whilst Z is a
## model matrix for b. The idea behind this extra term is that we can now account
## for clustering of data or in other words, there is randomness at different levels
## within the data we can now account for.
## The LMMsetup function produces the basic components of a linear mixed model,
## represented by the formula y = Xβ + Zb + ε. N.B. the function will return
## the basic components for a linear model by default if no ref is supplied.
##
## Inputs:
##  form: the formula used for the fixed effects part of the model (y ~ Xβ + ε).
##  dat: the data frame containing all variables needed in the model.
##  ref: the list of vectors of variable names identifying the random effects part
##       of the model.
##
## Outputs:
##  X: the design matrix for fixed effects.
##  y: the response vector.
##  qrZ: the QR decomposition of random effects design matrix Z.
##  lengths: the number of columns in each fixed effects block.
LMMsetup <- function(form, dat, ref = list()) {
  # Construct fixed effects design matrix X.
  X <- model.matrix(form, dat)
  # Extract response vector y from the model data frame. Store as 1 column matrix
  # for convenience in calculations and operations.
  y <- model.response(model.frame(form, dat))
  y <- matrix(y, ncol = 1)
  # If no random effects, return X and y for linear model as there will be no Zb.
  if (length(ref) == 0) {
    return(list(X = X, y = y, qrZ = NULL, lengths = 0))
  } else {
    # Otherwise, initialise empty random effects design matrix.
    Z <- matrix(NA, nrow(dat), 0)
  }
  # Also initialise a lengths vector where for each random effect vector in ref,
  # we add a block to Z.
  lengths <- rep(0, length(ref))
  # Iterate through each random effects vector in ref.
  for (i in 1:length(ref)) {
    # Construct a string for the linear model formula, and parse it with as.formula.
    # e.g. c("a", "b", "c") -> "~ a:b:c-1" -> ~ a:b:c-1
    # The "-1" allows us to exclude the intercept from our effect formula.
    effect_formula <- as.formula(paste(c("~ ", paste(ref[[i]], collapse = ":"), "-1"), collapse = ""))
    # Assemble this block of the random effects design matrix Z.
    block <- model.matrix(effect_formula, dat)
    # Assign the number of columns in the block to the lengths vector.
    lengths[i] <- ncol(block)
    # Bind this block to random effects design matrix Z.
    Z <- cbind(Z, block)
  }
  # Determine dimensions of random effects design matrix Z (an n x p matrix).
  n <- length(y)
  p <- sum(lengths)
  # If p > n, stop since n must be larger than p in our assumptions. Print an
  # error message and stop the code running.
  if (p >= n) {
    stop("n must be larger than p!")
  }
  # Finally, take the qr decomposition of Z and return it in a list alongside
  # all the other useful objects that we can avoid repeating calculations on
  # when fitting a linear mixed model.
  qrZ <- qr(Z)
  return(list(X = X, y = y, qrZ = qrZ, lengths = lengths))
}
## The CholSolve function solves a system of the form Ax = y using Cholesky decomposition.
## This method, while still O(n**3) time if we have to perform the decomposition,
## has a smaller constant of proportionality than solve, and is thus more optimal.
##
## Inputs:
##  A: a symmetric positive definite matrix.
##  y: the vector of response variables.
##  R_chol: an (optional) Cholesky factor representing the upper triangular Cholesky
##     factor of the decomposition of A (A = R^(T)R) if available.
##
## Outputs:
##  x: the vector which is to be solved for (such that Ax = y).
##  R_chol: the upper triangular Cholesky factor of A, saved for future use.
CholSolve <- function(A, y, R_chol = NULL) {
  # Unless provided, calculate upper-triangular Cholesky factor R such that A = R^(T)R.
  if (is.null(R_chol)) {
    R_chol <- chol(A)
  }
  # Solve R^(T)z = y for z, and then Rx = z for x, using efficient triangular matrix methods.
  x <- backsolve(R_chol, forwardsolve(t(R_chol), y))
  # Return vector x and Cholesky factor R.
  return(list(x = x, R_chol = R_chol))
}
## Here we write a function, WMult, which will efficiently compute W multiplied
## with x. W is given by (ZψZ^(T) + Iσ**2)^-1 and is a part of both
## terms in the log likelihood for theta and beta.
## When taking the QR decomposition of Z, this becomes
## W = Q [(RψR^(T) + Iσ**2)^-1  &  0  \\
##              0               &   I_{n-p} * sigma^-2      ] Q^T
## We can take advantage of this structure to compute Wx efficiently
## Inputs:
##  x: the vector we want to multiply W by.
##  qrZ: the qr decomposition of Z.
##  sigma2: the variance in the residuals.
##  n: the number of rows in W.
##  p: the number of columns in W.
##  R_chol: the cholesky factorisation of RψR^(T) + Iσ**2
##
## Outputs:
##  y: the vector which is solved for (such that Wx = y).
WMult <- function(x, qrZ, sigma2, n, p, W_chol) {
  # Compute (Q^T)x by using qr.qty() which multiplies x by the transpose of QN
  Qtx <- qr.qty(qrZ, x)
  # Compute the inverse using the passed in W_chol - this is from the top
  # left quadrant
  WXL <- CholSolve(NULL, Qtx, R_chol = W_chol)
  # Initialise full n × ncol(X) matrix - this will become our out vector
  Wx <- matrix(0, n, ncol(x))
  # Fill in first p rows from the upper left quadrant we computed above
  Wx[1:p, ] <- WXL$x
  # Fill in remaining rows with the block from the bottom right quadrant of W
  Wx[(p + 1):n, ] <- Qtx[(p + 1):n, ] / sigma2
  # Compute Q Wx, for the final output
  Wx <- qr.qy(qrZ, Wx)
  return(Wx)
}
## The function NoEffectLMMprof calculates the log-likelihood of the model for when
## there are no random effects. That is, is calculates the log-likelihood of the
## normal linear regression model.
##
## Inputs:
##  theta: the vector of log-standard deviations of the residuals and random effects.
##  RSS: the evaluated (y-Xβ)^T(y-Xβ), to avoid repetition.
##  n: the number of rows of data we have.
##
## Outputs:
##  logLik: the log-likelihood, excluding constant terms.
NoEffectLMMprof <- function(theta, RSS, n) {
  # theta is the log of standard deviation so perform the inverse to get sigma**2:
  sigma2 <- exp(2 * theta)
  # The log-ikelihood of the normal linear regression model is - (y-Xβ)^T(y-Xβ)/2σ**2
  # - (n/2)ln(σ**2) - (n/2)ln(2π). The last term can be dropped as it's a constant
  # so calculate this as logLik and return it:
  logLik <- -RSS / (2 * sigma2) - n * log(sigma2) / 2
  return(logLik)
}
## The function LMMprof evaluates the negative log-likelihood of the model for a
## given set of log standard deviations (the vector θ), and stores the corresponding
## estimates of the fixed effects parameters β as an attribute. We aim to evaluate
## the logLik function: −(y − Xβ)^(T)(Zψθ ZT + Iσ2)^(−1)(y − Xβ)/2 − log|Zψθ ZT + Iσ2|/2
## in the most efficient way possible, especially in the n >> p case.
##
## Inputs:
## theta: the vector of log-standard deviations of the residuals and random effects.
## y: the response vector.
## X: the design matrix for fixed effects.
## qrZ: the QR decomposition of the random effects design matrix Z.
## lengths: number of random effects variables in each entry of ref.
##
## Outputs:
## logLik: the negative log-likelihood of the model for the given θ, with
## beta, the vector of fixed effects parameter estimates β, stored as an attribute.
LMMprof <- function(theta, y, X, qrZ, lengths) {
  # Compute the dimensions of n x p random effects design matrix Z.
  p <- sum(lengths)
  n <- length(y)
  # Calculate the variance of the residuals (σ**2) from theta[1] = log(σ).
  sigma2 <- exp(2 * theta[1])
  # Construct random effects covariance matrix ψ from remaining values in θ where
  # ψ is a diagonal matrix; random effects are independent.
  # N.B. theta[-1] are the log-standard deviations of the random effects.
  psi <- diag(rep(exp(2 * theta[-1]), lengths))
  # Combine random effects covariance matrix ψ and residual variance to
  # construct reduced covariance matrix of model.
  matrix_log <- qr.R(qrZ) %*% psi %*% t(qr.R(qrZ)) + diag(sigma2, p)
  # Compute Cholesky factorisation of covariance matrix (R^(T)R = S).
  matrix_chol <- chol(matrix_log)
  # Compute half the logarithm of the determinant of the reduced covariance matrix.
  second_term <- sum(log(diag(matrix_chol))) + (n - p) * log(sigma2) / 2
  # Compute W-weighted version of design matrix X which has useful attributes we
  # can use.
  WX <- WMult(X, qrZ, sigma2, n, p, matrix_chol)
  # It is important we estimate fixed effects parameters β using Cholesky
  # decomposition. To do this, we will use the CholSolve to solve for β given
  # that X^(T)WXβ = X^(T)Wy given that we have all these terms now.
  XtWy <- crossprod(X, WMult(y, qrZ, sigma2, n, p, matrix_chol))
  beta <- CholSolve(crossprod(X, WX), XtWy)$x
  # Calculate the first term in the log likelihood, optimising the efficiency using
  # our WMult function.
  first_term <- crossprod((y - X %*% beta), WMult(y - X %*% beta, qrZ, sigma2, n, p, matrix_chol))
  # Calculate negative log-likelihood of the model for the given θ.
  logLik <- -first_term / 2 - second_term
  # Store fixed effects parameter estimates β as an attribute of logLik.
  attr(logLik, "beta") <- beta
  # Return logLik.
  return(logLik)
}
## The lmm function esimates model parameters β and θ for a linear mixed model
## of the form y = Xβ + ZB + ε.
##
## Inputs:
## form: the formula used for the fixed effects part of the model (y ~ Xβ + ε).
## dat: the data frame containing all variables needed in the model.
## ref: the list of vectors of variable names specifying the random effects for
##      the random effects part of the model.
##
## Outputs:
## beta: the vector of fixed effects parameter estimates β.
## theta: the vector of log-standard deviations of the residuals and random effects.
lmm <- function(form, dat, ref = list()) {
  # Initialise the basic components of our linear mixed model: X, y, arZ, and lengths.
  setup <- LMMsetup(form, dat, ref)
  X <- setup$X
  y <- setup$y
  qrZ <- setup$qrZ
  lengths <- setup$lengths
  ## If there are no residual effects, default to a standard linear model.
  if (is.null(qrZ)) {
    # Estimate fixed effects parameters β using Cholesky decomposition.
    beta <- CholSolve(crossprod(X), crossprod(X, y))$x
    # Compute residuals y - Xβ.
    residuals <- y - X %*% beta
    # Compute the sum of squared residuals outside of our optimisation function
    # to avoid repeat calculations of this ((y-Xβ)^T(y-Xβ))
    RSS <- crossprod(residuals)
    # Initialise log-standard deviation of residuals as 0.
    theta_init <- 0
    # Set upper and lower bounds for θ optimisation, this should be more than enough
    # and we will assume θ must lie in this region.
    lower <- -50
    upper <- 50
    # Optimise θ with respect to the NoEffectLMMprof function.
    opt_result <- optim(
      par = theta_init,
      # Note: Negative since optim minimises.
      fn = function(theta) -as.numeric(NoEffectLMMprof(theta, RSS, nrow(X))),
      upper = upper,
      lower = lower,
      # Set precision and iteration limits, precision needs to be smaller than
      # machine precision as it will stop too soon if we don't include this
      method = "Brent", # Brent's method for scalar optimisation.
    )
    # Store the optimised θ and logLik.
    theta_opt <- opt_result$par
    logLik_opt <- NoEffectLMMprof(theta_opt, RSS, nrow(X))
    ## If there are random effects, use linear mixed model.
  } else {
    # Initialise θ with length number of random effects + 1 (for residuals).
    theta_init <- rep(0, length(ref) + 1)
    # Optimise θ with respect to the LMMprof function.
    opt_result <- optim(
      par = theta_init,
      # Note: Negative since optim minimises.
      fn = function(theta) -as.numeric(LMMprof(theta, y, X, qrZ, lengths)),
      # Set precision and iteration limits, precision needs to be smaller than
      # machine precision as it will stop too soon if we don't include this.
      control = list(reltol = 10^(-15), maxit = 100000)
    )
    # Store the optimised θ and logLik.
    theta_opt <- opt_result$par
    logLik_opt <- LMMprof(theta_opt, y, X, qrZ, lengths)
    # Extract beta from logLik to return.
    beta <- attr(logLik_opt, "beta")
  }
  # Return theta, and beta.
  return(list(beta = beta, theta = theta_opt))
}



# Test the function
library(nlme)
library(lme4)

### Test Setup ###

#setup <- LMMsetup(score ~ Machine, Machines, list("Worker", c("Worker", "Machine")))
#print(setup)

### Default Test ###

result <- lmm(score ~ Machine, Machines, list("Worker", c("Worker", "Machine")))  
lmer_result <- lmer(score ~ Machine + (1 | Worker) + (1 | Worker:Machine),
                    data = Machines,
                    REML = FALSE
)
lmer_result
result

### Default Empty List Test ###

result <- lmm(score ~ Machine, Machines)  
lm_result <- lm(score ~ Machine,
                data = Machines)
summary(lm_result)
result

## Assay Tests
result <- lmm(logDens ~ dilut + sample + Block, Assay, list(c("Block","sample")))
result
lmer_result <- lmer(logDens ~ dilut + sample + Block + (1 | Block:sample),
                    data = Assay,
                    REML = FALSE
)
lmer_result

## Meat Tests ##

result <- lmm(score ~ Storage + Block, Meat, list(c("Block", "Storage")))
result
lmer(score ~ Storage + Block + (1 | Block) + (1 | Storage) + (1 | Block:Storage),
     data = Meat,
     REML = FALSE
)