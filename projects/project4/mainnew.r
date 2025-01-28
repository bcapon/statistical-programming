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
## covariance matrix φ_θ parameterised by an unknown parameter θ, whilst Z is a
## model matrix for b. The idea behind this extra term is that we can now account
## for clustering of data or in other words, there is randomness at different levels
## within the data we need to account for.


## Setup Function for Variables ##

## Here we write a function to provide us with the variables required to fit
## a linear mixed model. We will take as inputs the model formula, form, which will
## specify the y = Xβ + ε part of the linear mixed model alongside a dataframe,
## dat, containing all the variables used. The final input is a list of vectors
## of variable names, ref, specifying the random effects of the linear mixed model.
## The function will return a list containing X, y, Z, the qr decomposition of Z,
## and the lengths of the random effects blocks, lengths.
## N.B. the function will return the linear model by default if no ref is supplied.

LMMsetup <- function(form, dat, ref = list()) {
  # Build the model matrix, X, using the formula and variables supplied as form
  # and dat respectively.
  X <- model.matrix(form, dat)

  # We want to extract the y vector corresponding to the model formula so we
  # extract the response vector from the model dataframe and convert to a matrix
  # with 1 column.
  y <- model.response(model.frame(form, dat))
  y <- matrix(y, ncol = 1)

  # If the ref is an empty list, return X and y as we are done. If not, initialise
  # the matrix Z with the same number of rows as dat and 0 columns.
  if (length(ref) == 0) {
    # Z <- matrix(NA, 0, 0) ### NA or NA matrix?
    return(list(X = X, y = y, qrZ = NULL, Z = NULL, lengths = 0))
  } else {
    Z <- matrix(NA, nrow(dat), 0)
  }

  # Also initialise a lengths vector where for each random effect vector in ref,
  # we add a block to Z
  lengths <- rep(0, length(ref))

  # Iterate through each vector in ref
  ### Would "i in 1:length(ref) be more standard here? Might have misunderestood the
  ### function here
  for (i in seq_along(ref)) {
    # Build a string for the formula, and then parse with as.formula e.g.
    # c("a", "b", "c") -> "~ a:b:c-1" -> ~ a:b:c-1. The "-1" accounts for the fact
    # that we would like to exclude any intercept from being in our effect formula
    effectformula <- as.formula(paste(c("~ ", paste(ref[[i]], collapse = ":"), "-1"), collapse = ""))

    # Build this block of Z by creating a new model matrix
    block <- model.matrix(effectformula, dat)

    # FOR DEBUGGING
    # print(paste(c("~ ", paste(ref[i], collapse = ":"), "-1")))

    # Bind the block to Z and assign the number of columns in the block to the
    # lengths vector we initialised earlier.
    lengths[i] <- ncol(block)
    Z <- cbind(Z, block)
  }

  # Finally, take the qr decomposition of Z and return it in a list alongside
  # all the other useful objects that we can avoid repeating calculations on
  # when fitting a linear mixed model.
  qrZ <- qr(Z)
  return(list(X = X, y = y, qrZ = qrZ, Z = Z, lengths = lengths))
}

## Here we write a function, cholsolve, which solves with triangular matrices using
## the Cholesky decomposition. Solving using this method, whilst still O(n**3) time,
## has a smaller constant of proportionality than "solve" and is optimal. The function
## will take inputs of A (a positive definitematrix which should be decomposed),
## y, and an optional argument L which is  the upper triangular Cholesky decomposition.
## The function will return a list containing the solution, x, and the Cholesky
## decomposition so we don't need to perform this calculation again.

### Where does the L come from, lower triangular? chol(A) returns the upper
### triangular factor so should be R, since it's taken for QR decomp shall we say
### chol_R? Might be a bit tired, cover in next workshop.
### Also, what do if A is NOT +ve definite?

cholsolve <- function(A, y, L = NULL) {
  # Check if we haven't calculated the triangular factor of the Cholesky decomposition
  # before and if so, calculate this now
  if (is.null(L)) {
    L <- chol(A)
  }

  # We will use forwardsolve to solve L^Tz = y and then backsolve to solve Lx = z
  # given that forwardsolve solves for an lower triangular matrix and backsolve
  # solves for a upper triangular matrix. After, return the list containing x and L.
  x <- backsolve(L, forwardsolve(t(L), y))
  return(list(x = x, L = L))
}

## Here we write a function, Wmul, which will perform the calculation of W for us.
## The function will take as inputs x, qrZ, sigma2, n, p, phi and an optional argument
## L representing the upper triangular factor of the Cholesky decomposition of ??
## The function will output a list with Wx and WXL ??
Wmul <- function(x, qrZ, sigma2, n, p, phi, L = NULL) {
  # Compute (Q^T)x by using qr.qty() which multiplies x by the transpose of Q
  Qtx <- qr.qty(qrZ, x)

  # Check if we haven't already performed a Cholesky decomposition to get the upper
  # triangular factor and if not, ?
  if (is.null(L)) {
    WXL <- cholsolve(qr.R(qrZ) %*% phi %*% t(qr.R(qrZ)) + diag(sigma2, p), Qtx)
  } else {
    WXL <- cholsolve(NULL, Qtx, L = L)
  }
  Wx <- matrix(0, n, ncol(x)) # Initialize full n × ncol(X) matrix
  Wx[1:p, ] <- WXL$x # Fill in first p rows from solution
  Wx[(p + 1):n, ] <- Qtx[(p + 1):n, ] / sigma2 # Fill in remaining rows
  Wx <- qr.qy(qrZ, Wx)

  return(list(Wx = Wx, L = WXL$L))
}

NoEffectLMMprof <- function(theta, yXBeta2, n) {
  sigma2 <- exp(2 * theta)
  loglik <- -yXBeta2 / (2 * sigma2) - log(n * sigma2) / 2
  return(loglik)
}

## Here we write a function, LMMprof, to evaluate the negative log-likelihood for
## a given theta and compute the corresponding beta to be used as an attribute.
## To do this, we will take a vector theta as an input alongside y, X, qrZ
## defined in the LMMsetup function. The function will output a double, loglikelihood.
LMMprof <- function(theta, y, X, qrZ, lengths) {
  # Z has n rows (same as y) and p columns (the sum of the number of columns in
  # each of the blocks)
  p <- sum(lengths)
  n <- length(y)

  # The first element of theta is log(σ) so exp(2log(σ)) = exp(log(σ**2)) = σ**2
  # will provide us with the variance, sigma**2
  sigma2 <- exp(2 * theta[1])

  # All other elements of theta correspond to the log standard deviations of the
  # random effect variances so we create a diagonal matrix containing these to get
  # the covariance matrix, psi.
  # N.B. This matrix is diagonal as random effects are INDEPENDENT.
  psi <- diag(rep(exp(2 * theta[-1]), lengths))

  # If qrZ is null, we resort to a linear model calculating just this term. Otherwise,
  # we include the random effects:
  if (is.null(qrZ)) {
    mat2log <- diag(sigma2, p)
  } else {
    mat2log <- qr.R(qrZ) %*% psi %*% t(qr.R(qrZ)) + diag(sigma2, p)
  }


  MatChol <- chol(mat2log) ## Get Cholesky factorization R’R = S
  halflogofdet <- sum(log(diag(MatChol))) + (n - p) * log(sigma2) / 2

  # solve for beta hat

  lis <- Wmul(X, qrZ, sigma2, n, p, psi)
  L <- lis$L
  WX <- lis$Wx
  XtWy <- t(X) %*% Wmul(y, qrZ, sigma2, n, p, psi, L = L)$Wx # Wmul(y, qrZ, sigma2, n, p, phi, L = L)

  beta <- cholsolve(t(X) %*% WX, XtWy)$x

  firstterm <- t(y - X %*% beta) %*% Wmul(y - X %*% beta, qrZ, sigma2, n, p, psi, L = L)$Wx

  loglikehood <- -firstterm / 2 - halflogofdet
  attr(loglikehood, "beta") <- beta
  return(loglikehood)
}


lmm <- function(form, dat, ref = list()) {
  # Get setup components
  setup <- LMMsetup(form, dat, ref)
  X <- setup$X
  y <- setup$y
  qrZ <- setup$qrZ
  lengths <- setup$lengths

  if (is.null(qrZ)) {
    beta <- cholsolve(t(X) %*% X, t(X) %*% y)$x
    yxbeta <- y - X %*% beta
    yXBeta2 <- t(yxbeta) %*% yxbeta
    theta_init <- 1
    # optimise
    lower <- -50
    upper <- 50
    opt_result <- optim(
      par = theta_init,
      fn = function(theta) -as.numeric(NoEffectLMMprof(theta, yXBeta2, nrow(X))), # negative because optim minimises
      # control = list(reltol = 10^(-13), maxit = 1000),
      upper = upper,
      lower = lower,
      method = "Brent",
    )
    # store final theta/ beta
    theta_opt <- opt_result$par
    final_ll <- NoEffectLMMprof(theta_opt, yXBeta2, nrow(X))
  } else {
    # Optimize LMMprof with respect to theta
    # Start with all parameters at 0 - fix later random init? ask in class
    theta_init <- rep(1, length(ref) + 1)

    # optimise
    opt_result <- optim(
      par = theta_init,
      fn = function(theta) -as.numeric(LMMprof(theta, y, X, qrZ, lengths)), # negative because optim minimises
      control = list(reltol = 10^(-15), maxit = 100000)
    )
    # store final theta/ beta
    theta_opt <- opt_result$par
    final_ll <- LMMprof(theta_opt, y, X, qrZ, lengths)
    beta <- attr(final_ll, "beta")
  }

  return(list(
    sd = exp(theta_opt),
    beta = beta,
    loglik = -opt_result$value,
    # - (nrow(X))/2 * log(2 * pi),
    convergence = opt_result$convergence
  ))
}



# BROKEN FOR list() - FIX THIS - special case handle?
### Can't perform qr.R on empty matrix, this will be the problem which needs solving ###

# Test the function
library(nlme)
library(lme4)

# result <- lmm(score ~ Machine, Machines, list()) # list("Worker", c("Worker", "Machine")))
# print(result)
# setup <- LMMsetup(score ~ Machine, Machines, list("Worker", c("Worker", "Machine")))
# print(setup)

result <- lmm(score ~ Machine, Machines)
result
# lmer_result <- lmer(score ~ Machine + (1 | Worker) + (1 | Worker:Machine),
#   data = Machines,
#   REML = FALSE
# )
#
# # Print fixed effects
# cat("\nFixed Effects:\n")
# print(fixef(lmer_result))
#
# # Get random effects standard deviations from lmer
# lmer_sds <- c(
#   sigma(lmer_result),
#   attr(VarCorr(lmer_result)$Worker, "stddev"),
#   attr(VarCorr(lmer_result)$`Worker:Machine`, "stddev")
# )
# print(lmer_sds)
# print(result$sd)
#
# # Convert to theta (log scale)
# lmer_theta <- log(lmer_sds)
# print(lmer_theta)
# # Get our fitted theta
# our_theta <- log(result$sd)
# print(our_theta)
# # Evaluate log likelihood at both sets of parameters
# our_ll <- LMMprof(our_theta, setup$y, setup$X, setup$qrZ, setup$lengths)
# lmer_ll <- LMMprof(lmer_theta, setup$y, setup$X, setup$qrZ, setup$lengths)
#
# cat("\nLog likelihood comparison:\n")
# cat("Our fitted values:", as.numeric(our_ll), "\n")
# cat("lmer fitted values:", as.numeric(lmer_ll), "\n")
