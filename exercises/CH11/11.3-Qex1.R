n <- 10
A <- matrix(sample(n*n), n, n)
R <- qr.R(qr(A))
R_i <- backsolve(R,diag(ncol(R))) # Solves R %*% Ri = I
R %*% (R_i) # should be identity

chol2inv(R)
R_i %*% t(R_i) # Should be equal