# Q1
A <- matrix(sample(9), 3,3)
B <- matrix(sample(9), 3,3)
sum(t(A) * B)

# Q2
sum(colSums(t(A) * B)) # Same as diag(AB)