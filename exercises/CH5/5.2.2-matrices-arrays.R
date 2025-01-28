# Vectors are one dimensional arrays data
# Arrays can have many dimensions

a <- array(1:24, c(3,2,4)) # First entry is a vector and second is dimensions (4 3x2 matrices)
a[1:2,1,] # (1,1) and (2,1)th elements in each matrix of the array. Select all with an empty arg
## ADD NOTES
a[7] # filled by column and selected by column
d = dim(a) # Dimensions as vector
a[3 + (2-1)*d[1] + (3-1)*d[1]*d[2]] # Here is (3,2,3)th element with a[i + (j − 1)d1 + (k − 1)d1d2] formula
# for (i,j,k)th element

# Two dimensional arrays can be called by matrix()

B <- matrix(1:6,2,3); B ## create a matrix (filled by col)
B[1,2] <- -1 ## change element 1,2
a <- c(.3,-1.2,2.3) ## a 3-vector
B %*% a # matrix multiplication
B * a # ELEMENT WISE MULTIPLICATION WITH RECYCLING