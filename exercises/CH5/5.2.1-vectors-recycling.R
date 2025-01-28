a3d <- c(TRUE, FALSE, FALSE)
class(a3d)
typeof(a3d)
a3d[2:3] # Indexing starts at 1

bb <- 1:10 # Range function is inclusive of last number
print(bb)
class(bb)
bb[c(1, 4, 9)] <- c(.1, -3, 2.2) # Need c() for indexing multiple numbers
bb
class(bb)
typeof(bb)

# c <-sin(a) * b carries out c[i] <-sin(a[i]) *b[i] for all i
# c <-sin(a) * b + 2 interprets 2 as a vector of 2s (recycling rule)

a <- 1:5 # a 4-vector
b <- 5:7 # a 3-vector
a * b # In this case, first 2 elements of b are repeated to make 5-vector

x <- 1:5; names(x) <- c("fred","sue","bill","eve","bob") ## Can name indexes!
print(x)
x[c(TRUE, FALSE, TRUE, FALSE, FALSE)] # Boolean indexing is built-in
x[c(TRUE, FALSE)] # Another example of vector recycling
x[x>3] # And conditional indexing
x[c("sue", "eve")] # name indexing
