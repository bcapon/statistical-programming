# Samples 50 numbers with replacement with prob proportional to 1/i
set.seed(0)
sample(1:100,50, replace = TRUE, prob = 1/1:100)

n <- 1000
bd <- matrix(sample(1:366, 30*n, replace = TRUE, prob = c(rep(4,365),1)), n, 30)
unique <- apply(bd, 1, unique)
typeof(unique)
length(which(lapply(unique, length) < 30)) / n # This is the probability required

# Alternatively: p <- mean(apply(bd, 1, function(x) length(unique(x)) != 30))