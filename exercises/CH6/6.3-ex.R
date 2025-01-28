n.rep <- 10000 ## number of CIs to compute
n.ci <- 0 ## counter for number that include the truth
n <- 20;p <- .2
for (i in 1:n.rep) {
  x <- rbinom(1,n,p) ## generate x
  p.hat <- x/n ## compute the estimate of p
  sig.p <- sqrt(p.hat*(1-p.hat)/n) ## and its standard error
  if (p.hat - 1.96*sig.p <p && p.hat+1.96*sig.p > p) n.ci <- n.ci + 1 ## truth included?
}
n.ci/n.rep ## proportion of intervals including the truth

# OR USING VECTORS:
x <- rbinom(n.rep, n, p) ## generate x
p.hat <- x / n ## compute the estimate of p
sig.p <- sqrt(p.hat*(1-p.hat)/n) ## and its standard error
mean((p.hat - 1.96*sig.p < p) & (p.hat+1.96*sig.p > p))
