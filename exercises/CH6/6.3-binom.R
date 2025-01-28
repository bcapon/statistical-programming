n.rep <- 10000
n.ci <- 0 
n <- 20; p <- 0.2
for (i in 1:n.rep){
  x <- rbinom(1,n,p)
  p.hat <- x / n
  sig.p <- sqrt(p.hat * (1-p.hat)/n)
  if(p.hat - 1.96*sig.p <= p && p <= p.hat + 1.96*sig.p) n.ci = n.ci +  1
}
n.ci