a <- factor(sample(c("fred","george","sue","ann"),20,replace=TRUE)); a # replace allows multiple of c(...)
b <- factor(a, levels = c("ann","sue","fred","george")); b

as.numeric(a)
as.numeric(b)

al <- levels(a)
al
an <- as.numeric(a)
an

# Use vector indexing:
c(al[an])
