# Factors are a vector of labels and as can be seen here, have levels.
# The levels appear to be alphabetically ordered with numerical encoding
fac <- factor(c("Fred", "Dave", "Bob", "Dan", "Fred"))
class(fac)
fac
levels(fac)
as.numeric(fac)
