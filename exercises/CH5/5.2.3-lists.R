# Lists used to combine objects of different types

stuff <- list(a=1:6,txt="furin cleavage site",l2 = function(x) log(x^2),more = list(a="er",b=42))
stuff[[1]]
stuff[1]
typeof(stuff[[1]])
typeof(stuff[1])

stuff[["l2"]] # Name indexing or stuff$12
stuff$a # Dollar for name indexing only seems to work for string names?
stuff[c(1,2)] # Indexing multiple elements

stuff[["g"]] <- runif(10) ## Adding an element or use stuff$g = ...
stuff

names(stuff)[1:2] <- c("b","text") # How to change names
names(stuff)
