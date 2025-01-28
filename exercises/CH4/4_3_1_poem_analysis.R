poem <- paste("Inside me is a skeleton, of this I have no doubt,",
              "now it’s got my flesh on, but it’s waiting to get out.")
pow <- strsplit(poem," ")[[1]] ## vector of poem words
pow
n.words <- length(pow) ## number of words
freq1 <- tabulate(nchar(pow)) ## count frequency of n-letter words

ie <- grep("e",pow,fixed=TRUE) ## find ‘e’ words
n.e <- length(ie) ## number of ‘e’ words

ia <- grep("a",pow,fixed=TRUE) ## find ‘a’ words
iea <- ia[ia %in% ie] ## find words with ‘e’ and ‘a’

pow[iea] <- paste(pow[iea],"*",sep="") ## mark ‘e’ ‘a’ words
paste(pow,collapse=" ") ## and put words back in one string.
cat(pow)