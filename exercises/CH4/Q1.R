poem <- paste("Inside me is a skeleton, of this I have no doubt,",
              "now it’s got my flesh on, but it’s waiting to get out.")
poem <- gsub("[,.]", "", poem)
pow <- strsplit(poem," ")[[1]] ## vector of poem words
#length(pow)
table(nchar(pow))
paste(pow, collapse = " ")
