poem <- paste("Inside me is a skeleton, of this I have no doubt,",
              "now it’s got my flesh on, but it’s waiting to get out.")
poem <- gsub(", ", "\n", poem, fixed = TRUE)
poem <- gsub(".", "\n", poem, fixed = TRUE)
cat(poem)