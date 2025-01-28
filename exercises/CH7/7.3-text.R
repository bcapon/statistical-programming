# getwd() and setwd() can change the directories
# source() can be used to read in code in wd
# e.g. dump(c(1,2,3), file="stuff.R") can save code in an r file
setwd("C:/Users/BCapo/Documents/GitHub/extended-statistical-programming/CH7")
text_file <- scan("data.txt", what = list("c",1,1), skip = 1)
names(text_file) <- scan("data.txt", what = "c", n = 3)
data.frame(text_file)

# Better to use read.table():
read.table("data.txt", header = TRUE)
#readLines("data.txt")

A <- matrix(1:6,3,2); y <- rnorm(100) ## create some objects
save(A,y,file="stuff.Rd") ## save them to a binary file
rm(y,A) ## delete the original objects
load("stuff.Rd") ## load them back in again from the file

raw <- readLines("https://www.gutenberg.org/cache/epub/84/pg84.txt")
raw