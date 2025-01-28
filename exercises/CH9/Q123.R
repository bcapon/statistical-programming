# Q1
x <- c(0.1,1.3,0.4,1.4,2.0,1.6)
z <- factor(c("a","a","fred","a","c","c"))
model.matrix(~ x + z)

# Q2
x <- c("a","a","b","a","b","a")
z <- c("ctrl","trt","trt","trt","ctrl","ctrl")
model.matrix(~x*z)

# Q3
z <- c(1,2,1,3,4,2)
model.matrix(~x*z)