## Assignment 1

## Problem 1
x = c(4, -5, 2)
y = c(6, 1, -3)

length(x)
length(y)

x+y
x-y

sum(x)
sum(y)

a <- length(x)
covariance = sum((x-mean(x))*(y-mean(y))/(a-1))
print(covariance)

cov(x, y) 

## Problem 2

x1 = rep(50, length = 10); x1
x2 = rep(c(1,2), length = 30); x2
x3 = seq(from=-1, to=3, length = 50); x3
x4 = r(2, length=8); x4
x5 = c(2, 1); x5

## Merging the Vector
result = c(x1, x2, x3, x4, x5);result

## Problem 3
A = matrix(c( 1.5, -1, -1, 3), nrow = 2); A
x = c(4,7); x
y = c(1,0); y

## Result matrix C

C = matrix( c(A,x,y), nrow = 2, ncol = 4); C

## Problem 4

r1A = c(3,-2,1)
r2A = c(-1,4,-2)

r1B = c(-7,4)
r2B = c(9,5)
r3B = c(2,-1)

## using rbind

A = rbind(r1A,r2A); A
B = rbind(r1B,r2B,r3B); B

c1A = c(3,-1)
c2A = c(-2,4)
c3A = c(1,-2)

c1B = c(-7,9,2)
c2B = c(4,5,-1)

## Using cbind

A = cbind(c1A,c2A,c3A); A
B = cbind(c1B,c2B); B

## finding the matrix multiplication

AB = A%*%B; AB

## matrix transpose and inverse

transpose = t(AB); transpose
inverse = solve(AB); inverse

## Finding the mean and Standard deviation for each row and column of all matrices

## Matrix A

mean(r1A)
sd(r1A)
mean(r2A)
sd(r2A)

mean(c1A)
sd(c1A)
mean(c2A)
sd(c2A)
mean(c3A)
sd(c3A)

## Matrix B

mean(r1B)
sd(r1B)
mean(r2B)
sd(r2B)
mean(r3B)

sd(r3B)

mean(c1B)
sd(c1B)
mean(c2B)
sd(c2B)

## For matrix AB

mean(AB[1,])
sd(AB[1,])
mean(AB[2,])
sd(AB[2,])

mean(AB[,1])
sd(AB[,1])
mean(AB[,2])
sd(AB[,2])

## Matrix AB transpose

mean(transpose[1,])
sd(transpose[1,])
mean(transpose[2,])
sd(transpose[2,])

mean(transpose[,1])
sd(transpose[,1])
mean(transpose[,2])
sd(transpose[,2])

## Matrix AB Inverse

mean(inverse[1,])
sd(inverse[1,])
mean(inverse[2,])
sd(inverse[2,])

mean(inverse[,1])
sd(inverse[,1])
mean(inverse[,2])
sd(inverse[,2])

## Problem 5

x = seq( from = -1, to = 3, by = 0.01); x

y = x[ x<= 1.25]; y
a=length(x);a
b=length(y);b
difference = a-b; difference