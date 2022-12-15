
#install.packages("pracma")
library(pracma)

## Bisection Method 
## Using 'bisect'
f1 <- function(x) { x^3 - x - 4}

bisect(f1, 1, 2)

## Using user-defined functions
bisection <- function(a,b,func,ep){
  
  if(func(a)*func(b) > 0){
    return("Given range is not appropriate")
  }
  i = 0
  while (b-a >= ep) {
    m = (a+b)/2
    ym = func(m)
    if((func(a)*ym)<0){
      b = m
    }
    else if((ym*func(b))<0){
      a = m
    }
    else if(ym == 0){
      return(m)
    }
    cat("a =",a,"\tb =",b,"\n")
    i = i + 1
  }
  cat("No. of iterations:\t",i,"\n")
  return(a)
}

## Regula Falsi Method

f2 <- function(x) { exp(x^2-1) + 10*sin(2*x) - 5}
regulaFalsi(f2, -1, 1,tol=0.001)

## Fixed Point Method

##install.packages("spuRs")
library(spuRs)

f3 <- function(x) {(10/(x+4))^(1/2)}
fixedpoint(f3, 1,max.iter=50,tol=0.001)

##Newton Rapshon Method
f4 <- function(x) {exp(x)-1-x-((x^2)/2)-((x^3)/6*exp(0.3*x))}
newtonraphson(f4,0.456,tol=0.001,max.iter = 200)


## System of Linear Equations

F1 = matrix( nrow=3, ncol=3, data=c(1,-2,3,-1,3,-1,2,-5,5), byrow = TRUE)
F2 = matrix(c(9,-6,17))
res=solve(F1)
Result=res%*%B
cat("x: ",Result[1],"\ny: ",Result[2],"\nz: ",Result[3])


