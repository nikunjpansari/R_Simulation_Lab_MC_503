## Exponential Distribution
## f(x) = (1/σ) exp (−x/σ), x > 0, σ > 0

n=50 ## no of samples
sigma=2
options(max.print=50)
sample_gen=function(n){
  x=numeric(0)
  i = 1
  for(u in 1:n){
    x[i]=(-sigma)*log(1-runif(1));
    i = i + 1
  }
  return(x);
}
vec1=sample_gen(n);vec1
mean(vec1)

## MLE (Maximum Likelihood Estimation of f(x) is mean(X))

## f(x) = (1/σ) exp (−(x − μ)/σ), x > 0, σ > 0

mui=3  
sigma=2
options(max.print=50)
sample_gen_1=function(n){
  y=numeric(0)
  i = 1
  for(u in 1:50){
    y[i]=mui-sigma*log(1-(runif(1)));
    i = i + 1
  }
  return(y);
}
vec2=sample_gen_1(n);vec2
mn=mean(vec2);mn
max_le=mn-mui;max_le

## MLE (Maximum Likelihood Estimation of f(x) is mean(X)- mui)

## Generalized Inverted Exponential Distribution

library(nleqslv)
options(max.print = 1000)
a=2
b=3
t=c()
n=50

##defining the result vector
mle_inv_1 <- c()
mle_inv_2 <- c()

## Iterating 1000 times
for(j in 1:1000){
  ## Generating 50 samples
  u <- runif(n)
  for(i in 1:n){
    t = -b/(log(1 - (1-u)^(1/a)))
  }
  
  ## Defining the function for Inverted Exponential Distribution
  exp_inv <- function(para){
    a1 = para[1]
    b1 = para[2]
    s1=0
    s2=0
    s3=0
    for(i in 1:n){
      s1 = s1 + log(1 - exp(-b1/t[i]))
      s2 = s2 + 1/t[i]
      s3 = s3 + ((1/t[i])*exp(-b1/t[i]))/(1-exp(-b1/t[i]))
    }
    eqn=numeric(2)
    eqn[1] = (n/a1) + s1
    eqn[2] = (n/b1) - s2 + (a1-1)*s3
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, exp_inv);res
  mle_inv_1[j]=res$x[1]
  mle_inv_2[j]=res$x[2]
}  

## Mean of Results (MLE)
mle_inv1 = mean(mle_inv_1);mle_inv1
mle_inv2 = mean(mle_inv_2);mle_inv2

## Chen Distribution
library(nleqslv)
options(max.print = 1000)
alpha=1
beta=2
t=c()
n=50

##defining the result vector
mle_1 <- c()
mle_2 <- c()

## Iterating 1000 times
for(j in 1:1000){
  ## Generating 50 samples
  u <- runif(n)
  for(i in 1:n){
    t = (log(1-(log(1-u)/alpha)))^(1/beta)
  }
  
  ## Defining the function for Chen distribution
  chen <- function(para){
    alpha = para[1]
    beta = para[2]
    s1=0
    s2=0
    s3=0
    for(i in 1:n){
      s1 = s1 + (1-exp(t[i]^beta))
      s2 = s2 + log(t[i])
      s3 = s3 + ((t[i]^beta)*log(t[i])*(1-(alpha * exp(t[i]^beta))))
    }
    eqn=numeric(2)
    eqn[1] = (n/alpha) + s1
    eqn[2] = (n/beta) + s2 +s3
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, chen);res
  mle_1[j]=res$x[1]
  mle_2[j]=res$x[2]
}  

## Mean of Results (MLE)
mle1 = mean(mle_1);mle1
mle2 = mean(mle_2);mle2

## Cauchy Distribution

## f(x) = σ/π(σ2+x2) , x ∈ R, σ > 0.

library(nleqslv)
options(max.print = 1000)
sigma=2
t=c()
n=50

##defining the result vector
mle1_1 <- c()

## Iterating 1000 times
for(j in 1:1000){
  ## Generating 50 samples
  u <- runif(n)
  for(i in 1:n){
    t = sigma*tan((pi/2)*((2*u)-1));
  }
  #print(u)
  ## Defining the function for Chen distribution
  cauchy_1 <- function(para){
    sigma = para[1]
    s1=0
    s2=0
    s3=0
    for(i in 1:n){
      s1 = s1 + (2*sigma)/((sigma)^2 + (t[i])^2)
    }
    eqn=numeric(1)
    eqn[1] = (n/sigma) - s1
    return(eqn)
  }
  xstr=c(1)
  res=nleqslv(xstr, cauchy_1);res
  mle1_1[j]=res$x[1]
}  

## Mean of Results (MLE)
mle11 = mean(mle1_1);mle11

## f(x) = σ/π(σ^2+(x−μ)^2) , x, μ ∈ R, σ > 0.

library(nleqslv)
options(max.print = 1000)
sigma=1
mui=2
t=c()
n=50

##defining the result vector
mle2_1 <- c()
mle2_2 <- c()

## Iterating 1000 times
for(j in 1:1000){
  ## Generating 50 samples
  u <- runif(n)
  for(i in 1:n){
    t =mui+sigma*(tan(((u-0.5)*pi)));
  }
  
  ## Defining the function for Cauchy distribution
  cauchy_2 <- function(para){
    sigma = para[1]
    mui=para[2]
    s1=0
    s2=0
    for(i in 1:n){
      s1 = s1 + (2*sigma)/((sigma)^2 + ((t[i]-mui)^2))
      s2 = s2 + (2*(t[i]-mui))/((sigma)^2 + ((t[i]-mui)^2))
    }
    eqn=numeric(2)
    eqn[1] = (n/sigma) - s1
    eqn[2] = s2
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, cauchy_2);res
  mle2_1[j]=res$x[1]
  mle2_2[j]=res$x[2]
}  

## Mean of Results (MLE)
mle21 = mean(mle2_1);mle21
mle22 = mean(mle2_2);mle22