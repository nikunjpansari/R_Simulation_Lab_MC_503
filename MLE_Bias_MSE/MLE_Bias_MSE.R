## Burr XII distribution.

library(nleqslv)
options(max.print = 1000)
c=1
k=2
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
    t = (((1 - u)^(-1/k))-1)^(1/c)
  }
  
  ## Defining the function for BurXII distribution
  burXll <- function(para){
    c = para[1]
    k = para[2]
    s1=0
    s2=0
    for(i in 1:n){
      s1 = s1 + log(t[i]) - (k+1)*(t[i]^c)*log(t[i])/(1 + t[i]^c)
      s2 = s2 + log(1 + t[i]^c)
    }
    eqn <- numeric(2)
    eqn[1] <- n/c + s1
    eqn[2] <- n/k - s2
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, burXll);res
  mle_1[j]=res$x[1]
  mle_2[j]=res$x[2]
}  

## MLE of Burr XII Distribution
mle1 = mean(mle_1);mle1
mle2 = mean(mle_2);mle2

## Computing the bias

b1 = (mle1-c);b1
b2 = (mle2-k);b2

## Computing the MSE

mse1 = (mle1-c)^2;mse1
mse2 = (mle2-k)^2;mse2

## Computing the fisher Information matrix

matrx_Fisher_information <- function(para){
  c=para[1]
  k=para[2]
  b=runif(50)
  x=fun1(b)
  for(i in 1:50){
    s1 = ((x[i]^c)*(log(x[i]))^2)/(1+x[i]^c)^2
    s2 = (x[i]^c)*log(x[i])/(1 + x[i]^c)
  }
  fi <- matrix(nrow = 2, ncol = 2)
  fi[1,1] = -n/c^2 - (k+1)*s1
  fi[1,2] = -s2
  fi[2,1] = -s2
  fi[2,2] = -n/(k^2)
  return(fi)
}

## Displaying the fisher Information matrix

fim = matrx_Fisher_information(c(c,k));fim

## Computing the Variance Covariance matrix

Varince_Covariance_mat = solve(-fim);Varince_Covariance_mat

var_c = var(mle_1)
var_k  = var(mle_2)
z = 1.96
left_int_c <- c()
right_int_c <- c()
left_int_k <- c()
right_int_k <- c()
for(i in 1:1000){
  left_int_c[i] = mle1 - z*sqrt(var_c)
  right_int_c[i] = mle1 + z*sqrt(var_c)
  left_int_k[i] = mle2 - z*sqrt(var_k)
  right_int_k[i] = mle2 + z*sqrt(var_k)
}
li_c = mean(left_int_c);li_c
ri_c = mean(right_int_c);ri_c
li_k = mean(left_int_k);li_k
ri_k = mean(right_int_k);ri_k


##---------------------------------------------------------##


## Inverse Weibull Distribution

library(nleqslv)
options(max.print = 1000)
a=1
b=2
t=c()
n=50

##defining the result vector
MLE_1 <- c()
MLE_2 <- c()

## Iterating 1000 times
for(j in 1:1000){
  ## Generating 50 samples
  u <- runif(n)
  for(i in 1:n){
    t = (-log(u)/b)^(-1/a)
  }
  
  ## Defining the function for Inverse Weibull distribution
  weibull_inverse <- function(para){
    a = para[1]
    b = para[2]
    s1=0
    s2=0
    for(i in 1:n){
      s1 = s1 + b*(t[i]^(-a))*log(t[i]) - log(t[i])
      s2 = s2 + t[i]^(-a)
    }
    eqn <- numeric(2)
    eqn[1] <- n/a + s1
    eqn[2] <- n/b - s2
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, weibull_inverse);res
  MLE_1[j]=res$x[1]
  MLE_2[j]=res$x[2]
}  

## MLE of Inverse Weibull Distribution
MLE1 = mean(MLE_1);MLE1
MLE2 = mean(MLE_2);MLE2

## Computing the bias

b_1 = (MLE1-a);b_1
b_2 = (MLE2-b);b_2

## Computing the MSE

mse_1 = (MLE1-a)^2;mse_1
mse_2 = (MLE2-b)^2;mse_2

## Computing the fisher Information matrix

matrx_Fisher_information_inverse <- function(para){
  a=para[1]
  b=para[2]
  m=runif(50)
  x=fun1(m)
  for(i in 1:50){
    s1 = b*(x[i]^(-a))*(log(x[i]))^2
    s2 = (x[i]^(-a))*log(x[i])
  }
  inv_wei_mat = matrix(nrow = 2, ncol = 2)
  inv_wei_mat[1,1] = -n/(a^2) - s1
  inv_wei_mat[1,2] = -s2
  inv_wei_mat[2,1] = -s2
  inv_wei_mat[2,2] = -n/(b^2)
  return(inv_wei_mat)
}

## Displaying the fisher Information matrix

fim_inv_wei = matrx_Fisher_information_inverse(c(a,b));fim_inv_wei

## Computing the Variance Covariance matrix

Varince_Covariance_mat_inv_wei = solve(-fim_inv_wei);Varince_Covariance_mat_inv_wei

var_a = var(MLE_1)
var_b  = var(MLE_2)
z = 1.96
left_int_a_inv_wei <- c()
right_int_a_inv_wei <- c()
left_int_b_inv_wei <- c()
right_int_b_inv_wei <- c()
for(i in 1:1000){
  left_int_a_inv_wei[i] = MLE1 - z*sqrt(var_a)
  right_int_a_inv_wei[i] = MLE1 + z*sqrt(var_a)
  left_int_b_inv_wei[i] = MLE2 - z*sqrt(var_b)
  right_int_b_inv_wei[i] = MLE2 + z*sqrt(var_b)
}
inv_wei_li_a = mean(left_int_a_inv_wei);inv_wei_li_a
inv_wei_ri_a = mean(right_int_a_inv_wei);inv_wei_ri_a
inv_wei_li_b = mean(left_int_b_inv_wei);inv_wei_li_b
inv_wei_ri_b = mean(right_int_b_inv_wei);inv_wei_ri_b
