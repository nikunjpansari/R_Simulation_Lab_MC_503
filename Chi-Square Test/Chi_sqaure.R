## Rayleigh distribution

## Sample generation for 100 samples

n=100
mui=1.2
lambda=1.5
t=c()
u <- runif(n)
for(i in 1:n){
  t = (mui + sqrt(-log(1-u)/lambda))
}

t
sort(t)

a=max(t);a
b=min(t);b

s1=(a-b)/10;s1

int=seq(b,a,s1);int

obs=c(rep(0,10))
for(i in 1:10){
  for(j in 1:100){
    if(t[j]>=int[i] && t[j]<=int[i+1])
      {
      obs[i]=obs[i]+1
      print(t[j])
    }
  }
}    
obs  
sum(obs)

## Goodness Fit test for F(x) = 1 − exp−λ(x−μ)^2; x > μ; μ, λ > 0

exp_value=c()
func1 <- function(x){
  k = 1-exp(-lambda*(x-mui)^2)
  return(k)
}

for(i in 1:10){
  exp_value[i] = 100*(func1(int[i+1])-func1(int[i]))
}
exp_value
sum(exp_value)

obs_val=0
for(i in 1:10){
  s1 = ((obs[i]-exp_value[i])^2)/exp_value[i]
  print(s1)
  obs_val = obs_val+s1
} 
obs_val

## Applying Chi-square Goodness Fit Test
## From Table at df=9 and 0.05 significance level

act_val = qchisq(0.05,lower.tail = FALSE,9)
if(obs_val > act_val){
  print("Chi-Square hypothesis Rejected, Goodness fit test failed")
}else
{print("Chi-Square hypothesis Accepted, Goodness fit test passed")
}


## Burr XII distribution

km = c(.70 ,.84 ,.58 ,.50 ,.55 ,.82 ,.59 ,.71 ,.72 ,.61 ,.62 ,.49 ,.54 
       ,.36 ,.36 ,.71 ,.35 ,.64 ,.85 ,.55 ,.59 ,.29 ,.75 ,.46 ,.46 ,.60 
       ,.60 ,.36 ,.52 ,.68 ,.80 ,.55 ,.84,.34 ,.34 ,.70 ,.49 ,.56 ,.71 
       ,.61 ,.57 ,.73 ,.75 ,.44 ,.44 ,.81,.80,.87,.29,.50)

g = sort(km);g
n = length(g);n

a_1=max(km);a_1
b_1=min(km);b_1

s2=(a_1-b_1)/10;s2

int_burr_XII = seq(b_1,a_1,s2);int_burr_XII

obs_1=c(rep(0,10))
for(i in 1:10){
  for(j in 1:50){
    if(km[j]>=int_burr_XII[i] && km[j]<=int_burr_XII[i+1])
    {
      obs_1[i]=obs_1[i]+1
      print(km[j])
    }
  }
}    
obs_1  
sum(obs_1)

## Computing the MLE of Burr XII: F(x; c, k) = 1 − (1 + xc)−k; x > 0; c, k > 0.

library(nleqslv)
options(max.print = 1000)
c=1
k=2
n=50

  
  ## Defining the function for BurXII distribution
  burXll <- function(para){
    c = para[1]
    k = para[2]
    s1=0
    s2=0
    for(i in 1:50){
      s1 = s1 + log(km[i]) - (k+1)*(km[i]^c)*log(km[i])/(1 + km[i]^c)
      s2 = s2 + log(1 + km[i]^c)
    }
    eqn <- numeric(2)
    eqn[1] <- n/c + s1
    eqn[2] <- n/k - s2
    return(eqn)
  }
  xstr=c(1,2)
  res=nleqslv(xstr, burXll);res
  mle_1=res$x[1]
  mle_2=res$x[2]

mle_1
mle_2
## Goodness Fit test for Burr XII: F(x; c, k) = 1 − (1 + xc)−k; x > 0; c, k > 0.

exp_value_1=c()
func2 <- function(x){
  k1 = (1 - (1 +x^mle_1)^(-mle_2))
  return(k1)
}

for(i in 1:10){
  exp_value_1[i] = 50*(func2(int_burr_XII[i+1])-func2(int_burr_XII[i]))
}
exp_value_1
sum(exp_value_1)

lk=0
for(i in 1:10){
  s_11 = ((obs_1[i]-exp_value_1[i])^2)/exp_value_1[i]
  lk = lk + s_11
} 
lk


## Applying Chi-square Goodness Fit Test for Burr XII Distribution
## From Table at df=9 and 0.05 significance level

kj = qchisq(0.05,lower.tail = FALSE,9) ;kj
if(lk > kj){
  print("Chi-Square hypothesis Rejected, Goodness fit test failed")
}else
{print("Chi-Square hypothesis Accepted, Goodness fit test passed")
}


## Burr X: F(x; c, k) = (1 − e−(cx)^2)k; x > 0, c > 0, k > 0.

km_1= c(.70 ,.84 ,.58 ,.50 ,.55 ,.82 ,.59 ,.71 ,.72 ,.61 ,.62 ,.49 ,.54 
       ,.36 ,.36 ,.71 ,.35 ,.64 ,.85 ,.55 ,.59 ,.29 ,.75 ,.46 ,.46 ,.60 
       ,.60 ,.36 ,.52 ,.68 ,.80 ,.55 ,.84,.34 ,.34 ,.70 ,.49 ,.56 ,.71 
       ,.61 ,.57 ,.73 ,.75 ,.44 ,.44 ,.81,.80,.87,.29,.50)

g_1 = sort(km_1);g_1
n_1 = length(g_1);n_1

a_11 = max(km);a_11
b_11 = min(km);b_11

s_burr_X=(a_11-b_11)/10;s_burr_X

int_burr_X = seq(b_11,a_11,s_burr_X);int_burr_X

obs_2=c(rep(0,10))
for(i in 1:10){
  for(j in 1:50){
    if(km_1[j]>=int_burr_X[i] && km_1[j]<=int_burr_X[i+1])
    {
      obs_2[i]=obs_2[i]+1
      print(km_1[j])
    }
  }
}    
obs_2  
sum(obs_2)

## Computing the MLE of Burr X: F(x; c, k) = (1 − e−(cx)^2)k; x > 0, c > 0, k > 0.

library(nleqslv)
options(max.print = 1000)
c=2
k=3
n=50

## Defining the function for Burr X distribution
burX <- function(para){
  c = para[1]
  k = para[2]
  s1=0
  s2=0
  s3=0
  for(i in 1:50){
    s1 = s1 + (km_1[i]^2) 
    s2 = s2 + ((km_1[i]^2)*exp(-(c*km_1[i])^2)/(1-exp(-(c*km_1[i])^2)))
    s3 = s3 + log(1 - exp(-(c*km_1[i])^2))
  }
  eqn <- numeric(2)
  eqn[1] <- 2*(n/c) - (2*c*s1) + (k-1)*(c^2)*s2
  eqn[2] <- n/k + s3
  return(eqn)
}
xstr=c(2,3)
res=nleqslv(xstr, burX);res
mle_1_burr_X=res$x[1]
mle_2_burr_X=res$x[2]

mle_1_burr_X
mle_2_burr_X

## Goodness Fit test for Burr X distribution

exp_value_12=c()
func2 <- function(x){
  k11 = (1 - exp(-(mle_1_burr_X*x)^2))^mle_2_burr_X
  return(k11)
}

for(i in 1:10){
  exp_value_12[i] = 50*(func2(int_burr_X[i+1])-func2(int_burr_X[i]))
}
exp_value_12
sum(exp_value_12)

lk_1=0
for(i in 1:10){
  s_121 = ((obs_2[i]-exp_value_12[i])^2)/exp_value_12[i]
  lk_1 = lk_1 + s_121
} 
lk_1


## Applying Chi-square Goodness Fit Test for Burr XII Distribution
## From Table at df=9 and 0.05 significance level

kj = qchisq(0.05,lower.tail = FALSE,9) ;kj
if(lk_1 > kj){
  print("Chi-Square hypothesis Rejected, Goodness fit test failed")
}else
{print("Chi-Square hypothesis Accepted, Goodness fit test passed")
}



