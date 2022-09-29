## Random sample generation from exponential distribution --  Part (a)

## f(x) = (1/σ) exp (−x/σ), x > 0, σ > 0

n=2000 ## no of samples
sigma=2
options(max.print=2000)
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
var(vec1)

## f(x) = (1/σ) exp (−(x − μ)/σ), x > 0, σ > 0

mui=3  
sigma=2
options(max.print=2000)
sample_gen_1=function(n){
  y=numeric(0)
  i = 1
  for(u in 1:2000){
    y[i]=mui-sigma*log(1-(runif(1)));
    i = i + 1
  }
  return(y);
}
vec2=sample_gen_1(n);vec2
mean(vec2)
var(vec2)

## Random Sample generation from Cauchy Distribution

## f(x) = σ/π(σ2+x2) , x ∈ R, σ > 0.
# Assume sigma = 1, f(x) = 1/(pi*(1+x^2)),  x : R

n=2000  ## no of samples
sigma=1
options(max.print=2000)
sample_gen_2=function(n){
  z=numeric(0)
  i = 1
  for(u in 1:n){
    z[i]=sigma*tan((pi/2)*(2*runif(1)-1));
    i = i + 1
  }
  return(z);
}

vec3=sample_gen_2(n);vec3
mean(vec3)
median(vec3)
var(vec3)

## f(x) = σ/π(σ^2+(x−μ)^2) , x, μ ∈ R, σ > 0.

n=2000  ## no of samples
sigma=1
mui=2
options(max.print=2000)
sample_gen_4=function(n){
  w=numeric(0)
  i = 1
  for(u in 1:n){
    w[i]= mui+sigma*(tan(((runif(1)-0.5)*pi)));
    i = i + 1
  }
  return(w);
}

vec4=sample_gen_4(n);
vec4
mean(vec4)
median(vec4)
var(vec4)

## Uniform distribution :Probability Integral Transform

## f(x) ={a + 2(1 − a)x, 0 ≤ x ≤ 1 ; 0, elsewhere}
## E[x] = (-a/6 + 2/3)

a=8
k=5000  ## no of samples
X=rep(0,k)
## u1,u2,u3 ∼ U(0, 1)

for(i in 1:k){
    u1=runif(1,min=0,max=1)
    u2=runif(1,min=0,max=1)
    u3=runif(1,min=0,max=1)
    if(u1<=a)
    {
      X[i]=u2
    }
    if(u1>a)
    {
      X[i]=max(u2,u3)
    }
}

sort(X)
mean(X)
(-a/6+2/3)
