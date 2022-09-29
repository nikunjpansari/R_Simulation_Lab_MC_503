## Defining the function for Simpson's 1/3 Rule

Simson_1_3 <- function(ll,ul,num,func){
  
  h = (ul-ll)/num
  s1 = 0; s2 = 0
  for(i in 2:num){
    if( (i-1)%%2 == 0){
      s1 = s1 + func[i]
    }
    else{
      s2 = s2 + func[i]
    }
  }
  I = (h/3)*( func[1] + func[num+1] + 4*s2 + 2*s1 )
  return(I)
  
}

## Defining the function for Trapezoidal Rule

Trapezoidal_rule <- function(ll,ul,num,func){
  
  h = (ul-ll)/num
  s = 0
  for(i in 2:num){
    s = s + func[i]
  }
  I = (h/2)*( func[1] + func[num+1] + 2*s )
  return(I)
  
}

## ---------------------------------------------------------------------------##

##----------------------------Exercise 1--------------------------------------##

num = 10
ll = 0
ul = 1
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))

for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = 4*x - 3*x*x
}

res_1 = Simson_1_3(ll,ul,num,func)
print("Simpson's Rule : "); res_1
res_2 = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); res_2

##----------------------------Exercise 2--------------------------------------##

num = 5                                               # as n = (b-a)/h
ll = 0
ul = 5
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))

for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = 1/(1+x)
}
result = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); result

##----------------------------Exercise 3--------------------------------------##

num = 6
ll = 0
ul = 5
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))

for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = x/(1+x)
}
sim = Simson_1_3(ll,ul,num,func)
print("Simpson's Rule : "); round(sim,digits=3)
trap = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); round(trap,digits=3)

##----------------------------Exercise 4--------------------------------------##

num = 4
ll = 1.2
ul = 1.6
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))


for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = x + (1/x)
}



sim_1 = Simson_1_3(ll,ul,num,func)
print("Simpson's Rule : "); round(sim_1,digits=2)


trap_1 = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); round(trap_1,digits=2)

##----------------------------Exercise 5--------------------------------------##

num = 6
ll = 0
ul = 0.6
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))

for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = exp(x)
}
sim_2 = Simson_1_3(ll,ul,num,func)
print("Simpson's Rule : "); round(sim_2,digits=5)
trap_2 = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); round(trap_2,digits=5)

##----------------------------Exercise 6--------------------------------------##

num = 6
ll = 0
ul = pi/2
t = (ul-ll)/num

func = vector(mode="numeric", length=(num+1))

for(i in 1:(num+1)){
  x = t*(i-1)+ll
  func[i] = exp(x)*sin(x)
}
sim_3 = Simson_1_3(ll,ul,num,func)
print("Simpson's Rule : "); round(sim_3,digits=5)
trap_3 = Trapezoidal_rule(ll,ul,num,func)
print("Trapezoidal Rule : "); round(trap_3,digits=5)

##---------------------------------------------------------------------------##