## Chi-Square Test for BUrr X distrbution

km = c(.70 ,.84 ,.58 ,.50 ,.55 ,.82 ,.59 ,.71 ,.72 ,.61 ,.62 ,.49 ,.54 
       ,.36 ,.36 ,.71 ,.35 ,.64 ,.85 ,.55 ,.59 ,.29 ,.75 ,.46 ,.46 ,.60 
       ,.60 ,.36 ,.52 ,.68 ,.80 ,.55 ,.84,.34 ,.34 ,.70 ,.49 ,.56 ,.71 
       ,.61 ,.57 ,.73 ,.75 ,.44 ,.44 ,.81,.80,.87,.29,.50)

g = sort(km);g
n = length(g)

c=5
k=8.2680
t=c()
for(i in 1:n){
  t[i] = (1 - exp(-(c*g[i]^2)))^k
}

t
em_1 = (1:n)/n;em_1
em_2 = (0:n-1)/n;em_2

for(i in 1:n){
  d1[i] = max(em_1[i] - t[i])
  d2[i] = max(t[i] -em_2[i])
}

d=max(d1,d2);d
if(d > 0.1884){
  print("Hypothesis Rejected!!")
}else
{print("Hypothesis Accepted!!")
}

##Using KS Test
BurrX_KS = function(z){
  return (1 - exp(-(c*z^2)))^k
}
ks.test(km,BurrX_KS)


## Chi-Square Test for BUrr XII distrbution

km_1 = c(.70 ,.84 ,.58 ,.50 ,.55 ,.82 ,.59 ,.71 ,.72 ,.61 ,.62 ,.49 ,.54 
       ,.36 ,.36 ,.71 ,.35 ,.64 ,.85 ,.55 ,.59 ,.29 ,.75 ,.46 ,.46 ,.60 
       ,.60 ,.36 ,.52 ,.68 ,.80 ,.55 ,.84,.34 ,.34 ,.70 ,.49 ,.56 ,.71 
       ,.61 ,.57 ,.73 ,.75 ,.44 ,.44 ,.81,.80,.87,.29,.50)

g_1 = sort(km_1);g_1
n_1 = length(g_1)

c=5
k=8.2680
t_1=c()
for(i in 1:n){
  t_1[i] = (1 -(1 + g[i]^c)^(-k))
}

t_1
em_11 = (1:n_1)/n_1;em_11
em_22 = (0:n_1-1)/n_1;em_11

for(i in 1:n_1){
  d1[i] = max(em_11[i] - t_1[i])
  d2[i] = max(t_1[i] -em_11[i])
}

d_Burr_12=max(d1,d2);d_Burr_12
if(d_Burr_12 > 0.1884){
  print("Hypothesis Rejected!!")
}else
{print("Hypothesis Accepted!!")
}

##Using KS Test
BurrXll_KS = function(z){
  return (1 - (1+z^c)^(-k))
}
ks.test(km,BurrXll_KS)

