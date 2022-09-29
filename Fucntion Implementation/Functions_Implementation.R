## Matrix Multiplication

A = matrix(c(3,-1,-2,4,1,-2), nrow = 2); A
B = matrix(c(-7,9,2,4,5,-1), nrow = 3); B

multiplication_matrix <- function( X, Y){
  Z = matrix(c(0,0,0,0), nrow =2, ncol = 2)
  for(i in 1:2){  
    for(j in 1:2){ 
      for(k in 1:3){ 
        Z[i,j] = Z[i,j] + X[i,k]*Y[k,j]
      }
    }
  }
  Z
}

AB = multiplication_matrix(A,B); AB

## Matrix Transpose

transpose <- function(X){
  temp = 0
  for(i in 1:2){
    for(j in i:2){
      temp = X[i,j]
      X[i,j] = X[j,i]
      X[j,i] = temp
    }
  }
  X
}

AB_transpose = transpose(AB); AB_transpose

## Matrix Inverse

Inverse <- function(X){
  adj_X = matrix(c( X[2,2], -X[2,1], -X[1,2], X[1,1]), nrow = 2)
  det_X = X[1,1]*X[2,2]-X[1,2]*X[2,1]
  X_inverse = adj_X/det_X
}

AB_inverse = Inverse(AB); AB_inverse

## For Mean and Standard Deviation

M_SD <- function(num){
  n = length(num)
  meanVec = sum(num)/n
  print("Mean : ")
  print(meanVec)
  sd = sqrt((sum((num-meanVec)*(num-meanVec)))/(n-1))
  print("Standard Deviation : ")
  print(sd)
}

## Row and Column-wise Mean & SD of A

M_SD(A[1,])
M_SD(A[2,])

M_SD(A[,1])
M_SD(A[,2])
M_SD(A[,3])

## Row and Column-wise Mean & SD of A

M_SD(B[1,])
M_SD(B[2,])
M_SD(B[3,])

M_SD(B[,1])
M_SD(B[,2])

## Row and Column-wise Mean & SD of AB

M_SD(AB[1,])
M_SD(AB[2,])

M_SD(AB[,1])
M_SD(AB[,2])

## Row and Column-wise Mean & SD of Transpose of AB

M_SD(AB_transpose[1,])
M_SD(AB_transpose[2,])

M_SD(AB_transpose[,1])
M_SD(AB_transpose[,2])


## Row and Column-wise Mean & SD of Inverse of AB

M_SD(AB_inverse[1,])
M_SD(AB_inverse[2,])

M_SD(AB_inverse[,1])
M_SD(AB_inverse[,2])

##---------------------------------------------------##

## Factorial of a number

fact <- function(n){
  result = 1
  if(n==0 || n==1){
    result = 1
  }
  else{
    for(i in 2:n){
      result = result*i
    }
  }
  print(result)
}

fact(13)
fact(32)

##-------------------------------------------------##

## Find MAX and MIN from a given set of numbers

MAX_MIN <- function(vec){
  mx = mn = vec[1]
  for(i in vec){
    if(i > mx){
      mx = i
    }
    if(i < mn){
      mn = i
    }
  }
  print("Max :")
  print(mx)
  print("Min :")
  print(mn)
}

n = c(-4, 44.7, -2, 4, 54, 1, -3, 4)
MAX_MIN(n)

##-------------------------------------------------------##

## Sort the numbers

num_sort <- function(num){
  n = length(num)
  for(i in 1:(n-1)){
    swapped = FALSE
    for(j in 1:(n-i)){
      if(num[j] > num[j+1]){
        temp = num[j]
        num[j] = num[j+1]
        num[j+1] = temp
        swapped = TRUE;
      }
    }
    if(swapped == FALSE)
      break
  }
  return(num)
}

n = c(-4, 44.7, -2, 4, 54, 1, -3, 4)
num_sort(n)

##------------------------------------------------------------##

## Check whether a number is prime or composite 

Prime <- function(a){
  if(a == 0 || a == 1){
    print("Neither prime nor composite")
  }
  else{
    for(i in 2:sqrt(a)){
      if(a%%i == 0){
        return("Composite")
        break
      }
      else
        return("Prime")
    }
  }
}

Prime(5)
Prime(1)
Prime(0)
Prime(8)

##---------------------------------------------------##

## Computing the GAMMA Function

GAMMA <- function(n){
  if(n == 1){
    return(1)
  }
  else if(n == 0.5){
    return(sqrt(3.14159))
  }
  else{
    return((n-1)*GAMMA(n-1))
  }
}

GAMMA(8)
gamma(8)

GAMMA(2)
gamma(2)

GAMMA(25)
gamma(25)

GAMMA(3/2)
gamma(3/2)