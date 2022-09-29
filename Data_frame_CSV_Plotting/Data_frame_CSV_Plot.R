## IRIS dataset
library(MASS)
data(iris)
head(iris)

## finding the no of rows and columns of IRIS dataset
dim(iris)

## Summary of Sepal Length and Sepal Width
df=iris[,1:2]
summary(df)

## Types of Species and its number
summary(iris$Species)

## Another dataset from IRIS dataset with petal length >2

new_iris = subset(iris, Petal.Length > 2);new_iris
length(rownames(new_iris))

## Own dataset with 4 column and 5 rows

name=c("Ashwin","Bhuvi","Mahi","Kohli","Rohit")
weight=c(61,41,74,62,71)
height=c(162,155,142,169,168)
remark=c("All Rounder","Bowler","WK Batsman","Batsman","Batsman")

df=data.frame(cbind(name,weight,height,remark));df

write.csv(df, file="C:\\Users\\Nikunj\\Desktop\\IIT Patna\\SEM I\\R Simulation Lab\\Lab Assignments\\Assignment 5\\killer.csv")

final_csv=read.csv("C:\\Users\\Nikunj\\Desktop\\IIT Patna\\SEM I\\R Simulation Lab\\Lab Assignments\\Assignment 5\\killer.csv", header=TRUE);final_csv


## Finding the Outliers in dataset

X=c(2,4,6,10,4,7,12,20,5)
Y=c(10,5,5,20,4,70,40,12)
Z=c(2,4,2.5,34,1.6,9.5,6,2)

##Summary of the Vectors

summary(X)
summary(Y)
summary(Z)

## Defining the Remove Outlier function

remove_outlier=function(X){
  while(1){
    lower = summary(X)[2]
    upper = summary(X)[5]
    
    ## Computing the Inter-Quartile range
    IQR = upper - lower
    lim_lower = lower-1.5*IQR
    lim_upper = upper+1.5*IQR
    high=max(X)
    low=min(Y)
    cat("LL",lim_lower,"\tUL",lim_upper,"\tMax",high,"\tMin",low,"\n");cat
    if( high > lim_upper || low < lim_lower){
      X=X[X>lim_lower]
      X=X[X<lim_upper]
    }
    else
      break
    cat(X,"\n")
  }
  return(X);
  
}
## Passing the vector to the remove Outlier function

a=remove_outlier(X);
b=remove_outlier(Y);
c=remove_outlier(Z);

final = cbind(a,b,c)

## After Outliers removed,display the final result df
result= data.frame(final);result


## Histogram of the Students marks in Statistics

A1=sample(20:25,5,replace = TRUE)
A2=sample(25:30,4,replace = TRUE)
A3=sample(31:35,3,replace = TRUE)
A4=sample(36:40,4,replace = TRUE)
A5=sample(41:45,2,replace = TRUE)
A6=sample(46:50,1,replace = TRUE)

mk=c(A1,A2,A3,A4,A5,A6)
length(mk)

hist(mk, right = TRUE,main="Students marks in Statistics", xlab = "Marks in Statistics",
     ylab = "Number of students", col = "red", border = "black",
     xlim = range(20:50), ylim = range(0:10),
     axes = TRUE, plot = TRUE, labels = FALSE)

compute_mode <- function(mk) {
  tmp <- unique(mk)
  tmp[which.max(tabulate(match(mk, tmp)))]
}

mean(mk)
median(mk)
compute_mode(mk)