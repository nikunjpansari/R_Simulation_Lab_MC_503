## Pie Chart for the AQI of Indian Cities

##install.packages("plotrix")
library(plotrix)

## Simple Pie-Chart

AQI=c(276,7,92,268,412,86)
City=c('Patna','Ratlam','Mysore','Jaunpur','Pitampura','Panchkula')
percent=round(100*AQI/sum(AQI), 1)

value=paste(percent)
value=paste(value,"%",sep="")

pie(AQI,labels=value,col=rainbow(length(AQI)),main="AQI of Indian Cities")
legend("topright",c('Patna','Ratlam','Mysore','Jaunpur','Pitampura','Panchkula'),cex=0.8,fill=rainbow(length(AQI)))

##3D Pie Chart for the AQI of Indian Cities

pie3D(AQI,labels=value,col=rainbow(length(AQI)),main="AQI of Indian Cities")
legend("topright",c('Patna','Ratlam','Mysore','Jaunpur','Pitampura','Panchkula'),cex=0.6,fill=rainbow(length(AQI)))

## Performance of Indian Players in T-20 Cricket Match

runs=c(12,0,35,35,18,33,1)
balls=c(18,1,34,29,18,17,1)
player_name=c("Rohit","KL Rahul","Kohli","Jadeja","Suryakumar","Pandya","D Kartik")

barplot(runs,width=0.20,xlab="Player Names",ylab ="Runs", main="Player performance",names.arg=player_name,col=rainbow(length(runs)))

cor(runs,balls)
cov(runs,balls)

## Score Table of Ind VS Eng Test match (2021)

reg=c("Runs","Balls","Fours")
color=c("Red","Green","Blue")
mn=matrix(c(6,9,1,29,28,5,73,173,11,11,48,0,1,6,0,91,58,9,85,138,12,31,91,2,0,12,0,4,11,1,0,2,0),nrow=3,byrow =F,ncol =11 )
player_name=c("Rohit","Shubhman","Pujara","Kohli","Rahane","Pant","Washington","Ashwin","Nadeem","Ishant","Bumrah")
barplot(mn,xlab="Player Names",ylab ="performance", main="Score Card Analyis of Indian Players",names.arg=player_name,col=color)
legend("topright",reg,cex=1.4,fill=color)

## Mathematical Function Plotting

## function 1
f_1=function(x)
  
{
  sin(x^3+5*x)
}
x=seq(-2,5,0.10)
plot(x, f_1(x), type="o",main='Mathematical Function Plotting', col="blue", pch="o", lty=1, ylim=c(-2,2),ylab="f(x)")

## function 2
f_2=function(x)
{
  cos(abs(x)+exp(x^2))
}

points(x, f_2(x), col="red", pch="*")
lines(x, f_2(x), col="red",lty=2)

## function 3
f_3=function(x){
  f_1(x)+f_2(x)
}

points(x, f_3(x), col="black",pch="+")
lines(x, f_3(x), col="black", lty=3)

legend(-2,2,legend=c("f_1(x)","f_2(x)","f_3(x)"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

## Box Plot graphs

## mt cars dataset consisting of 32 different cars and 11 different attributes
data(mtcars)
## Define the information of first 6 rows of the dataset
head(mtcars) 

mpgl = mtcars[,1]
cyl= mtcars[,2]

boxplot(mpgl ~ cyl, data = mtcars,main = "Mileage Data", xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon")

## Histogram of 50 random numbers

value=sample(1:99, 50);value
hist( value,xlim = c(1,99), ylim = c(0,10),main="Random Integer Plot", xlab = "Random Integers", col = "red", border = "blue",breaks = 10)

