## Total Medal dataset

medal=read.csv("C:\\Users\\Nikunj\\Desktop\\IIT Patna\\SEM I\\R Simulation Lab\\Lab Assignments\\Assignment 5\\medals_total.csv", header=TRUE)

## Total no of medals won by India,USA and China

total = subset(medal, medal$Country.Code == c("USA","CHN","IND"), select = c("Country","Gold.Medal","Silver.Medal","Bronze.Medal"));total

km = subset(medal, medal$Country.Code == c("USA","CHN","IND"), select = c("Country","Gold.Medal","Silver.Medal","Bronze.Medal"));km

km_China= subset(medal, medal$Country.Code == "CHN",select = c("Gold.Medal","Silver.Medal","Bronze.Medal"));km_China

x = rep(1,times=km_China$Gold.Medal)
y = rep(2,times=km_China$Silver.Medal)
z= rep(3,times=km_China$Bronze.Medal)



mdl = c(x,y,z)

## Histogram for medals won by China

hist(mdl, col = "red", border = "green",
     main = paste("Medals won by China"),
     xlim = range(0:4), ylim = range(0:40),
     xlab = "Medals\n 1:Gold      2 : Silver     3 : Bronze ", ylab = "Frequency",
     axes = TRUE, plot = TRUE)





km_UK= subset(medal, medal$Country.Code == "GBR",select = c("Gold.Medal","Silver.Medal","Bronze.Medal"));km_UK



p = rep(1,times=km_UK$Gold.Medal)


q = rep(2,times=km_UK$Silver.Medal)
r = rep(3,times=km_UK$Bronze.Medal)

mdl_2 = c(p,q,r)

## Histogram for medals won by UK

hist(mdl_2, col = "violet", border = "black",
     main = paste("Medals won by UK"),
     xlim = range(0:4), ylim = range(0:25),
     xlab = "Medals\n 1:Gold      2 : Silver     3 : Bronze ", ylab = "Frequency",
     axes = TRUE, plot = TRUE)

## Filtering the data for the 5 countries only

second = medal[c(48,1,3,2,12),1:7];second

## Using the dataset to make the pie chart
uj = medal[1:5,6]
n = length(medal)
pie(uj, labels = second[1:5,7], edges = 500, radius = 0.8, col = NULL, border = "green"
    , main = "Country-wise Medal Distribution")
## AirPassenger dataset

AirPassengers
cat("Total no of Passengers :",sum(AirPassengers))

kj = (AirPassengers)


kl_sum = c()
kl = as.numeric(1949:1960)

p = 1

for (i in 1:12) {
  s=0
  for (j in 1:12) {
    s = s + kj[p]
    p=p+1
  }
  kl_sum[i] = s 
}

plot(kl, kl_sum,main="Scatter Plot between Year and No of Passengers", xlab="Year", ylab="Number of Passengers", xlim = range(1949:1960), ylim = range(1000:6000),col = "red", type = "b")

## Boxplot for the no of passengers for each months

boxplot(kj[1:12],main="Passengers in January",col = "grey",xlab = "January", ylab = "No. of passengers")
boxplot(kj[13:24],main="Passengers in Feburary",col = "grey",xlab = "Feburary", ylab = "No. of passengers")
boxplot(kj[25:36],main="Passengers in March",col = "grey",xlab = "March", ylab = "No. of passengers")
boxplot(kj[37:48],main="Passengers in April",col = "grey",xlab = "April", ylab = "No. of passengers")
boxplot(kj[49:60],main="Passengers in May",col = "grey",xlab = "May", ylab = "No. of passengers")
boxplot(kj[61:72],main="Passengers in June",col = "grey",xlab = "June", ylab = "No. of passengers")
boxplot(kj[73:84],main="Passengers in July",col = "grey",xlab = "July", ylab = "No. of passengers")
boxplot(kj[85:96],main="Passengers in August", col = "grey",xlab = "August", ylab = "No. of passengers")

boxplot(kj[97:108],main="Passengers in September", col = "grey",xlab = "September", ylab = "No. of passengers")
boxplot(kj[109:120],main="Passengers in October",col = "grey",xlab = "October", ylab = "No. of passengers")
boxplot(kj[121:132],main="Passengers in November",col = "grey",xlab = "November", ylab = "No. of passengers")
boxplot(kj[133:144],main="Passengers in December",col = "grey",xlab = "December", ylab = "No. of passengers")

