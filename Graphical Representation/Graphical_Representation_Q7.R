## Scatter plot of the function
##install.packages("dplyr")
library(dplyr)

mn = seq(0,2*pi,by=0.01) # 'by' parameter gives the step size
group[mn < 1.5*pi]="blue"
group[mn < pi]="green"
group[mn < pi/2]="red"

df=data.frame(mn, mp, group)

plot(mn, mp, pch = 16, col = df$group, type = 'h', xlab = "x-axis", 
     ylab = "y-axis", xlim = c(0,2*pi), main = "Scatter Plot")
