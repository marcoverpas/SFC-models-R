#SIM Model, R version

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 3, figures 3.2 and 3.3.

#Created by Mrco Veronese Passarella on 2 October 2018

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 65)
nPeriods=65  

#PARAMETERS
alpha1=0.6
alpha2=0.4
theta=0.2

#VARIABLES
#Income
y=matrix(data=0,nrow=1,ncol=nPeriods)
#Income
y_star=matrix(data=0,nrow=1,ncol=nPeriods)
#Consumption
c=matrix(data=0,nrow=1,ncol=nPeriods)
#Government expenditures
g=matrix(data=0,nrow=1,ncol=nPeriods) 
#Taxes
t=matrix(data=0,nrow=1,ncol=nPeriods)
#Disposable income
yd=matrix(data=0,nrow=1,ncol=nPeriods)
#Cash demand
h_h=matrix(data=0,nrow=1,ncol=nPeriods)
#Cash supply
h_s=matrix(data=0,nrow=1,ncol=nPeriods)
#Labour 
n=matrix(data=0,nrow=1,ncol=nPeriods) 
#Wages
w=matrix(data=1,nrow=1,ncol=nPeriods)

#MODEL

#Define time
for (i in 2:nPeriods){
  
  for (iterations in 1:20){

  if (i>=15){
    g[,i]=20   #Government expenditures passed from 0 to 20 after 15 periods
  }    
  
  
  #Consumption function (SIM)
  c[,i] = alpha1*yd[,i] + alpha2*h_h[,i-1]
  
  #Alternative consumption function (SIMEX)
  #c[,i] = alpha1*yd[,i-1] + alpha2*h_h[,i-1]
  
  #Determination of output
  y[,i]= c[,i] + g[,i]
  
  #Steady state solution --- additional equation
  y_star[,i] = g[,i]/theta

  #Determination of employment
  n[,i] = y[,i]/w[,i]
    
  #Tax payments
  t[,i] = theta*w[,i]*n[,i]
  
  #Disposable income derived from accounting identity
  yd[,i] = w[,i]*n[,i] - t[,i]
  
  #Increase in cash money, as a result of government deficit
  h_s[,i] = h_s[,i-1] + g[,i] - t[,i]
  
  #Increase in cash held by households
  h_h[,i] = h_h[,i-1] + yd[,i] - c[,i]
  
  }
}
  
#Figure 3.1
#plot(y_star[,2:65],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.1: Impact of Y and Y* of a permanent increase in G",ylab = '',xlab = '',ylim=range(0,130))
#lines(y[,2:65],type="l",lwd=2,lty=1,col="3")
#par(xpd=TRUE)
#legend(-2,-32,c("Steady state solution Y*","Income Y"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)

#Figure 3.2
plot(yd[,2:65],type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.2: YD and C starting from scratch",ylab = '',xlab = '',ylim=range(0,90))
lines(c[,2:65],type="l",lwd=2,lty=1,col="3")
#abline(h=80,col=1,lty=1,lwd=1)
segments(x0=-3, # Value from x (initial)
         x1=66, # Value to x (final)
         y0=80, # Value from y (initial)
         y1=80, # Value to y (final)
         col=1,lty=2,lwd=1)
par(xpd=TRUE)
legend(-2,-22,c("Income YD","Consumption C"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(2,3), box.lwd=0)

#Figure 3.3
plot(h_h[,2:65],type="l",lwd=2,lty=1,col="4",font.main=1,cex.main=0.75,main="Figure 3.3: Wealth level and wealth change, starting from scratch",ylab = '',xlab = '')
par(new="TRUE")
plot(diff(h_h[,2:65]),type="l",lwd=2,lty=1,col="2",xlab = '',ylab = '',xaxt='n',yaxt='n')
axis(side=4)
par(xpd=TRUE)
legend(-2,-4,c("Wealth level H (money stock)","Household saving (the change in H)"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,2), box.lwd=0)
