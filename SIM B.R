#SIM B, R version

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 3, figure 3.1

#Created by Marco Veronese Passarella on 9 October 2018

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 65)
nPeriods=65  

#NUMBER OF SCENARIOS
nScenarios=2  

#PARAMETERS
alpha1=0.6
alpha2=0.4
theta=0.2

#VARIABLES
#Government expenditures
g=matrix(data=20,nrow=nScenarios,ncol=nPeriods) 
#Income
y=matrix(data=g/theta,nrow=nScenarios,ncol=nPeriods)
#Income
y_star=matrix(data=y,nrow=nScenarios,ncol=nPeriods)
#Disposable income
yd=matrix(data=g*(1-theta)/theta,nrow=nScenarios,ncol=nPeriods)
#Consumption
c=matrix(data=yd,nrow=nScenarios,ncol=nPeriods)
#Wages
w=matrix(data=1,nrow=nScenarios,ncol=nPeriods)
#Labour 
n=matrix(data=y/w,nrow=nScenarios,ncol=nPeriods) 
#Taxes
t=matrix(data=theta*w*n,nrow=nScenarios,ncol=nPeriods)
#Cash demand
h_h=matrix(data=(1-alpha1)*yd/alpha2,nrow=nScenarios,ncol=nPeriods)
#Cash supply
h_s=matrix(data=h_h,nrow=nScenarios,ncol=nPeriods)


#Define scenario
for (j in 1:nScenarios){

#Define time
for (i in 2:nPeriods){
  
  #Define iterations
  for (iterations in 1:20){

  #Government sp. passed from 20 to 25 after 15 periods in scenario 2  
  if (i>=15 && j==2){
    g[j,i]=25
  }    
  
  #Consumption function (SIM)
  c[j,i] = alpha1*yd[j,i] + alpha2*h_h[j,i-1]
  
  #Alternative consumption function (SIMEX)
  #c[j,i] = alpha1*yd[j,i-1] + alpha2*h_h[j,i-1]
  
  #Determination of output
  y[j,i]= c[j,i] + g[j,i]
  
  #Steady state solution --- additional equation
  y_star[j,i] = g[j,i]/theta

  #Determination of employment
  n[j,i] = y[j,i]/w[j,i]
    
  #Tax payments
  t[j,i] = theta*w[j,i]*n[j,i]
  
  #Disposable income derived from accounting identity
  yd[j,i] = w[j,i]*n[j,i] - t[j,i]
  
  #Increase in cash money, as a result of government deficit
  h_s[j,i] = h_s[j,i-1] + g[j,i] - t[j,i]
  
  #Increase in cash held by households
  h_h[j,i] = h_h[j,i-1] + yd[j,i] - c[j,i]
  
  }
  
  }
}

#Tables
assign ("Tab1", (round(rbind(c[1,],y[1,],yd[1,]), digits=4)))
Table_base <- as.data.frame(Tab1,row.names=c('Consumption (c)','Income (y)','Disposable income (yd)'))

assign ("Tab2", (round(rbind(c[2,],y[2,],yd[2,]), digits=4)))
Table_shock <- as.data.frame(Tab2,row.names=c('Consumption (c)','Income (y)','Disposable income (yd)'))


#Figures
#Figure 3.1
plot(y_star[2,2:65],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.1: Impact of Y and Y* of a permanent increase in G",ylab = '',xlab = '',ylim=range(95,130))
lines(y[2,2:65],type="l",lwd=2,lty=1,col="3")
par(xpd=TRUE)
legend(10,-32,c("Steady state solution Y*","Income Y"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)
