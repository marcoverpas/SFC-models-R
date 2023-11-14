#PC model for R

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 4, figures 4.3 and 4.4.

#Created by Marco Veronese Passarella on 30 May 2019

#STEP 1: Clear the workspace and define the number of periods and scenarios
rm(list=ls(all=TRUE))
#Number of periods
nPeriods = 90
#Number of scenarios
nScenarios=3 

#STEP 2: 
#Variables
#Government bills held by Central Bank
b_cb=matrix(data=21.576,nrow=nScenarios,ncol=nPeriods)
#Government bills held by households
b_h=matrix(data=64.865,nrow=nScenarios,ncol=nPeriods) 
#Government bills supplied by government
b_s=matrix(data=21.576+64.865,nrow=nScenarios,ncol=nPeriods) 
#Consumption goods
cons=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Government goods
g=matrix(data=20,nrow=nScenarios,ncol=nPeriods) 
#Cash money held by households
h_h=matrix(data=21.62,nrow=nScenarios,ncol=nPeriods) 
#Cash money supplied by central bank
h_s=matrix(data=21.62,nrow=nScenarios,ncol=nPeriods) 
#Interest rate on government bills
r=matrix(data=0.025,nrow=nScenarios,ncol=nPeriods) 
#Interest rate as policy instrument
r_bar=matrix(data=0.025,nrow=nScenarios,ncol=nPeriods) 
#Taxes
t=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Households wealth
v=matrix(data=64.865+21.62,nrow=nScenarios,ncol=nPeriods) 
#Income = GDP
y=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Disposable income of households
yd=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 

#STEP 3: Set values for parameters and exogenous variables (excluding shocks)
alpha1=0.6
alpha2=0.4
lambda0 = 0.635
lambda1 = 5
lambda2 = 0.01
theta=0.2

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:100){
      
      #Introduce shocks
      if (i>=10 && j==2){
        r_bar[j,i]=0.035
      }
      
      #STEP 5: Model     
      
      #Determination of output - eq. 4.1
      y[j,i] = cons[j,i] + g[j,i]
      
      #Disposable income - eq. 4.2
      yd[j,i] = y[j,i] - t[j,i] + r[j,i-1]*b_h[j,i-1]
      
      #Tax payments - eq. 4.3
      t[j,i] = theta*(y[j,i] + r[j,i-1]*b_h[j,i-1])
      
      #Wealth accumulation - eq. 4.4
      v[j,i] = v[j,i-1] + (yd[j,i] - cons[j,i])
      
      #Consumption function - eq. 4.5
      cons[j,i] = alpha1*yd[j,i] + alpha2*v[j,i-1]
      
      #Cash money - eq. 4.6
      h_h[j,i] = v[j,i] - b_h[j,i]
      
      #Demand for government bills - eq. 4.7
      b_h[j,i] = v[j,i]*(lambda0 + lambda1*r[j,i] - lambda2*(yd[j,i]/v[j,i]))
      
      #Supply of government bills - eq. 4.8
      b_s[j,i] = b_s[j,i-1] + (g[j,i] + r[j,i-1]*b_s[j,i-1]) - (t[j,i] + r[j,i-1]*b_cb[j,i-1])
      
      #Supply of cash - eq. 4.9
      h_s[j,i] = h_s[j,i-1] + b_cb[j,i] - b_cb[j,i-1]
      
      #Government bills held by the central bank - eq. 4.10
      b_cb[j,i] = b_s[j,i] - b_h[j,i]
      
      #Interest rate as policy instrument - eq. 4.11
      r[j,i] = r_bar[j,i]
      
      
        }
      }
    }

#STEP 5: Consistency check (redundant equation)

plot(h_h[1,2:nPeriods]-h_s[1,2:nPeriods], type="l", ylim = range(-5,5))

#STEP 6: Create and display graphs

x=c("1958":"2001")

#Figure 4.3
plot(h_h[2,2:45]/v[2,2:45],xaxt='n',type="l",col="3",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 4.3: Evolution of shares of bills and money balances in the \n portfolio of households, following an increase in 100 points in \n the rate of interest on bills",ylab = NA,xlab = NA,ylim=range(0.2,0.25))
par(new=T)
plot(b_h[2,2:45]/v[2,2:45],type="l",lwd=2,lty=2,col="8",axes=F, ylab=NA, xlab=NA,ylim=range(0.75,0.8))
axis(side = 4)
#par(xpd=TRUE)
legend("center",c("Share of money balances","Share of bills (right axis)"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(3,8), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 4.4
plot(yd[2,2:45],xaxt='n',type="l",col="1",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 4.4: Evolution of household disposable income and \n household consumption following an increase in 100 points in \n the rate of interest on bills",ylab = '',xlab = '',ylim=range(86,91))
lines(cons[2,2:45],type="l",lwd=2,lty=2,col="4")
#par(xpd=TRUE)
legend("bottomright",c("Disposable income","Consumption"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(1,4), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)
