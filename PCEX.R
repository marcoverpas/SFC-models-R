#PCEX2 model for R
#from Wynne Godley and Marc Lavoie
#Monetary Economics
#Chapter 4 

#Created by Marco Veronese Passarella on 3 June 2019

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
yd=matrix(data=90,nrow=nScenarios,ncol=nPeriods) 
#Expexted disposable income of households
yd_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Expexted wealth of households
v_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Propensity to consume out of income
alpha1=matrix(data=0.6,nrow=nScenarios,ncol=nPeriods) 
#Demand for government bills
b_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Demand for cash
h_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 


#STEP 3: Set values for parameters and exogenous variables (excluding shocks)
#alpha1=0.6
alpha2=0.4
lambda0 = 0.635
lambda1 = 5
lambda2 = 0.01
theta=0.2
alpha10 = 0.7
iota = 4

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
      
      #Consumption function - eq. 4.5e
      cons[j,i] = alpha1[j,i]*yd_e[j,i] + alpha2*v[j,i-1]
      
      #Demand for government bills - eq. 4.7e
      b_d[j,i] = v_e[j,i]*(lambda0 + lambda1*r[j,i] - lambda2*(yd_e[j,i]/v_e[j,i]))
      
      #Demand for cash money - eq. 4.13
      h_d[j,i] = v_e[j,i] - b_d[j,i]
      
      #Expected wealth - eq. 4.14
      v_e[j,i] = v[j,i-1] + (yd_e[j,i] - cons[j,i])
      
      #Cash money - eq. 4.6
      h_h[j,i] = v[j,i] - b_h[j,i]
      
      #Government bills held by households - eq. 4.15
      b_h[j,i] = b_d[j,i]
      
      #Supply of government bills - eq. 4.8
      b_s[j,i] = b_s[j,i-1] + (g[j,i] + r[j,i-1]*b_s[j,i-1]) - (t[j,i] + r[j,i-1]*b_cb[j,i-1])
      
      #Supply of cash - eq. 4.9
      h_s[j,i] = h_s[j,i-1]+b_cb[j,i]-b_cb[j,i-1]
      
      #Government bills held by the central bank - eq. 4.10
      b_cb[j,i] = b_s[j,i] - b_h[j,i]
      
      #Interest rate as policy instrument - eq. 4.11
      r[j,i] = r_bar[j,i]
      
      #Expected dispsable income - eq. 4.16
      yd_e[j,i] = yd[j,i-1]
      
      #Propensity to consume out of income - eq. 4.31
      alpha1[j,i] = alpha10 - iota*r[j,i-1]
      
      
        }
      }
    }

#STEP 5: Consistency check (redundant equation)
#plot(h_h[1,2:nPeriods]-h_s[1,2:nPeriods], type="l") #, ylim = range(-5,5))

#STEP 6: Create and display graphs

x=c("1959":"2000")

#Figure 4.9
plot(v[2,9:50],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 4.9: Evolution of GDP, C, YD and V following an increase in \n the interest rate",ylab = NA,xlab = NA,ylim=range(80,112))
lines(y[2,9:50],type="l",lwd=2,lty=2,col="4",ylab=NA, xlab=NA)
lines(yd[2,9:50],type="l",lwd=2,lty=3,col="3",ylab=NA, xlab=NA)
lines(cons[2,9:50],type="l",lwd=2,lty=4,col="1",ylab=NA, xlab=NA)
legend("bottomright",c("Household wealth","National income (GDP)","Disposable income","Consumption"),  bty = "n", cex = 0.8, lty=c(1,2,3,4), lwd=c(2,2,2,2), col = c(2,4,3,1), box.lwd=0)
axis(side=1,at=1:42,labels=x,tck=-0.05)

#Figure 4.10
plot(g[2,9:50]+r[2,8:49]*b_h[2,8:49],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 4.10: Evolution of Govt. receipts and expenditures following \n an increase in r",ylab = NA,xlab = NA,ylim=range(20.4,23.2))
lines(t[2,9:50],type="l",lwd=2,lty=2,col="4",ylab=NA, xlab=NA)
legend("bottomright",c("Govt. exp. plus net debt service","Tax revenues"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,4), box.lwd=0)
axis(side=1,at=1:42,labels=x,tck=-0.05)

