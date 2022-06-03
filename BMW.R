#BMW model for R
#from Wynne Godley and Marc Lavoie
#Monetary Economics
#Chapter 7 

#Created by Marco Veronese Passarella on 30 May 2019

#STEP 1: Clear the workspace and define the number of periods and scenarios
rm(list=ls(all=TRUE))
#Number of periods
nPeriods = 90
#Number of scenarios
nScenarios=3 

#STEP 2: 
#Variables
#Amortization funds
af=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Consumption goods demand by households
c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Consumption goods supply
cs=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Depreciation allowances
da=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Stock of capital
k=matrix(data=200,nrow=nScenarios,ncol=nPeriods)  
#Target stock of capital
kt=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Demans for bank loans 
ld=matrix(data=200,nrow=nScenarios,ncol=nPeriods)  
#Supply of bank loans 
ls=matrix(data=200,nrow=nScenarios,ncol=nPeriods)  
#Demand for Investment
id=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Supply of Investment
is=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Bank deposits held by households
mh=matrix(data=200,nrow=nScenarios,ncol=nPeriods)
#Supply of bank deposits
ms=matrix(data=200,nrow=nScenarios,ncol=nPeriods)
#Labour
n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Labour productivity
pr=matrix(data=1,nrow=nScenarios,ncol=nPeriods) 
#Rate of interests on banks loans
rl=matrix(data=0.04,nrow=nScenarios,ncol=nPeriods)
#Rate of interests on bank loans - exogenously set
rl_bar=matrix(data=0.04,nrow=nScenarios,ncol=nPeriods)  
#Rate of interests on bank deposits
rm=matrix(data=0.04,nrow=nScenarios,ncol=nPeriods) 
#Wage rate
w=matrix(data=0.86,nrow=nScenarios,ncol=nPeriods)
#Wage Bill
wbd=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Supply of Wages
wbs=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Income
y=matrix(data=200,nrow=nScenarios,ncol=nPeriods) 
#Disposal Income of households
yd=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Set autonomous component of consumption as a matrix (for shocks) 
alpha0=matrix(data=25,nrow=nScenarios,ncol=nPeriods)
#Set autonomous propensity to consume out of income as a matrix (for shocks) 
alpha1=matrix(data=0.75,nrow=nScenarios,ncol=nPeriods)

#STEP 3: Set values for parameters and exogenous variables (excluding shocks)
alpha2=0.1         #Propensity to consume out of wealth
delta=0.1          #Depreciation rate
gamma=0.15         #Reaction speed of adjustment of capital to its target value
kappa=1            #Capital-Output ratio

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:100){
      
      #Introduce shocks
      if (i>=6 && j==2){
        alpha0[j,i]=28
      }    
      
      if (i>=6 && j==3){
        alpha1[j,i]=0.74
      }  
      
      #STEP 5: Model     
      
      #Households
      yd[j,i] = wbd[j,i] + rm[j,i-1]*mh[j,i-1]                      #Disposable income
      mh[j,i] = mh[j,i-1] + yd[j,i] - c[j,i]                        #Demand for bank deposits
      c[j,i] = alpha0[j,i] + alpha1[j,i]*yd[j,i] + alpha2*mh[j,i-1] #Consumption
      
      #Firms
      y[j,i] = c[j,i] + id[j,i]                                     #Total output (income)
      kt[j,i] = kappa*y[j,i-1]                                      #Target capital stock
      da[j,i] = delta*k[j,i-1]                                      #Depreciation allowances
      af[j,i] = da[j,i]                                             #Amortization funds
      id[j,i] = gamma*(kt[j,i] - k[j,i-1]) + da[j,i]                #Investment
      k[j,i] = k[j,i-1] + id[j,i] - da[j,i]                         #Current stock of capital
      ld[j,i] = ld[j,i-1] + id[j,i] - af[j,i]                       #Demand for bank loans
      wbd[j,i] = y[j,i] - rl[j,i-1]*ld[j,i-1] - af[j,i]             #Wage bill
      
      #Banks
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1])                   #Supply of bank loans
      ms[j,i] = ms[j,i-1] + (ls[j,i] - ls[j,i-1])                   #Supply of bank deposits
      rm[j,i] = rl[j,i]                                             #Interest rate on deposits
      rl[j,i] = rl_bar[j,i]                                         #Interest rate on loans
      
      # Wage Bill equations
      wbs[j,i] = w[j,i]*n[j,i]                                      #Supply of wages  
      n[j,i] = y[j,i]/pr[j,i]                                       #Labour demand
      w[j,i] = wbd[j,i]/n[j,i]                                      #Wage rate 
    }
  }
}

#STEP 5: Consistency check (redundant equation)
plot(mh[1,2:nPeriods]-ms[1,2:nPeriods], type="l", ylim = range(-5,5))

#STEP 6: Create and display graphs

x=c("1958":"2001")

#Figure 7.1
plot(yd[2,2:45],xaxt='n',type="l",col="1",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.1: Evolution of household disposable income and \n consumption, following an increase in autonomous \n consumption expenditures",ylab = '',xlab = '',ylim=range(180,210))
lines(c[2,2:45],type="l",lwd=2,lty=2,col="4")
#par(xpd=TRUE)
legend("bottomright",c("Disposable income","Consumption"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(1,4), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 7.2
plot(id[2,2:45],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.2: Evolution of gross investment and disposable investment, \n following an increase in autonomous consumption expenditures",ylab = '',xlab = '',ylim=range(19,24))
lines(da[2,2:45],type="l",lwd=2,lty=2,col="3")
#par(xpd=TRUE)
legend(10,21.5,c("Gross investment","Replacement investment \n (capital depreciation)"),  bty = "n", cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 7.3
plot(yd[3,2:45],xaxt='n',type="l",col="1",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.3: Evolution of household disposable income and \n consumption, following an increase in the propensity \n to save out of disposable income",ylab = '',xlab = '',ylim=range(165,180))
lines(c[3,2:45],type="l",lwd=2,lty=2,col="5")
#par(xpd=TRUE)
legend("topright",c("Disposable income","Consumption"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(1,5), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 7.4
plot(y[3,2:45]/k[3,2:45],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.4: Evolution of the output to capital ratio following \n an increase in the propensity to save out of disposable income",ylab = '',xlab = '',ylim=range(0.94,1.001))
#par(xpd=TRUE)
legend(15,0.97,c("Output to capital ratio \n (a proxy for the output \n to capacity ratio)"),  bty = "n", cex = 0.8, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)
