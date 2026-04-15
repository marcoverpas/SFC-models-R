#+++++++++++++++++++++++++++++++++++

#Hybrid PC + BMW model 

#Created by Marco Veronese Passarella

#Last revision: 15/04/2026

#+++++++++++++++++++++++++++++++++++

#Set the environment ####
rm(list=ls(all=TRUE))     #Clear the workspace 
nPeriods = 90             #Define number of periods
nScenarios=3              #Define number of scenarios

#+++++++++++++++++++++++++++++++++++

#Set values for parameters and exogenous variables (excluding shocks)
alpha1<-0.6         #Propensity to consume out of income 
alpha2<-0.4         #Propensity to consume out of wealth
lambda10<-0.3       #Portfolio equation coefficient
lambda11<-0         #Portfolio equation coefficient
lambda12<-0         #Portfolio equation coefficient
lambda13<-0         #Portfolio equation coefficient
lambda_c<-0.2       #Wealth to consumption ratio
theta<-0.2          #Average tax rate on income  
delta<-0.1          #Depreciation rate
gamma<-0.15         #Reaction speed of adjustment of capital to its target value
kappa<-0.3          #Capital-Output ratio

#+++++++++++++++++++++++++++++++++++

#Define model variables ####

#Government bills held by Central Bank
b_cb<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Government bills held by households
b_h<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Government bills held by banks
b_b<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Government bills supplied by government
b_s<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Consumption goods
cons<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Government goods
g<-matrix(data=20,nrow=nScenarios,ncol=nPeriods) 
#Cash money held by households
h_h<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Cash money supplied by central bank
h_s<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Interest rate on government bills
rb<-matrix(data=0.02,nrow=nScenarios,ncol=nPeriods) 
#Interest rate on deposits
rm<-matrix(data=0.02,nrow=nScenarios,ncol=nPeriods) 
#Interest rate on loans
rl<-matrix(data=0.02,nrow=nScenarios,ncol=nPeriods) 
#Taxes
t<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Households wealth
v<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Income = GDP
y<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Disposable income of households
yd<-matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Amortization funds
af<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Depreciation allowances
da<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Stock of capital
k<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)  
#Target stock of capital
kt<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Demans for bank loans 
ld<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)  
#Supply of bank loans 
ls<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)  
#Demand for Investment
id<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Bank deposits held by households
mh<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Supply of bank deposits
ms<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)

#+++++++++++++++++++++++++++++++++++

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:30){
      
      #Run the model ####     
      
      #Determination of output
      y[j,i] = cons[j,i] + g[j,i] + id[j,i]
      
      #Disposable income
      yd[j,i] = y[j,i] - t[j,i] - af[j,i] - rl[j,i-1]*ld[j,i-1] + rb[j,i-1]*b_h[j,i-1] + rm[j,i-1]*mh[j,i-1]
      
      #Tax payments
      t[j,i] = theta*(y[j,i] + rb[j,i-1]*b_h[j,i-1] + rm[j,i-1]*mh[j,i-1]) 
      
      #Wealth accumulation
      v[j,i] = v[j,i-1] + (yd[j,i] - cons[j,i])
      
      #Consumption function
      cons[j,i] = alpha1*yd[j,i] + alpha2*v[j,i-1]
      
      #Cash money as function of expected transactions
      h_h[j,i] = lambda_c * cons[j,i-1]
      
      #Demand for government bills
      b_h[j,i] = v[j,i]*(lambda10 + lambda11*rb[j,i] - lambda12*rm[j,i] - lambda13*(yd[j,i]/v[j,i]))
      
      #Supply of government bills
      b_s[j,i] = b_s[j,i-1] + (g[j,i] + rb[j,i-1]*b_s[j,i-1]) - (t[j,i] + rb[j,i-1]*b_cb[j,i-1])
      
      #Supply of cash
      h_s[j,i] = h_s[j,i-1] + b_cb[j,i] - b_cb[j,i-1]
      
      #Government bills held by the central bank
      b_cb[j,i] = b_s[j,i] - b_h[j,i] - b_b[j,i]
      
      #Target capital stock
      kt[j,i] = kappa*y[j,i-1]
      
      #Depreciation allowances
      da[j,i] = delta*k[j,i-1]
      
      #Amortization funds
      af[j,i] = da[j,i]
      
      #Gross investment
      id[j,i] = gamma*(kt[j,i] - k[j,i-1]) + da[j,i]
      
      #Capital stock
      k[j,i] = k[j,i-1] + id[j,i] - da[j,i]
      
      #Loans
      ld[j,i] = ld[j,i-1] + id[j,i] - af[j,i]
      
      #Banks
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1]) 
      ms[j,i] = ms[j,i-1] + (ls[j,i] - ls[j,i-1])
      
      #Deposits as buffer stock
      mh[j,i] = v[j,i] - b_h[j,i] - h_h[j,i]
      
      #Bills held by banks (if negative then b_b = advances received from CB) ***
      b_b[j,i] = mh[j,i] - ms[j,i]
      
    }
  }
}

#+++++++++++++++++++++++++++++++++++

#Consistency check (redundant equation) ####
plot(h_h[1,2:nPeriods]-h_s[1,2:nPeriods], type="l",lwd=2,lty=1,
     font.main=1,cex.main=1,
     main="Consistency check",ylab = '',xlab = '', ylim = range(-1,1))
lines(mh[1,2:nPeriods]-ms[1,2:nPeriods]-b_b[1,2:nPeriods],type="l",
      lwd=2,lty=2, col=2)
legend("topright",c("Cash: hh-hs","Deposits: mh-ms-bb"),bty="n",cex=1,
       lty=c(1,2),lwd=c(2,2),col=c(1,2),box.lwd=0)

#+++++++++++++++++++++++++++++++++++

#Create and display graphs ####

#Define layout
layout(matrix(c(1,2), 1, 2, byrow = TRUE))

#Figure 1
plot(yd[1,2:nPeriods],type="l",col="1",lwd=2,
     lty=1,font.main=1,cex.main=1,
     main="Fig. 1. Household income and consumption",
     ylab='$',xlab='Time',ylim=range(0,90))
lines(cons[1,2:nPeriods],type="l",lwd=2,lty=2,col="4")
legend("bottomright",c("Disposable income","Consumption"),bty="n",cex=1,
       lty=c(1,2),lwd=c(2,2),col=c(1,4),box.lwd=0)

#Figure 2
plot(id[1,2:nPeriods],type="l",col="orange",lwd=2,
     lty=1,font.main=1,cex.main=1,
     main="Fig. 2. Investment and capital stock",
     ylab='$',xlab='Time',ylim=range(0,40))
lines(k[1,2:nPeriods],type="l",lwd=2,lty=2,col=2)
legend("right",c("Investment","Capital stock"),bty="n",cex=1,
       lty=c(1,2),lwd=c(2,2),col=c("orange",2),box.lwd=0)