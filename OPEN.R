#OPEN Model

#Created by Marco Veronese Passarella on 2 October 2018

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 65)
nPeriods=65  

#PARAMETERS
alpha1_n = 0.6
alpha2_n = 0.4
alpha2_s = 0.3
lambda0_n = 0.635
lambda0_s = 0.67
lambda1_n = 5
lambda1_s = 6
lambda2_n = 0.01
lambda2_s = 0.07
mu_n = 0.18781
theta_n = 0.2
theta_s = 0.2

#PARAMETERS TO BE SHOCKED
mu_s=matrix(data=0.18781,nrow=3,ncol=nPeriods) 
alpha1_s=matrix(data=0.7,nrow=3,ncol=nPeriods) 

#EXOGENOUS
g_n = 20
g_s = 20

#VARIABLES
b_cb_n=matrix(data=11.622,nrow=3,ncol=nPeriods) #Bills held by the Central bank in Country N
b_cb_s=matrix(data=11.622,nrow=3,ncol=nPeriods) #Bills held by the Central bank in Country S
b_h_n=matrix(data=64.865,nrow=3,ncol=nPeriods) #Bills held by households, Country N
b_h_s=matrix(data=64.865,nrow=3,ncol=nPeriods) #Bills held by households, Country S
b_s_n=matrix(data=76.486,nrow=3,ncol=nPeriods) #Supply of government bills in Country N
b_s_s=matrix(data=76.486,nrow=3,ncol=nPeriods) #Supply of government bills in Country S
cons_n=matrix(data=0,nrow=3,ncol=nPeriods) #Households consumption, Country N
cons_s=matrix(data=0,nrow=3,ncol=nPeriods) #Households consumption, Country S
h_h_n=matrix(data=21.622,nrow=3,ncol=nPeriods) #Cash held by households, Country N
h_h_s=matrix(data=21.622,nrow=3,ncol=nPeriods) #Cash held by households, Country S
h_s_n=matrix(data=21.622,nrow=3,ncol=nPeriods) #Supply of cash in Country N
h_s_s=matrix(data=21.622,nrow=3,ncol=nPeriods) #Supply of cash in Country S
im_n=matrix(data=0,nrow=3,ncol=nPeriods) #Imports, Country N
im_s=matrix(data=0,nrow=3,ncol=nPeriods) #Imports, Country S
or_n=matrix(data=10,nrow=3,ncol=nPeriods) #Gold holding by Central bank in Country N
or_s=matrix(data=10,nrow=3,ncol=nPeriods) #Gold holding by Central bank in Country S
p_g_n=matrix(data=1,nrow=3,ncol=nPeriods) #Price of gold in Country N
p_g_s=matrix(data=1,nrow=3,ncol=nPeriods) #Price of gold in Country S
r_n=matrix(data=0.025,nrow=3,ncol=nPeriods) #Interest rate on bills in Country N
r_s=matrix(data=0.025,nrow=3,ncol=nPeriods) #Interest rate on bills in Country S
t_n=matrix(data=0,nrow=3,ncol=nPeriods) #Tax payments, Country N
t_s=matrix(data=0,nrow=3,ncol=nPeriods) #Tax payments, Country S
v_n=matrix(data=86.487,nrow=3,ncol=nPeriods) #Households wealth, Country N
v_s=matrix(data=86.487,nrow=3,ncol=nPeriods) #Households wealth, Country S
x_n=matrix(data=0,nrow=3,ncol=nPeriods) #Exports, Country N
x_s=matrix(data=0,nrow=3,ncol=nPeriods) #Exports, Country S
xr=matrix(data=1,nrow=3,ncol=nPeriods) #Exchange rate (units of currency S for one unit of currency N)
y_n=matrix(data=0,nrow=3,ncol=nPeriods) #National income, Country N
y_s=matrix(data=0,nrow=3,ncol=nPeriods) #National income, Country S
yd_n=matrix(data=0,nrow=3,ncol=nPeriods) #National disposable income, Country N
yd_s=matrix(data=0,nrow=3,ncol=nPeriods) #National disposable income, Country S


#Define (three) scenarios and time

for (j in 1:3){
  
  for (i in 2:nPeriods){
    
    if (i>=15 && j==2){   #First experiment (scenario 2) 
      mu_s[j,i]=0.20781                         
    }
    
    if (i>=15 && j==3){   #Second experiment (scenario 3)
      alpha1_s[j,i]=0.6                         
    }
    
    for (iterations in 1:20){
      
      
      #Determination of national income in Country N - eq. 6.O.1
      y_n[j,i] = cons_n[j,i] + g_n + x_n[j,i] - im_n[j,i]
      
      #Determination of national income in Country S - eq. 6.O.2
      y_s[j,i] = cons_s[j,i] + g_s + x_s[j,i] - im_s[j,i]
      
      #Imports in Country N - eq. 6.O.3
      im_n[j,i] = mu_n*y_n[j,i]
      
      #Imports in Country S - eq. 6.O.4
      im_s[j,i] = mu_s[j,i]*y_s[j,i]
      
      #Exports of Country N - eq. 6.O.5
      x_n[j,i] = im_s[j,i]/xr[j,i]
      
      #Exports of Country S - eq. 6.O.6
      x_s[j,i] = im_n[j,i]*xr[j,i]
      
      #Disposable income in Country N - eq. 6.O.7
      yd_n[j,i] = y_n[j,i] - t_n[j,i] + r_n[j,i-1]*b_h_n[j,i-1]
      
      #Disposable income in Country S - eq. 6.O.8
      yd_s[j,i] = y_s[j,i] - t_s[j,i] + r_s[j,i-1]*b_h_s[j,i-1]
      
      #Tax payments in Country N - eq. 6.O.9
      t_n[j,i] = theta_n*(y_n[j,i] + r_n[j,i-1]*b_h_n[j,i-1])
      
      #Tax payments in Country S - eq. 6.O.10
      t_s[j,i] = theta_s*(y_s[j,i] + r_s[j,i-1]*b_h_s[j,i-1])
      
      #Wealth accumulation in Country N - eq. 6.O.11
      v_n[j,i] = v_n[j,i-1] + (yd_n[j,i] - cons_n[j,i])
      
      #Wealth accumulation in Country S - eq. 6.O.12
      v_s[j,i] = v_s[j,i-1] + (yd_s[j,i] - cons_s[j,i])
      
      #Consumption function in Country N - eq. 6.O.13
      cons_n[j,i] = alpha1_n*yd_n[j,i] + alpha2_n*v_n[j,i-1]
      
      #Consumption function in Country S - eq. 6.O.14
      cons_s[j,i] = alpha1_s[j,i]*yd_s[j,i] + alpha2_s*v_s[j,i-1]
      
      #Cash money held in Country N - eq. 6.O.15
      h_h_n[j,i] = v_n[j,i] - b_h_n[j,i]
      
      #Cash money held in Country S - eq. 6.O.16
      h_h_s[j,i] = v_s[j,i] - b_h_s[j,i]
      
      #Demand for government bills in Country N - eq. 6.O.17
      b_h_n[j,i] = v_n[j,i]*(lambda0_n + lambda1_n*r_n[j,i] - lambda2_n*(yd_n[j,i]/v_n[j,i]))
      
      #Demand for government bills in Country S - eq. 6.O.18
      b_h_s[j,i] = v_s[j,i]*(lambda0_s + lambda1_s*r_s[j,i] - lambda2_s*(yd_s[j,i]/v_s[j,i]))
      
      #Supply of government bills in Country N - eq. 6.O.19
      b_s_n[j,i] = b_s_n[j,i-1] + (g_n + r_n[j,i-1]*b_s_n[j,i-1]) - (t_n[j,i] + r_n[j,i-1]*b_cb_n[j,i-1])
      
      #Supply of government bills in Country S - eq. 6.O.20
      b_s_s[j,i] = b_s_s[j,i-1] + (g_s + r_s[j,i-1]*b_s_s[j,i-1]) - (t_s[j,i] + r_s[j,i-1]*b_cb_s[j,i-1])
      
      #Bills held by Central bank in Country N - eq. 6.O.21
      b_cb_n[j,i] = b_s_n[j,i] - b_h_n[j,i]
      
      #Bills held by Central bank in Country S - eq. 6.O.22
      b_cb_s[j,i] = b_s_s[j,i] - b_h_s[j,i]
      
      #Gold holding by Central bank in Country N - eq. 6.O.23
      or_n[j,i] = or_n[j,i-1] + (h_s_n[j,i] - h_s_n[j,i-1] -(b_cb_n[j,i] - b_cb_n[j,i-1]))/p_g_n[j,i]
      
      #Gold holding by Central bank in Country S - eq. 6.O.24
      or_s[j,i] = or_s[j,i-1] + (h_s_s[j,i] - h_s_s[j,i-1] -(b_cb_s[j,i] - b_cb_s[j,i-1]))/p_g_s[j,i]
      
      #Supply of cash in Country N - eq. 6.O.25
      h_s_n[j,i] = h_h_n[j,i]
      
      #Supply of cash in Country S - eq. 6.O.26
      h_s_s[j,i] = h_h_s[j,i]
      
      #Price of gold in Country N - eq. 6.O.27
      #p_g_n[j,i] = p_g_bar
      
      #Price of gold in Country S - eq. 6.O.28
      p_g_s = p_g_n*xr
      
      #Exchange rate - eq. 6.O.29
      #xr[j,i] = xr_bar
      
      #Interest rate in Country N - eq. 6.O.30
      #r_n[j,i] = r_bar_n
      
      #Interest rate in Country S - eq. 6.O.31
      #r_s[j,i] = r_bar_s
      
      
    }
  }
}

#Figure 6.8
plot(y_n[2,5:65],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.8: [...] increase in the import propensity of the S Country",ylab = '',xlab = '',ylim=range(102,111))
lines(y_s[2,5:65],type="l",lwd=2,lty=2,col="2")
par(xpd=TRUE)
legend(10,98,c("North Country GDP","South Country GDP"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)

#Figure 6.9
plot(v_s[2,5:65]-v_s[2,4:64],type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.9: Evolution of balances in the S Country [...]",ylab = '',xlab = '',ylim=range(-1.2,0.2))
lines(t_s[2,5:65]-(g_s+r_s[2,5:65]*b_h_s[2,4:64]),type="l",lwd=2,lty=3,col="3")
lines(x_s[2,5:65]-im_s[2,5:65],type="l",lwd=2,lty=2,col="4")
par(xpd=TRUE)
legend(5,-1.6,c("Change in households wealth - S Country","Government balance with the S Country","Trade balance - S Country"),  bty = 1, cex = 0.8, lty=c(1,3,2), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)

#Figure 6.10
plot(b_cb_s[2,5:65]-b_cb_s[2,4:64],type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.10: Evolution of balances in the S Country [...]",ylab = '',xlab = '',ylim=range(-1.2,1.2))
lines(h_h_s[2,5:65]-h_h_s[2,4:64],type="l",lwd=2,lty=3,col="3")
lines(p_g_s[2,5:65]*(or_s[2,5:65]-or_s[2,4:64]),type="l",lwd=2,lty=2,col="4")
par(xpd=TRUE)
legend(-10,-1.8,c("Change in the stock of bills held by the CB - S Country","Change in the S Country stock of money","Value of the change in gold reserves of the CB - S Country"),  bty = 1, cex = 0.8, lty=c(1,3,2), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)

#Figure 6.11
plot(b_cb_s[3,5:65]-b_cb_s[3,4:64],type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.11: Evolution of the 3 components of the CB balance [...]",ylab = '',xlab = '',ylim=range(-2,2))
lines(h_h_s[3,5:65]-h_h_s[3,4:64],type="l",lwd=2,lty=3,col="3")
lines(p_g_s[3,5:65]*(or_s[3,5:65]-or_s[3,4:64]),type="l",lwd=2,lty=2,col="4")
par(xpd=TRUE)
legend(-10,-3,c("Change in the stock of bills held by the CB - S Country","Change in the S Country stock of money","Value of the change in gold reserves of the CB - S Country"),  bty = 1, cex = 0.8, lty=c(1,3,2), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
