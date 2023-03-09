################################################################################
#
# R code for OPENSIME model using "sfcr" package
#
# Authors: Emilio Carnevali and Marco Veronese Passarella
#
# Last change: 9 March 2023 
#
# Note: this code replicates the OPENSIME model presented and used in E. Carnevali,
# "A New, Simple SFC Open Economy Framework", Review of Political Economy, 2021,
# DOI: https://doi.org/10.1080/09538259.2021.1899518
#
################################################################################

#CLEAR ALL

# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

#Upload libraries
library(sfcr)
library(tidyverse)

################################################################################

#CREATE AND DEFINE VARIABLES AND PARAMETERS

#Define model coefficients
openSime_ext <- sfcr_set(
  
  #Parameters
  alpha1_uk ~ 0.75,
  alpha1_us ~ 0.75,
  alpha2_uk ~ 0.13333,
  alpha2_us ~ 0.13333,
  eps0 ~ -2.1,
  eps1 ~ 0.7,
  eps2 ~ 1.228,
  lambda10 ~ 0.7,
  lambda11 ~ 5,
  lambda12 ~ 5,
  lambda20 ~ 0.25,
  lambda21 ~ 5,
  lambda22 ~ 5,
  lambda40 ~ 0.7,
  lambda41 ~ 5,
  lambda42 ~ 5,
  lambda50 ~ 0.25,
  lambda51 ~ 5,
  lambda52 ~ 5,
  mu0 ~ -2.1,
  mu1 ~ 0.7,
  mu2 ~ 1.228,
  theta_uk ~ 0.2,
  theta_us ~ 0.2,
  
  # Exogenous variables
  g_uk ~ 16,
  g_us ~ 16,
  r_uk ~ 0.03,
  r_us ~ 0.03,
  
)

#Define initial values of variables
openSime_init <- sfcr_set(
  
  #Endogenous variables
  b_ukuk_d ~ 106.8396, # Bills issued by the UK acquired by the UK: demand
  b_ukuk_s ~ 106.8396, # Bills issued by the UK acquired by the UK: supply
  b_cb_ukuk_s ~ 7.631402, # Bills issued by the UK, supplied to the UK Central bank
  b_uk_s ~ 152.628, # Bills issued by the UK - total supply
  b_ukus_d ~ 38.15701, # Bills issued by the US acquired by the UK: demand
  b_ukus_s ~ 38.15701, # Bills issued by the US acquired by the UK: supply
  b_usuk_d ~ 38.15701, # Bills issued by the UK acquired by the US: demand
  b_usuk_s ~ 38.15701, # Bills issued by the UK acquired by the US: supply
  b_us_s ~ 152.628, # Bills issued by the US - total supply
  b_usus_d ~ 106.8396, # Bills issued by the US acquired by the US: demand
  b_usus_s ~ 106.8396, # Bills issued by the US acquired by the US: supply
  b_cb_usus_s ~ 7.631402, # Bills issued by the US supplied to the US Central Bank
  cab_uk ~ 0, # Current account balance in the UK
  cab_us ~ 0, # Current account balance in the US
  cons_uk ~ 81.39959, # Consumption in the UK
  cons_us ~ 81.39959, # Consumption in the US
  f_cb_uk ~ 81.39959, # Profits of Central Bank in the UK
  f_cb_us ~ 81.39959, # Profits of Central Bank in the US
  h_uk_s ~ 7.631402, # Supply of the UK cash
  h_us_s ~ 7.631402, # Supply of the US cash
  im_uk ~ 33.87894, # Imports of the UK from the US
  im_us ~ 33.87894, # Imports of the US from the UK
  psbr_uk ~ 8.61602e-07, # Government deficit in the UK
  psbr_us ~ 8.61602e-07, # Government deficit in the US
  t_uk ~ 20.3499, # Tax revenue in the UK
  t_us ~ 20.3499, # Tax revenue in the US
  v_uk ~ 152.628, # Net financial assets of the UK
  v_us ~ 152.628, # Net financial assets of the US
  x_uk ~ 33.87894, # Exports from the UK to the US
  x_us ~ 33.87894, # Exports from the US to the UK
  xr_uk ~ 1, # Exchange rate: units of dollars against 1 unit of pound
  xr_us ~ 1, # Exchange rate: units of pounds against 1 unit of dollar
  yd_uk ~ 81.39959, # Disposable income in the UK
  yd_us ~ 81.39959, # Disposable income in the US
  yd_hs_uk ~ 81.39959, # Haig-Simons disposable income in the UK
  yd_hs_us ~ 81.39959, # Haig-Simons disposable income in the US
  y_uk ~ 97.39959, # Income in the UK
  y_us ~ 97.39959, # Income in the US
  h_uk_h ~ 7.631402, # Holding of money in Uk 
  h_us_h ~ 7.631402, # Holding of money in Us
  tb_uk ~ 0, # Trade balance UK
  tb_us ~ 0, # Trade balance US
  bp_uk ~ 0, # UK Balance of payment
  bp_us ~ 0, # US Balance of payment
  nafa_uk ~ 8.61602e-07, # UK Net accumulation of financial assets
  nafa_us ~ 8.61602e-07, # US Net accumulation of financial assets
  kabp_uk ~ 0, # UK Financial account balance
  kabp_us ~ 0, #US Financial account balance
)

################################################################################

#CREATE THE MODEL

#Define model equations
openSime_eqs <- sfcr_set(
  
  # 1. INCOME AND WEALTH
  
  # Disposable income in UK - eq. 1
  yd_uk ~ (y_uk + r_uk[-1]*b_ukuk_s[-1] + xr_us[-1]*r_us[-1]*b_ukus_s[-1])*(1 - theta_uk),
  
  # Haig-Simons disposable income in UK - eq. 2
  yd_hs_uk ~ yd_uk + (xr_us-xr_us[-1])*b_ukus_s[-1],
  
  # Wealth accumulation in UK - eq. 3
  v_uk ~ v_uk[-1] + yd_hs_uk - cons_uk,
  
  # Disposable income in US - eq. 4
  yd_us ~ (y_us + r_us[-1]*b_usus_s[-1] + xr_uk[-1]*r_uk[-1]*b_usuk_s[-1])*(1 - theta_us),
  
  # Haig-Simons disposable income in US - eq. 5
  yd_hs_us ~ yd_us + (xr_uk-xr_uk[-1])*b_usuk_s[-1],
  
  # Wealth accumulation in US - eq. 6
  v_us ~ v_us[-1] + yd_hs_us - cons_us,
  
  # Taxes in UK - eq. 7
  t_uk ~ theta_uk*(y_uk + r_uk[-1]*b_ukuk_d[-1] + xr_us[-1]*r_us[-1]*b_ukus_s[-1]),
  
  # Taxes in US - eq. 8
  t_us ~ theta_us*(y_us + r_us[-1]*b_usus_d[-1] + xr_uk[-1]*r_uk[-1]*b_usuk_s[-1]),
  
  # Income UK - eq. 9
  y_uk ~ cons_uk + g_uk + x_uk - im_uk,
  
  # Income US - eq. 10
  y_us ~ cons_us + g_us + x_us - im_us,
  
  # consumption in UK - eq. 11
  cons_uk ~ alpha1_uk*yd_hs_uk + alpha2_uk*v_uk[-1],
  
  #consumption in US - eq. 12
  cons_us ~ alpha1_us*yd_hs_us + alpha2_us*v_us[-1],
  
  #2. TRADE
  
  # Exports from UK - eq. 13
  x_uk ~ exp(eps0 - eps1*log(xr_uk[-1]) + eps2*log(y_us)),
  
  # Imports of UK - eq. 14
  im_uk ~ exp(mu0 + mu1*log(xr_uk[-1]) + mu2*log(y_uk)),
  
  # Exports of US - eq. 15
  x_us ~ im_uk*xr_uk,
  
  # Imports of US - eq. 16
  im_us ~ x_uk*xr_uk,
  
  #3. ASSET DEMANDS
  
  # Demand for UK bills in UK - eq. 17
  b_ukuk_d ~ v_uk*(lambda10 + lambda11*r_uk - lambda12*r_us),
  
  # Demand for US bills in UK - eq. 18
  b_ukus_d ~ v_uk*(lambda20 - lambda21*r_uk + lambda22*r_us),
  
  # Demand for US	bills in US - eq. 19
  b_usus_d ~ v_us*(lambda40 + lambda41*r_us - lambda42*r_uk),
  
  # Demand for UK bills in US - eq. 20
  b_usuk_d ~ v_us*(lambda50 - lambda51*r_us + lambda52*r_uk),
  
  # Holding of money in UK - eq. 21
  h_uk_h ~ v_uk - b_ukuk_s - (b_ukus_s*xr_us),
  
  # Holding of money in US - eq. 22
  h_us_h ~ v_us - b_usus_s - (b_usuk_s*xr_uk),
  
  #4. ASSET SUPPLIES
  
  # Supply of UK bills to UK households - eq. 23
  b_ukuk_s ~ b_ukuk_d,
  
  # Supply of US bills to US households - eq. 24
  b_usus_s ~ b_usus_d,
  
  # Supply of UK bills to US households - eq. 25
  b_usuk_s ~ b_usuk_d*xr_us,
  
  # Supply of US bills to UK households - eq. 26
  b_ukus_s ~ b_ukus_d*xr_uk,
  
  #5. PUBLIC SECTOR
  
  #Supply of US bills to US Central bank - eq. 27 (central bank lander of last resort)
  b_cb_usus_s ~ b_us_s - b_usus_s - b_ukus_s,
  
  #Supply of UK bills to UK Central bank - eq. 28
  b_cb_ukuk_s ~ b_uk_s - b_ukuk_s - b_usuk_s,
  
  # Suply of cash in UK - eq. 29
  h_uk_s ~ b_cb_ukuk_s,
  
  # Suply of cash in US - eq. 30
  h_us_s ~ b_cb_usus_s,
  
  # Profits of Central Bank in UK - eq. 31
  f_cb_uk ~ r_uk[-1]*b_cb_ukuk_s[-1], 
  
  # Profits of Central Bank in US - eq. 32
  f_cb_us ~ r_us[-1]*b_cb_usus_s[-1],
  
  # Government budget constraint - UK - eq. 33
  b_uk_s ~ b_uk_s[-1] + g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk,
  
  # Government budget constraint - US - eq. 34
  b_us_s ~ b_us_s[-1] + g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us,
  
  #6. EXCHANGE RATES 
  
  # UK Exchange rate - eq. 35
  xr_us ~ ((r_uk[-1]*b_usuk_s[-1] - (b_usuk_s-b_usuk_s[-1]) - x_uk + im_uk)/(r_us[-1]*b_ukus_s[-1] - (b_ukus_s-b_ukus_s[-1]))),
  
  #US exchange rate - eq. 36
  xr_uk ~ 1/xr_us,
  
  #7. OTHER CALCULATIONS 
  
  # Government deficit in the UK - eq. 39
  psbr_uk ~ g_uk + r_uk[-1]*b_uk_s[-1] - t_uk - f_cb_uk,
  
  # Government deficit in the US - eq. 40
  psbr_us ~ g_us + r_us[-1]*b_us_s[-1] - t_us - f_cb_us,
  
  # Net accumulation of financial assets in the UK - eq. 41
  nafa_uk ~ psbr_uk + cab_uk,
  
  # Net accumulation of financial assets in the US - eq. 42
  nafa_us ~ psbr_us + cab_us,
  
  # Current account balance - UK - eq. 43
  cab_uk ~ x_uk - im_uk + xr_us[-1]*r_us[-1]*b_ukus_s[-1] - r_uk[-1]*b_usuk_s[-1] ,
  
  # Current account balance in US - eq. 44
  cab_us ~ x_us - im_us + xr_uk[-1]*r_uk[-1]*b_usuk_s[-1] - r_us[-1]*b_ukus_s[-1],
  
  # Financial account balance in UK - eq. 45
  kabp_uk ~ - (b_ukus_s-b_ukus_s[-1])*xr_us + (b_usuk_s-b_usuk_s[-1]),
  
  # Financial account balance in US - eq. 46
  kabp_us ~ - (b_usuk_s-b_usuk_s[-1])*xr_uk + (b_ukus_s-b_ukus_s[-1]),
  
  #Trade balance UK - eq. 47
  tb_uk ~ x_uk - im_uk,
  
  #Trade balance US - eq. 48
  tb_us ~ x_us - im_us,
  
  #Balance of payment UK - eq. 49
  bp_uk ~ cab_uk + kabp_uk,
  
  #Balance of payment US - eq. 50
  bp_us ~ cab_us + kabp_us
  
  ##############################################################################
  
  #8. REDUNDANT EQUATIONS 
  
  #h_uk_s ~ h_uk_h - eq. 37
  #h_us_s ~ h_us_h - eq. 38
  
)

################################################################################

#SALVE THE MODEL AND CREATE SCENARIOS

#Define and run baseline scenario
openSime <- sfcr_baseline(
  equations = openSime_eqs,
  external = openSime_ext,
  initial = sfcr_set(openSime_init),
  periods = 100,
  method = "Broyden"#,
  #max_iter = 350
)

#Add shock (from period 15)
shock1 <- sfcr_shock(v = sfcr_set(eps0 ~ -2.05), s = 15, e = 100)
openSime1 <- sfcr_scenario(openSime, shock1, 100, method = "Broyden")

################################################################################

#PLOT RESULTS

#Change layout
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))

#Consistency check
plot(openSime$h_uk_s[1:100]-openSime$h_uk_h[1:100]+openSime$xr_us[1:100]*(openSime$h_us_s[1:100]-openSime$h_us_h[1:100]),type="l",col=1,lwd=3,ylim=range(-10,10),cex.main=1,font.main=1,main="a) Consistency check: \n H_s_uk - H_h_uk + xr_us*(H_s_us - H_h_us)",ylab='Value',xlab='Periods') 
lines(openSime1$h_uk_s[1:100]-openSime1$h_uk_h[1:100]+openSime1$xr_us[1:100]*(openSime1$h_us_s[1:100]-openSime1$h_us_h[1:100]),col=2,lwd=3,lty=3)
abline(h=0,col="gray20")
legend("topright",c("Baseline","Shock"),  bty = 'n', cex=0.8, lty=c(1,3), lwd=c(3,3), col = c(1,2), box.lty=0)

#Plot GDPs
plot(openSime1$y_uk[10:60],type="l",col=1,lty=1,lwd=3,cex.main=1,font.main=1,main="b) UK GDP and US GDP after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(min(openSime1$y_us[10:60]*openSime1$xr_us[10:60]),max(openSime1$y_uk[10:60])) )
lines(openSime1$y_us[10:60]*openSime1$xr_us[10:60],type="l",col="lightblue4",lty=1,lwd=3)
lines(openSime1$y_us[10:60],type="l",col="lightblue4",lty=3,lwd=3)
abline(h=openSime1$y_uk[10],col="gray20")
legend("right",c("UK GDP in £","US GDP in £","US GDP in $"),  bty = 'n', cex=0.8, lty=c(1,1,3), lwd=c(3,3,3), col = c(1,"lightblue4","lightblue4"), box.lty=0)

#Plot financial account balances
plot(openSime1$kabp_uk[10:60],type="l",col=5,lty=1,lwd=3,cex.main=1,font.main=1,main="c) UK KAB and US KAB after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(min(openSime1$kabp_uk[10:60]),max(openSime1$kabp_us[10:60])) )
lines(openSime1$kabp_us[10:60]*openSime1$xr_us[10:60],type="l",col="blue4",lty=1,lwd=3)
abline(h=0,col="gray20")
legend("topright",c("UK KABP in £","US KABP in £"),  bty = 'n', cex=0.8, lty=c(1,1), lwd=c(3,3), col = c(5,"blue4"), box.lty=0)

#Plot UK internal and external balances
plot(openSime1$cab_uk[10:60],type="l",col=2,lty=1,lwd=3,cex.main=1,font.main=1,main="d) UK internal and external balances after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(-0.3,0.7) )
lines(openSime1$nafa_uk[10:60],type="l",col=3,lty=1,lwd=3)
lines(openSime1$tb_uk[10:60],type="l",col=4,lty=3,lwd=3)
lines(openSime1$psbr_uk[10:60],type="l",col="orange",lty=1,lwd=3)
abline(h=0,col="gray20")
legend("topright",c("CAB","NAFA","Trade balance","Govern. deficit"),  bty = 'n', cex=0.8, lty=c(1,1,3,1), lwd=c(3,3,3,3), col = c(2,3,4,"orange"), box.lty=0)

#Plot exchange rate
plot(openSime1$xr_uk[10:60],type="l",col="purple",lty=1,lwd=3,cex.main=1,font.main=1,main="e) Nominal exchange rate after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(1,1.05) )
abline(h=1,col="gray20")
legend("right",c("Quantity of $ per 1 £"),  bty = 'n', cex=0.8, lty=c(1), lwd=c(3), col = c("purple"), box.lty=0)
