################################################################################
#
# R code for OPENFLEX model
#
# Author: Marco Veronese Passarella
#
# Last change: 9 March 2023 
#
# Note: this R code replicates the OPENFLEX model presented and used in W. Godley
# and M. Lavoie, Monetary Economics, Palgrave, 2007, chapter 12. Its main feature
# is that no model-specific R package is used. The only difference compared to
# the original code (https://gennaro.zezza.it/software/eviews/glch12.php#openflex)
# is that a lag is added to the denominators of equations 12.45 and 12.46.
#
################################################################################

#CLEAR ALL

# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

################################################################################

#CREATE AND DEFINE VARIABLES AND PARAMETERS

#Define time span
nPeriods = 50

#Define model variables
b_ukuk_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK acquired by the UK: demand
b_ukuk_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK acquired by the UK: supply
b_cb_ukus_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US, demanded by the UK Central bank
b_cb_ukus_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US, supplied to the UK Central bank
b_cb_ukuk_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK, demanded by the UK Central bank
b_cb_ukuk_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK, supplied to the UK Central bank
b_uk_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK - total supply
b_ukus_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US acquired by the UK: demand
b_ukus_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US acquired by the UK: supply
b_usuk_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK acquired by the US: demand
b_usuk_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the UK acquired by the US: supply
b_us_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US - total supply
b_usus_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US acquired by the US: demand
b_usus_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US acquired by the US: supply
b_cb_usus_d=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US demanded by the US Central Bank
b_cb_usus_s=matrix(data=0,nrow=1,ncol=nPeriods) # Bills issued by the US supplied to the US Central Bank
c_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real consumption in the UK
c_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real consumption in the US
cab_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Current account balance in the UK
cab_us=matrix(data=0,nrow=1,ncol=nPeriods) # Current account balance in the US
cons_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Consumption in the UK
cons_us=matrix(data=0,nrow=1,ncol=nPeriods) # Consumption in the US
ds_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Domestic sales in the UK
ds_us=matrix(data=0,nrow=1,ncol=nPeriods) # Domestic sales in the US
ds_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real domestic sales in the UK
ds_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real domestic sales in the US
dxre_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Expected change in the exchange rate of the UK (measured as units of the UK currency against 1 unit of the US currency)
dxre_us=matrix(data=0,nrow=1,ncol=nPeriods) # Expected change in the exchange rate the US (measured as units of the US currency against 1 unit of the UK currency)
f_cb_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Profits of Central Bank in the UK
f_cb_us=matrix(data=0,nrow=1,ncol=nPeriods) # Profits of Central Bank in the US
g_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Government expenditure in the UK
g_us=matrix(data=0,nrow=1,ncol=nPeriods) # Government expenditure in the US
g_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real government expenditure in the UK
g_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real government expenditure in the US
h_uk_d=matrix(data=0,nrow=1,ncol=nPeriods) # Demand for cash of the UK
h_uk_s=matrix(data=0,nrow=1,ncol=nPeriods) # Supply of the UK cash
h_us_d=matrix(data=0,nrow=1,ncol=nPeriods) # Demand for cash of the US
h_us_s=matrix(data=0,nrow=1,ncol=nPeriods) # Supply of the US cash
im_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Imports of the UK from the US
im_us=matrix(data=0,nrow=1,ncol=nPeriods) # Imports of the US from the UK
im_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real imports of the UK from the US
im_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real imports of the US from the UK
kab_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Capital account balance in the UK
kab_us=matrix(data=0,nrow=1,ncol=nPeriods) # Current account balance in the US
kabp_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Capital account balance in the UK, excluding official transactions
kabp_us=matrix(data=0,nrow=1,ncol=nPeriods) # Current account balance in the US, excluding official transactions
n_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Employment in the UK
n_us=matrix(data=0,nrow=1,ncol=nPeriods) # Employment in the US
or_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Gold reserves in the UK
or_us=matrix(data=0,nrow=1,ncol=nPeriods) # Gold reserves in the US
pds_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of domestic sales in the UK
pds_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of domestic sales in the US
pg_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of gold in the UK
pg_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of gold in the US
pm_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of imports in the US
pm_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of imports in the UK
pr_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Productivity in the UK
pr_us=matrix(data=0,nrow=1,ncol=nPeriods) # Productivity in the US
ps_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of sales in the UK
ps_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of sales in the US
psbr_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Government deficit in the UK
psbr_us=matrix(data=0,nrow=1,ncol=nPeriods) # Government deficit in the US
py_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of output in the US
py_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of imports in the UK
px_us=matrix(data=0,nrow=1,ncol=nPeriods) # Price of exports in the US
px_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Price of exports in the UK
r_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Interest rate on the UK bills
r_us=matrix(data=0,nrow=1,ncol=nPeriods) # Interest rate on the US bills
s_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Value of sales in the UK
s_us=matrix(data=0,nrow=1,ncol=nPeriods) # Value of sales in the US
s_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real sales in the UK
s_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real sales in the US
t_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Tax revenue in the UK
t_us=matrix(data=0,nrow=1,ncol=nPeriods) # Tax revenue in the US
v_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Net financial assets of the UK
v_us=matrix(data=0,nrow=1,ncol=nPeriods) # Net financial assets of the US
v_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real net financial assets of the UK
v_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real net financial assets of the US
w_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Nominal wage rate in the UK
w_us=matrix(data=0,nrow=1,ncol=nPeriods) # Nominal wage rate in the US
x_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Exports from the UK to the US
x_us=matrix(data=0,nrow=1,ncol=nPeriods) # Exports from the US to the UK
x_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real exports from the UK to the US
x_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real exports from the US to the UK
xr_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Exchange rate: units of S currency against 1 unit of N currency
xr_us=matrix(data=0,nrow=1,ncol=nPeriods) # Exchange rate: units of N currency against 1 unit of S currency
xre_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Expected exchange rate: units of S currency against 1 unit of N currency
xre_us=matrix(data=0,nrow=1,ncol=nPeriods) # Expected exchange rate: units of N currency against 1 unit of S currency
yd_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Disposable income in the UK
yd_us=matrix(data=0,nrow=1,ncol=nPeriods) # Disposable income in the US
yd_hs_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Haig-Simons disposable income in the UK
yd_hs_us=matrix(data=0,nrow=1,ncol=nPeriods) # Haig-Simons disposable income in the US
ydhs_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real Haig-Simons disposable income in the UK
ydhs_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real Haig-Simons disposable income in the US
ydhse_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Expected real Haig-Simons disposable income in the UK
ydhse_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Expected real Haig-Simons disposable income in the US
y_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Income in the UK
y_us=matrix(data=0,nrow=1,ncol=nPeriods) # Income in the US
y_k_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Real income in the UK
y_k_us=matrix(data=0,nrow=1,ncol=nPeriods) # Real income in the US
nafa_uk=matrix(data=0,nrow=1,ncol=nPeriods) # Net accumulation of financial assets in the UK
nafa_us=matrix(data=0,nrow=1,ncol=nPeriods) #Net accumulation of financial assets in the US

#Parameters
alpha1_uk = 0.75
alpha1_us = 0.75
alpha2_uk = 0.13333
alpha2_us = 0.13333
eps0 = -2.1
eps1 = 0.7
eps2 = 1
lambda10 = 0.7
lambda11 = 5
lambda12 = 5
lambda20 = 0.25
lambda21 = 5
lambda22 = 5
lambda40 = 0.7
lambda41 = 5
lambda42 = 5
lambda50 = 0.25
lambda51 = 5
lambda52 = 5
mu0 = -2.1
mu1 = 0.7
mu2 = 1
nu0m = - 0.00001
nu0x = - 0.00001
nu1m = 0.7
nu1x = 0.5
phi_uk = 0.2381
phi_us = 0.2381
theta_uk = 0.2
theta_us = 0.2

#Exogenous variables
b_cb_ukus_s[,] = 0.02031
dxre_us[,] = 0
g_k_uk[,] = 16
g_k_us[,] = 16
or_uk[,] = 7
pg_us[,] = 1
pr_uk[,] = 1.3333
pr_us[,] = 1.3333
r_uk[,] = 0.03
r_us[,] = 0.03
w_uk[,] = 1
w_us[,] = 1

# Starting values for stocks
b_cb_ukuk_d[,] = 0.27984
b_cb_ukuk_s[,] = 0.27984
b_cb_ukus_d[,] = 0.0203
b_cb_usus_d[,] = 0.29843
b_cb_usus_s[,] = 0.29843
b_uk_s[,] = 138.94
b_ukuk_d[,] = 102.18
b_ukuk_s[,] = 102.18
b_ukus_d[,] = 36.493
b_ukus_s[,] = 36.504
b_us_s[,] = 139.02
b_usuk_d[,] = 36.497
b_usuk_s[,] = 36.487
b_usus_d[,] = 102.19
b_usus_s[,] = 102.19
h_uk_d[,] = 7.2987
h_uk_s[,] = 7.2987
h_us_d[,] = 7.2995
h_us_s[,] = 7.2995
or_us[,] = 7
v_k_uk[,] = 152.62
v_k_us[,] = 152.63
v_uk[,] = 145.97
v_us[,] = 145.99001

# Other endogenous
c_k_uk[,] = 81.393
c_k_us[,] = 81.401
cab_uk[,] = 0
cab_us[,] = 0
cons_uk[,] = 77.851
cons_us[,] = 77.86
ds_k_uk[,] = 97.393
ds_k_us[,] = 97.401
ds_uk[,] = 93.154
ds_us[,] = 93.164
dxre_uk[,] = 0
f_cb_uk[,] = 0.00869
f_cb_us[,] = 0.00895
g_uk[,] = 15.304
g_us[,] = 15.304
im_k_uk[,] = 11.928
im_k_us[,] = 11.926
im_uk[,] = 11.407
im_us[,] = 11.409
kabp_uk[,] = 0.00002
kabp_us[,] = - 0.00002
n_uk[,] = 73.046
n_us[,] = 73.054
pds_uk[,] = 0.95648
pds_us[,] = 0.95649
pg_uk[,] = 0.99971
pm_uk[,] = 0.95628
pm_us[,] = 0.95661
ps_uk[,] = 0.95646
ps_us[,] = 0.9565
px_uk[,] = 0.95634
px_us[,] = 0.95656
py_uk[,] = 0.95648
py_us[,] = 0.95649
s_k_uk[,] = 109.32
s_k_us[,] = 109.33
s_uk[,] = 104.56
s_us[,] = 104.57
t_uk[,] = 19.463
t_us[,] = 19.465
x_k_uk[,] = 11.926
x_k_us[,] = 11.928
x_uk[,] = 11.406
x_us[,] = 11.41
xr_uk[,] = 1.0003
xr_us[,] = 0.99971
xre_uk[,] = 1.0003
xre_us[,] = 0.99971
y_k_uk[,] = 97.392
y_k_us[,] = 97.403
y_uk[,] = 93.154
y_us[,] = 93.164
yd_uk[,] = 77.851
yd_us[,] = 77.86
ydhs_k_uk[,] = 81.394
ydhs_k_us[,] = 81.402
ydhse_k_uk[,] = 81.394
ydhse_k_us[,] = 81.402

################################################################################

#CREATE THE MODEL

#Define time
for (i in 2:nPeriods){
  
  #Define alternative scenarios / shocks
  if(i>5){eps0=-2}
  
  #Define iterations for converging to simultaneous solution
  for (iterations in 1:300){
    
    ############################################################################
    
    #ACCOUNTING IDENTITIES
    
    # Disposable income in UK - eq. 12.1
    yd_uk[,i] = (y_uk[,i] + r_uk[,i-1]*b_ukuk_d[,i-1] + xr_us[,i]*r_us[,i-1]*b_ukus_s[,i-1])*(1 - theta_uk) + (xr_us[,i]-xr_us[,i-1])*b_ukus_s[,i-1]

    # Haig-Simons disposable income in UK - eq. 12.2
    yd_hs_uk[,i] = yd_uk[,i] + (xr_us[,i]-xr_us[,i-1])*b_ukus_s[,i-1]
    
    # Wealth accumulation in UK - eq. 12.3
    v_uk[,i] = v_uk[,i-1] + yd_uk[,i] - cons_uk[,i]

    # Disposable income in US - eq. 12.4
    yd_us[,i] = (y_us[,i] + r_us[,i-1]*b_usus_d[,i-1] + xr_uk[,i]*r_uk[,i-1]*b_usuk_s[,i-1])*(1 - theta_us) + (xr_uk[,i]-xr_uk[,i-1])*b_usuk_s[,i-1]
    
    # Haig-Simons disposable income in US - eq. 12.5
    yd_hs_us[,i] = yd_us[,i] + (xr_uk[,i]-xr_uk[,i-1])*b_usuk_s[,i-1]

    # Wealth accumulation in US - eq. 12.6
    v_us[,i] = v_us[,i-1] + yd_us[,i] - cons_us[,i]
    
    # Taxes in UK - eq. 12.7
    t_uk[,i] = theta_uk*(y_uk[,i] + r_uk[,i-1]*b_ukuk_d[,i-1] + xr_us[,i]*r_us[,i-1]*b_ukus_s[,i-1])

    # Taxes in US - eq. 12.8
    t_us[,i] = theta_us*(y_us[,i] + r_us[,i-1]*b_usus_d[,i-1] + xr_uk[,i]*r_uk[,i-1]*b_usuk_s[,i-1])
    
    # Equations 12.9 & 12.10 are dropped in favour of equations 12.53 & 12.54

    # Profits of Central Bank in UK - eq. 12.11 - typo in the book for r_us
    f_cb_uk[,i] = r_uk[,i-1]*b_cb_ukuk_d[,i-1] + r_us[,i-1]*b_cb_ukus_s[,i-1]*xr_us[,i]
    
    # Profits of Central Bank in US - eq. 12.12
    f_cb_us[,i] = r_us[,i-1]*b_cb_usus_d[,i-1]
    
    # Government budget constraint - UK - eq. 12.13
    b_uk_s[,i] = b_uk_s[,i-1] + g_uk[,i] + r_uk[,i-1]*b_uk_s[,i-1] - t_uk[,i] - f_cb_uk[,i]

    # Government budget constraint - US - eq. 12.14
    b_us_s[,i] = b_us_s[,i-1] + g_us[,i] + r_us[,i-1]*b_us_s[,i-1] - t_us[,i] - f_cb_us[,i]
    
    # Current account balance - UK - eq. 12.15
    cab_uk[,i] = x_uk[,i] - im_uk[,i] + xr_us[,i]*r_us[,i-1]*b_ukus_s[,i-1] - r_uk[,i-1]*b_usuk_s[,i-1] + r_us[,i-1]*b_cb_ukus_s[,i-1]*xr_us[,i]

    # Capital account balance in UK - eq. 12.16
    kab_uk[,i] = kabp_uk[,i] - (xr_us[,i]*(b_cb_ukus_s[,i]-b_cb_ukus_s[,i-1]) + pg_uk[,i]*(or_uk[,i]-or_uk[,i-1]))
    
    # Current account balance in US - eq. 12.17
    cab_us[,i] = x_us[,i] - im_us[,i] + xr_uk[,i]*r_uk[,i-1]*b_usuk_s[,i-1] - r_us[,i-1]*b_ukus_s[,i-1] - r_us[,i-1]*b_cb_ukus_s[,i-1]

    # Capital account balance in US - eq. 12.18
    kab_us[,i] = kabp_us[,i] + (b_cb_ukus_s[,i]-b_cb_ukus_s[,i-1]) - pg_us[,i]*(or_us[,i]-or_us[,i-1])
    
    # Capital account balance in UK, net of official transactions - eq. 12.19
    kabp_uk[,i] = - (b_ukus_s[,i]-b_ukus_s[,i-1])*xr_us[,i] + (b_usuk_s[,i]-b_usuk_s[,i-1])

    # Capital account balance in US, net of official transactions - eq. 12.20
    kabp_us[,i] = - (b_usuk_s[,i]-b_usuk_s[,i-1])*xr_uk[,i] + (b_ukus_s[,i]-b_ukus_s[,i-1])
    
    ############################################################################
    
    # TRADE

    # Import prices in UK - eq. 12.21
    pm_uk[,i] = exp(nu0m + nu1m*log(py_us[,i]) + (1 - nu1m)*log(py_uk[,i]) - nu1m*log(xr_uk[,i]))
    
    # Export prices in UK - eq. 12.22
    px_uk[,i] = exp(nu0x + nu1x*log(py_us[,i]) + (1 - nu1x)*log(py_uk[,i]) - nu1x*log(xr_uk[,i]))

    # Export prices in US - eq. 12.23
    px_us[,i] = pm_uk[,i]*xr_uk[,i]
    
    # Import prices in US - eq. 12.24
    pm_us[,i] = px_uk[,i]*xr_uk[,i]

    # Real exports from UK - eq. 12.25 - depends on current relative price
    x_k_uk[,i] = exp(eps0 - eps1*log(pm_us[,i]/py_us[,i]) + eps2*log(y_k_us[,i]))
    
    # Real imports of UK - eq. 12.26
    im_k_uk[,i] = exp(mu0 - mu1*log(pm_uk[,i-1]/py_uk[,i-1]) + mu2*log(y_k_uk[,i]))

    # Real exports from US - eq. 12.27
    x_k_us[,i] = im_k_uk[,i]
    
    # Real imports of US - eq. 12.28
    im_k_us[,i] = x_k_uk[,i]

    # Exports of UK - eq. 12.29
    x_uk[,i] = x_k_uk[,i]*px_uk[,i]
    
    # Exports of US - eq. 12.30
    x_us[,i] = x_k_us[,i]*px_us[,i]

    # Imports of UK - eq. 12.31
    im_uk[,i] = im_k_uk[,i]*pm_uk[,i]
    
    # Imports of US - eq. 12.32
    im_us[,i] = im_k_us[,i]*pm_us[,i]
    
    ############################################################################
    
    # INCOME AND EXPENDITURE

    # Real wealth in UK - eq. 12.33
    v_k_uk[,i] = v_uk[,i]/pds_uk[,i]
    
    # Real wealth in US - eq. 12.34
    v_k_us[,i] = v_us[,i]/pds_us[,i]

    # Real Haig-Simons disposable income in UK - eq. 12.35
    ydhs_k_uk[,i] = yd_uk[,i]/pds_uk[,i] - v_k_uk[,i-1]*(pds_uk[,i]-pds_uk[,i-1])/pds_uk[,i]
    
    # Real Haig-Simons disposable income in US - eq. 12.36
    ydhs_k_us[,i] = yd_us[,i]/pds_us[,i] - v_k_us[,i-1]*(pds_us[,i]-pds_us[,i-1])/pds_us[,i] 

    # Real consumption in UK - eq. 12.37
    c_k_uk[,i] = alpha1_uk*ydhse_k_uk[,i] + alpha2_uk*v_k_uk[,i-1]
    
    # Real consumption in US - eq. 12.38
    c_k_us[,i] = alpha1_us*ydhse_k_us[,i] + alpha2_us*v_k_us[,i-1]

    # Expected real Haig-Simons disposable income in UK - eq. 12.39
    ydhse_k_uk[,i] = (ydhs_k_uk[,i] + ydhs_k_uk[,i-1])/2
    
    # Expected real Haig-Simons disposable income in US - eq. 12.40
    ydhse_k_us[,i] = (ydhs_k_us[,i] + ydhs_k_us[,i-1])/2

    # Real sales in UK - eq. 12.41
    s_k_uk[,i] = c_k_uk[,i] + g_k_uk[,i] + x_k_uk[,i]
    
    # Real sales in US - eq. 12.42
    s_k_us[,i] = c_k_us[,i] + g_k_us[,i] + x_k_us[,i]

    # Value of sales in UK - eq. 12.43
    s_uk[,i] = s_k_uk[,i]*ps_uk[,i]
    
    # Value of sales in US - eq. 12.44
    s_us[,i] = s_k_us[,i]*ps_us[,i]

    # Price of sales in UK - eq. 12.45 
    ps_uk[,i] = (1 + phi_uk)*(w_uk[,i]*n_uk[,i] + im_uk[,i])/s_k_uk[,i-1] #-------lag added
    
    # Price of sales in US - eq. 12.46 
    ps_us[,i] = (1 + phi_us)*(w_us[,i]*n_us[,i] + im_us[,i])/s_k_us[,i-1] #-------lag added

    # Price of domestic sales in UK - eq. 12.47
    pds_uk[,i] = (s_uk[,i] - x_uk[,i])/(s_k_uk[,i] - x_k_uk[,i])
    
    # Price of domestic sales in US - eq. 12.48
    pds_us[,i] = (s_us[,i] - x_us[,i])/(s_k_us[,i] - x_k_us[,i])

    # Domestic sales in UK - eq. 12.49
    ds_uk[,i] = s_uk[,i] - x_uk[,i]
    
    # Domestic sales in US - eq. 12.50
    ds_us[,i] = s_us[,i] - x_us[,i]

    # Real domestic sales in UK - eq. 12.51
    ds_k_uk[,i] = c_k_uk[,i] + g_k_uk[,i]
    
    # Real domestic sales in US - eq. 12.52
    ds_k_us[,i] = c_k_us[,i] + g_k_us[,i]

    # Value of output in UK - eq. 12.53
    y_uk[,i] = s_uk[,i] - im_uk[,i]
    
    # Value of output in US - eq. 12.54
    y_us[,i] = s_us[,i] - im_us[,i]

    # Value of real output in UK - eq. 12.55
    y_k_uk[,i] = s_k_uk[,i] - im_k_uk[,i]
    
    # Value of real output in US - eq. 12.56
    y_k_us[,i] = s_k_us[,i] - im_k_us[,i]

    # Price of output in UK - eq. 12.57
    py_uk[,i] = y_uk[,i]/y_k_uk[,i]
    
    # Price of output in US - eq. 12.58
    py_us[,i] = y_us[,i]/y_k_us[,i]

    # Consumption in UK - eq. 12.59
    cons_uk[,i] = c_k_uk[,i]*pds_uk[,i]
    
    # Consumption in US - eq. 12.60
    cons_us[,i] = c_k_us[,i]*pds_us[,i]

    # Government expenditure in UK - eq. 12.61
    g_uk[,i] = g_k_uk[,i]*pds_uk[,i]
    
    # Government expenditure in US - eq. 12.62
    g_us[,i] = g_k_us[,i]*pds_us[,i]

    # Note: tax definitions in the book as eqns 12.63 & 12.64 are already as eqns 12.7 & 12.8
    
    # Employment in UK - eq. 12.65
    n_uk[,i] = y_k_uk[,i]/pr_uk[,i]

    # Employment in US - eq. 12.66
    n_us[,i] = y_k_us[,i]/pr_us[,i]
    
    ############################################################################
    
    # ASSET DEMANDS

    # Demand for UK bills in UK - eq. 12.67
    b_ukuk_d[,i] = v_uk[,i]*(lambda10 + lambda11*r_uk[,i] - lambda12*(r_us[,i] + dxre_us[,i]))
    
    # Demand for US bills in UK - eq. 12.68
    b_ukus_d[,i] = v_uk[,i]*(lambda20 - lambda21*r_uk[,i] + lambda22*(r_us[,i] + dxre_us[,i]))

    # Demand for money in UK - eq. 12.69
    h_uk_d[,i] = v_uk[,i] - b_ukuk_d[,i] - b_ukus_d[,i]
    
    # Demand for US	bills in US - eq. 12.70
    b_usus_d[,i] = v_us[,i]*(lambda40 + lambda41*r_us[,i] - lambda42*(r_uk[,i] + dxre_uk[,i]))

    # Demand for UK bills in US - eq. 12.71
    b_usuk_d[,i] = v_us[,i]*(lambda50 - lambda51*r_us[,i] + lambda52*(r_uk[,i] + dxre_uk[,i]))
    
    # Demand for money in US - eq. 12.72
    h_us_d[,i] = v_us[,i] - b_usus_d[,i] - b_usuk_d[,i]

    ############################################################################
    
    # ASSET SUPPLIES

    # Suply of cash in US - eq. 12.77
    h_us_s[,i] = h_us_d[,i]
    
    # Supply of US bills to CountryN - eq. 12.78
    b_usus_s[,i] = b_usus_d[,i]

    # Supply of US bills to US Central bank - eq. 12.79
    b_cb_usus_s[,i] = b_cb_usus_d[,i]
    
    # Suply of cash in UK - eq. 12.80
    h_uk_s[,i] = h_uk_d[,i]

    # Bills issued by US acquired by US - eq. 12.81
    b_ukuk_s[,i] = b_ukuk_d[,i]
    
    # Supply of UK bills to UK Central bank - eq. 12.82
    b_cb_ukuk_s[,i] = b_cb_ukuk_d[,i]
    
    # Balance sheet of US Central bank - eq. 12.83 - expressed as changes
    b_cb_usus_d[,i] = b_cb_usus_d[,i-1] + (h_us_s[,i]-h_us_s[,i-1]) - (or_us[,i]-or_us[,i-1])*pg_us[,i]

    # Balance sheet of UK Central bank - eq. 12.84
    b_cb_ukuk_d[,i] = b_cb_ukuk_d[,i-1] + (h_uk_s[,i]-h_uk_s[,i-1]) - (b_cb_ukus_s[,i]-b_cb_ukus_s[,i-1])*xr_us[,i] - (or_uk[,i]-or_uk[,i-1])*pg_uk[,i]
    
    # Price of gold is equal in the two countries - eq. 12.85
    pg_uk[,i] = pg_us[,i]/xr_uk[,i]

    # US exchange rate - eq. 12.86
    xr_us[,i] = 1/xr_uk[,i]
    
    # Equilibrium condition for bills issued by UK acquired by US - eq. 12.87
    b_usuk_s[,i] = b_usuk_d[,i]*xr_us[,i]

    # Equilibrium condition for bills issued by US acquired by UK Central bank - eq. 12.88
    b_cb_ukus_d[,i] = b_cb_ukus_s[,i]*xr_us[,i]
    
    # UK Exchange rate - eq. 12.89FL
    xr_uk[,i] = b_ukus_s[,i]/b_ukus_d[,i]

    # Supply of UK bills to CountryS - eq. 12.90FL
    b_ukus_s[,i] = b_us_s[,i] - b_usus_s[,i] - b_cb_usus_d[,i] - b_cb_ukus_s[,i]
    
    # Government deficit in the UK
    psbr_uk[,i] = g_uk[,i] + r_uk[,i-1]*b_uk_s[,i-1] - t_uk[,i] - f_cb_uk[,i]

    # Government deficit in the US
    psbr_us[,i] = g_us[,i] + r_us[,i-1]*b_us_s[,i-1] - t_us[,i] - f_cb_us[,i]
    
    # Net accumulation of financial assets in the UK
    nafa_uk[,i] = psbr_uk[,i] + cab_uk[,i]

    # Net accumulation of financial assets in the US
    nafa_us[,i] = psbr_us[,i] + cab_us[,i]
    
    ############################################################################
    
    # Redundant equation -  eq. 12.82A
    #b_cb_ukuk_s[,i] = b_uk_s[,i] - b_ukuk_s[,i] - b_usuk_s[,i]
    
    
  }
  
}

################################################################################

# PLOT RESULTS

#Change layout
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))

#Hidden equation
plot(b_cb_ukuk_s[,1:nPeriods]-(b_uk_s[,1:nPeriods] - b_ukuk_s[,1:nPeriods] - b_usuk_s[,1:nPeriods]),type="l",col="red4",lty=1,lwd=3,cex.main=1,font.main=1,main="a) Consistency check",ylab='Value',xlab='Periods',ylim=range(-1,1))
legend("bottomleft", 
       legend = expression(Equation (12.82):~italic(B["cb£s"]^"£" - (B[s]^"£" - B["£s"]^"£" - B["$s"]^"£"))),
       bty = 'n', cex = 1, lty = c(1), lwd = c(3), col = c("red4"), box.lty = 0)

#Plot GDPs
plot(y_k_uk[,1:nPeriods],type="l",col=1,lty=1,lwd=3,cex.main=1,font.main=1,main="b) UK and US real GDP after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(min(y_k_us[,1:nPeriods]),max(y_k_uk[,1:nPeriods])) )
lines(y_k_us[,1:nPeriods],type="l",col="lightblue4",lty=1,lwd=3)
abline(h=y_k_uk[4],col="gray20")
legend("top",c("UK GDP","US GDP"),  bty = 'n', cex=0.8, lty=c(1,1), lwd=c(3,3), col = c(1,"lightblue4"), box.lty=0)

#Plot financial account balances
plot(kabp_uk[,1:nPeriods],type="l",col=5,lty=1,lwd=3,cex.main=1,font.main=1,main="c) UK KAB and US KAB after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(min(kabp_uk[,1:nPeriods]),max(kabp_us[,1:nPeriods])) )
lines(kabp_us[,1:nPeriods]*xr_us[,1:nPeriods],type="l",col="blue4",lty=1,lwd=3)
abline(h=0,col="gray20")
legend("topright",c("UK KABP in £","US KABP in £"),  bty = 'n', cex=0.8, lty=c(1,1), lwd=c(3,3), col = c(5,"blue4"), box.lty=0)

#Plot UK internal and external balances
plot(cab_uk[,1:nPeriods],type="l",col=2,lty=1,lwd=3,cex.main=1,font.main=1,main="d) UK internal and external balances after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(-0.4,1) )
lines(nafa_uk[,1:nPeriods],type="l",col=3,lty=1,lwd=3)
lines(x_uk[,1:nPeriods]-im_uk[,1:nPeriods],type="l",col=4,lty=1,lwd=3)
lines(psbr_uk[,1:nPeriods],type="l",col="orange",lty=1,lwd=3)
abline(h=0,col="gray20")
legend("topright",c("CAB","NAFA","Trade balance","Govern. deficit"),  bty = 'n', cex=0.8, lty=c(1,1,1,1), lwd=c(3,3,3,3), col = c(2,3,4,"orange"), box.lty=0)

#Plot exchange rate
plot(xr_uk[,1:nPeriods],type="l",col="purple",lty=1,lwd=3,cex.main=1,font.main=1,main="e) Nominal exchange rate after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(1,1.25) )
abline(h=1,col="gray20")
legend("topleft",c("Quantity of $ per 1 £"),  bty = 'n', cex=0.8, lty=c(1), lwd=c(3), col = c("purple"), box.lty=0)

#Plot prices
plot(px_uk[,1:nPeriods],type="l",col=2,lty=1,lwd=3,cex.main=1,font.main=1,main="f) Various price indices in UK after \n increase in US propensity to import",ylab='Value',xlab='Periods',ylim=range(0.8,1) )
lines(pm_uk[,1:nPeriods],col=3,lty=1,lwd=3)
lines(ps_uk[,1:nPeriods],col=4,lty=1,lwd=3)
lines(pds_uk[,1:nPeriods],col=5,lty=1,lwd=3)
lines(py_uk[,1:nPeriods],col=6,lty=1,lwd=3)
abline(h=px_uk[,4],col="gray20")
legend("bottomleft",c("Export","Import","Sales","Domestic sales","Output"),  bty = 'n', cex=0.8, lty=c(1,1,1,1,1), lwd=c(3,3,3,3,3), col = c(2,3,4,5,6), box.lty=0)
