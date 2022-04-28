#----------------------------------------------------------------
# Vehicle Sales Forecasting
#
# Created:  04/12/2022
#
# Last Modified:  04/28/2022  
#
# Authors:  Miles S. Marimbire, University of Cincinnati
#----------------------------------------------------------------

#----------------------------------------------------------------
# This code is open source feel free to add any modification you wish
#----------------------------------------------------------------
# Set up R 
#----------------------------------------------------------------
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())
# To clear just the console window, type "Ctrl+L" or use Edit pull down menu

# Specify a display option
#options("scipen"=999, digits=2)

# === Set the working directory.  Note that R uses "/" not "\"
# === So the command is setwd("your directory path") or use the Session pull down menu
setwd("~/R/")
# === NOTE:  If using a MAC computer, you might need to replace the above command with something like
# === setwd("Desktop/R/")
#----------------------------------------------------------------

#----------------------------------------------------------------
# List of packages 
packages <- c("AER","car","ggplot2","gmodels", "haven", "jtools", "pastecs", "psych", "skedastic",
              "stargazer", "summarytools","tidyverse", "corrplot", "olsrr", "RNHANES", "dplyr", 
              "gridExtra", "dynlm", "ggthemes","shiny","urca","vars","shinydashboard","readxl", "dlookr", "gtools", "scales", "caret", "rvest","xml2","readr")

# Install the packages
# Run this code to install packages you do not have installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))
search()

# Set the number of digits to display when using the jtools commands, like summ
options("jtools-digits"=4) 
options(scipen = 999)

# You can use these methods if you need a example of how to use a package
browseVignettes("AER")  # Short documents on how to use the packages
?mean # Opens the help page for the mean function
?"+" #  Opens the help page for addition
?"if" # Opens the help page for if, used for branching code
??plotting #  Searches for topics containing words like "plotting"
??"regression model" #  Searches for topics containing phrases like this
#--------------------------------------------------------

#--------------------------------------------------------
# Vehicle Data
#--------------------------------------------------------
# Load in the datasets

### LOAD DATA

# Total US vehicle sales, 1953:1 - 2022:1
vehicle <- read_csv("Vehicle Data.csv")
View(vehicle)

# Vehicle Miles Traveled, US  1976:1 - 2022:1
miles <- read_excel("miles trav.xlsx")
View(miles)

# Consumer Price Index for All Urban Consumers Used Cars and Trucks in U.S. City Average, US 1953:1 - 2022: 3
consumer <- read_csv("Consumer Price Index for All Urban Consumers Used Cars and Trucks in U.S. City Average.csv")
View(consumer)

#--------------------------------------------------------
# EDA
#--------------------------------------------------------

# Create variables from rows in data set
vehicle_sale = vehicle$`Total Vehicle Sales`
miles_trav = miles$`Vehicles Miles Travled`
con_price = consumer$`Consumer Price Index`

# Make sure all time series data is the same length
length(vehicle_sale) # 553
length(miles_trav)  # 553
length(con_price) # 831 

# Make con_price the same length as other variables
m = length(consumer$`Consumer Price Index`)
v = consumer$`Consumer Price Index`[277:m]
length(v)
var_consum = v[1:553]
length(var_consum)
var_consum

datei = consumer$DATE[279:m-2]  # check dates are correct
datei
c_datee = datei[1:553] 
length(c_datee)

# Check and verify the length of the dates
v_date = vehicle$DATE # "1/1/1976" - "1/1/2022"
m_date = miles$DATE # "1/1/1976" - "1/1/2022"
c_date = datei # "1/1/1976" - "1/1/2022"


# Verify 
length(c_date)
length(m_date)
length(v_date)

length(var_consum)
length(vehicle_sale)
length(miles_trav)


# Take logs of variables
lvehicle = log(vehicle_sale)
lmiles = log(miles_trav)
lconsumer = log(var_consum)

# Covert into a Time-Series
vehtot = ts(lvehicle,frequency=12,start=c(1976,1),end=c(2022,1))
miletot = ts(lmiles,frequency=12,start=c(1976,1),end=c(2022,1)) 
contot = ts(lconsumer,frequency=12,start=c(1976,1),end=c(2022,1))

# drop last 4 observation to save for out-of-sample prediction
ntot = length(vehtot)
dc_var = contot[1:(ntot-4)]
dv_var = vehtot[1:(ntot-4)]
dm_var = miletot[1:(ntot-4)]

nt = length(v_date)
con_date = c_date[1:(nt-4)]
mile_date = m_date[1:(nt-4)]
sale_date = v_date[1:(nt-4)]


# check data
cbind(dc_var, dv_var, dm_var)


source("intord.R")

# Determine order of integration 
intord(dc_var) # I(1)
intord(dv_var) # I(1) 
intord(dm_var) # I(1) Seasonality is present do not rely on ADF test

# Difference variables

# Consumer
dconsumer = diff(dc_var)

# Vehicle
dvehicle = diff(dv_var)

# Miles
dmiles = diff(dm_var)


# Descriptive Statistics

# Consumer 
summary(dconsumer)
sd(dconsumer)
length(dconsumer)

# Miles
summary(dmiles)
sd(dmiles)
length(dmiles)

# Vehicles 
summary(dvehicle)
sd(dvehicle)
length(dvehicle)

# Cointergration 
# Engle-Granger cointegration method
# MUST HAVE VARIABLES IN LEVELS NOT DIFFERENCED DO NOT USE DIFF VARIABLES

reg_1 = lm(dv_var[1:526] ~ dc_var[1:526] + dm_var[1:526]) # Before Pandemic   
summ(reg_1)
AIC(reg_1); BIC(reg_1)
uhat = reg_1$residuals
intord(uhat)

reg_2 = lm(dv_var[1:383] ~ dc_var[1:383] + dm_var[1:383]) # Before 2008 Crash  
summ(reg_2)
AIC(reg_2); BIC(reg_2)
uhat_2 = reg_2$residuals
intord(uhat_2)

reg_3 = lm(dv_var ~ dc_var + dm_var)    
summ(reg_3)
AIC(reg_3); BIC(reg_3)
uhat_3 = reg_3$residuals
intord(uhat_3)

reg_4 = lm(dm_var ~ dv_var + dc_var)    
summ(reg_4)
AIC(reg_4); BIC(reg_4)
uhat_4 = reg_4$residuals
intord(uhat_4)

reg_5 = lm(dc_var ~ dm_var + dv_var )    
summ(reg_5)
AIC(reg_5); BIC(reg_5)
uhat_5 = reg_5$residuals
intord(uhat_5)

reg_6 = lm(dc_var ~ dv_var )    
summ(reg_6)
AIC(reg_6); BIC(reg_6)
uhat_6 = reg_6$residuals
intord(uhat_6)

reg_7 = lm(dc_var ~ dm_var)    
summ(reg_7)
AIC(reg_7); BIC(reg_7)
uhat_7 = reg_7$residuals
intord(uhat_7)

ecm0 = uhat_3
ecm1 <- embed(ecm0,2)
ecmeg <- ecm1[,2]

### NONE OF THE COMBINATIONS APPEAR TO BE COINTEGRATED ###

# Johansen approach to cointegration:
# stack all the variables IN LEVELS (undifferenced) in a matrix (in the cbind statement)
yy = cbind(dc_var, dm_var, dv_var)    

# Max eigenvalue test:
vecm1 = ca.jo(yy, ecdet = "const", type="eigen", K=6, spec="longrun",
              season=12)
summary(vecm1)
#           test 10pct  5pct  1pct
# r = 0  | 21.70 19.77 22.00 26.81 
# Strong evidence (@10%) of cointegration
# Other level's suggest that there is no level of cointergration




# Trace (eigenvalue) test:
vecm2 = ca.jo(yy, ecdet = "const", type="trace", K=6, spec="longrun",
              season=12)
summary(vecm2)
#           test 10pct  5pct  1pct
# r = 0  | 40.73 32.00 34.91 41.07
# Strong evidence (@10% and 5%) of cointegration
# Other level's suggest that there is no level of cointergration


cointv <- vecm1@V
cointj <- cointv[,1]
yym <- as.matrix(yy)
ecmj <- yym%*%cointj[1:3] + cointj[4] 

ecj <- embed(ecmj,2)
ecmj1 <- ecj[,2]  # the error correction terms to use in an ECM uhat(t-1)


# Two estimates of the error correction term (from Engle-Granger, and from Johansen)
par(mfrow=c(1,2))
plot(ecmeg,type='l')
plot(ecmj1,type='l')
xx = cbind(ecmeg, ecmj1)
matplot(xx, type='l', lty=1, col=1:2)
abline(h=0.0)

cor(ecmeg, ecmj1)


# DYNAMIC regression models

# Auto regressive Model building (i.e. only lags of dependent variable in model)
# variables must be "time series objects in dynlm, so convert to ts:
dvehiclet = ts(dvehicle,frequency=12,start=c(2002,2)) 

# Dynamic Regression model with 18 Lags
# Only top 4 Lags are significant so will look at only top 4
# "second best" more conservative model (including more lags)
dy_reg_1 = dynlm(dvehiclet~L(dvehiclet,1:18))
summ(dy_reg_1)
AIC(dy_reg_1)
BIC(dy_reg_1)

# test for seasonality
r_ur = dynlm(dvehiclet ~ L(dvehiclet,1:18)+season(dvehiclet))
summ(r_ur)
anova(dy_reg_1,r_ur, test="F") # No evidence of seasonality. 

# best model
dy_reg_2 = dynlm(dvehiclet~L(dvehiclet,1:4))
summ(dy_reg_2)
AIC(dy_reg_2)
BIC(dy_reg_2)

# Both AIC and BIC support claim of "dy_reg_2" being the better model
# AIC and BIC show that the model does better when there are fewer Lags

# check for serial correlation
library(lmtest)
r2 = dynlm(dvehiclet ~ L(dvehiclet,1:18))
bgtest(r2, order=1)
bgtest(r2, order=2)
bgtest(r2, order=3)
bgtest(r2, order=4)
bgtest(r2, order=5)
bgtest(r2, order=6)
bgtest(r2, order=7)
bgtest(r2, order=8)
bgtest(r2, order=9)
bgtest(r2, order=10)
bgtest(r2, order=11)
bgtest(r2, order=12)
bgtest(r2, order=13)
bgtest(r2, order=14)
bgtest(r2, order=15)
bgtest(r2, order=16)
bgtest(r2, order=17)
bgtest(r2, order=18)
bgtest(r2, order=19)

source("serialcor_test.R")

serialcor_test(dy_reg_2)


# looks that p=18 is better choice (in removing serial correlation)


#stack variables in a matrix
dy = cbind(dvehicle, dconsumer, dmiles)

# lag selection criteria
VARselect(dy, lag.max=20, type="const", exogen=ecmj1)

# choosing lag length using BIC
p = 13
m1 <- VAR(dy, type = "const", p=p, exogen=ecmj1)
summary(m1)
AIC(m1)
BIC(m1)
m1eq1 = m1$varresult$dvehicle  # dvehicle equation from VAR 
AIC(m1eq1)
BIC(m1eq1)

plot(m1, names = "dvehicle")
plot(m1, names = "dconsumer")
plot(m1, names = "dmiles")



# Test for serial correlation

## Box-Ljung Q statistic p-values
# Get residuals from each equation - put appropriate variables in 
# each of the next 3 lines:
res_dc = m1$varresult$dconsumer$residuals
res_dv = m1$varresult$dvehicle$residuals
res_dm = m1$varresult$dmiles$residuals

# test residuals one at a time:
res = res_dc

blt <- rep(0,12)
for (i in 1:12) {
  b <- Box.test(res,lag = i , type = "Ljung-Box")
  blt[i] <- b$p.value
}
blt

# RMSE in-sample
rmse = sqrt(mean(res^2))
rmse

source("serialcor_test.R")

serialcor_test(m1eq1)

# USING VAR statement estimation:
p = 13
m1 = VAR(dy, type = "const", p=p, exogen=ecmj1) # unrestricted
dyr = dy = cbind(dvehicle, dconsumer) # daaa drop one variable for restricted model
m1r = VAR(dyr, type = "const", p=p) # unrestricted
# Joint F-test for Granger causality
anova(m1r$varresult$dvehicle, m1$varresult$dvehicle) #), test="F")  # Does aaa GC con?
anova(m1r$varresult$dconsumer, m1$varresult$dconsumer, test="F")  # Does aaa GC inc?

summ(m1$varresult$dcon)
summ(m1r$varresult$dcon)

# Variance Decompositions (contribution of each variable to predicting a variable)
p = 13
var3 = VAR(dy, p=p, type="const",season=12)
vard <- fevd(var3, n.ahead=12)
vard
vard$dmiles


# Forecasting with a VECM and undiff values
varf <- vec2var(vecm1)
fcast <- predict(varf, n.ahead = 4, ci = 0.95) 
plot(fcast)
fcast

# plotting forecasts for one variable (lhfa)
lconf <- fcast$fcst$dv_var[,1]
lconflow <- fcast$fcst$dv_var[,2]
lconfupp <- fcast$fcst$dv_var[,3]

ff <- cbind(lconf,lconflow,lconfupp)
par(mfrow=c(1,1))
matplot(ff,col=c(1,3,3),lty=1,lwd=2,type='l')

# Forecasting with a VECM and undiff values
varf <- vec2var(vecm1)
fcast <- predict(varf, n.ahead = 4, ci = 0.95) 
plot(fcast)
fcast



# plotting forecasts for one variable (lhfa)
lconf <- fcast$fcst$dv_var[,1]
lconflow <- fcast$fcst$dv_var[,2]
lconfupp <- fcast$fcst$dv_var[,3]

ff <- cbind(lconf,lconflow,lconfupp)
par(mfrow=c(1,1))
matplot(ff,col=c(1,3,3),lty=1,lwd=2,type='l')


# RMPSE out-of-sample

# actual values for next 4 periods (beyond estimation sample)
vehtotf4 = vehtot[(ntot-3): ntot]
miletotf4 = miletot[(ntot-3): ntot]
contotf4 = contot[(ntot-3): ntot]

# Estimates 
ar4 = arima(dv_var, order=c(4,1,0))
fcast = predict(ar4,n.ahead=4,se.fit = TRUE)


yf = fcast$pred # forecast values
yfse = fcast$se # standard errors of forecasts
u95 = yf + 2*yfse
l95 = yf - 2*yfse
ff1 = cbind(yf,u95,l95)

y = dv_var
par(mfrow=c(2,1))
matplot(ff1,type='l',col=c(1,3,3),lty=1,main="forecasts from ARIMA(4,1,0)")
legend("topleft",legend=c("Forecast","upper .95 PI","lower .95 PI"),col=c(1:3),lty=c(1:3),lwd=1,bty="n",cex=1.1)
ff = cbind(c(y,u95),c(y,l95),c(y,yf))
matplot(ff,type='l',col=c(3,3,1),lwd=1,lty=1,main="forecasts from ARIMA(4,1,0)")
legend("topleft",legend=c("Forecast","upper .95 PI","lower .95 PI"),col=c(1:3),lty=1,lwd=1,bty="n",cex=1.1)

par(mfrow=c(1,1))
gg = cbind(vehtotf4, yf, lconf)
matplot(gg,type='l',col=c(3,3,1),lwd=1,lty=1,main="Comparison")


# forecast error = actual - predicted
ferr = vehtotf4 - lconf
rmspe_var = sqrt(mean(ferr^2))
rmspe_var

ferr = vehtotf4 - yf
rmspe_ar4 = sqrt(mean(ferr^2))
rmspe_ar4


##############################
##############################
y = dinct
# plot actual, predicted and residuals
par(mfrow=c(2,1))
xx <- cbind(y,r$fitted.values)
matplot(xx,col=1:2,type='l',lwd=1,lty=1,main="model A actual & fitted")
legend("topright", legend=c("Actual", "Fitted"), col=c("black", "red"), lty=1, cex=0.6)

plot(r$residuals,col=4,type='l',lwd=1, main="model A residuals")
abline(h=1,col=2)


#--------------------------------------------------------
# Visualization 
#--------------------------------------------------------

par(mfrow=c(2,2))
plot(contot, type="l")
plot(miletot, type="l")
plot(vehtot, type="l")


plot(contot, type="l")
plot(miletot, type="l")
plot(vehtot, type="l")
