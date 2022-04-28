intord = function(y){
#
# purpose: determine order of integration for variable y.
# 
# usage: out = intord(y)
# This creates time plots, calculates SDs
#  and performs an ADF unit root test for
# for level, 1st & 2nd difference of y
#
# out contains ADF t-statistics and Mackinnon (2010) critical values.
#
# Critical values from:
# MacKinnon, J.G. (2010) Critical Values for Cointegration Tests.
# Queen’s Economics Department Working Paper No. 1227, Table 2.
#
# Jeff Mills, 2011
#

n = length(y)

dy = diff(y)
d2y = diff(dy)

z = cbind(y[3:n],dy[2:(n-1)],d2y)

par(mfrow=c(3,2))
plot(z[,1],col=1,type='l',main=paste("SD=",round(sd(z[,1]),4)),ylab="y", xlab="Level")
abline(h=mean(z[,1]),col=2)
acf(y,lwd=3,main="ACF for level")
plot(z[,2],col=3,type='l',main=paste("SD=",round(sd(z[,2]),4)),ylab=expression(paste(Delta * y)), xlab="1st difference")
abline(h=mean(z[,2]),col=2)
acf(z[,2],lwd=3,main="ACF for 1st difference")
plot(z[,3],col=4,type='l',main=paste("SD=",round(sd(z[,3]),4)),ylab=expression(paste(Delta^2 * y)), xlab="2nd difference")
abline(h=mean(z[,3]),col=2)
plot(1:10, xaxt='n', yaxt ='n', col="white", main="ADF test & critical values", ylab="", xlab="")



# ADF test first round

# pmax is selected max lag length, maxp must be at least 2
pmax = 12
maxp = pmax + 1


n = length(y)
dy = diff(y)


z = embed(dy,maxp)

zz = embed(y,maxp); y1 = zz[,2]
xx = cbind(z[,1],y[maxp:(n-1)],z[,2:maxp])
nobs = nrow(xx)
# DF test (0 lags)
c = rep(1,nrow(xx))
xvars = cbind(c,y[maxp:(n-1)])
yvar = xx[,1]
ixx = solve(t(xvars)%*%xvars)
bh = ixx%*%t(xvars)%*%yvar
yh = xvars%*%bh
res = yvar - yh
rss = t(res)%*%res
k = ncol(xvars)
s2 = as.numeric(rss/(nobs-k))
covb = s2*ixx
seb = sqrt(diag(covb))

bic = rep(0,maxp)
adft = bic
adft[1] = bh[2]/seb[2]
bic[1] = log(rss/nobs) + log(nobs)*(k+1)/nobs

for (i in 3:(maxp+1)) {
xvars = cbind(c,xx[,2:i])
ixx = solve(t(xvars)%*%xvars)
bh = ixx%*%t(xvars)%*%yvar
yh = xvars%*%bh
res = yvar - yh
rss = t(res)%*%res
k = ncol(xvars)
s2 = as.numeric(rss/(nobs-k))
covb = s2*ixx
seb = sqrt(diag(covb))
adft[i-1] = bh[2]/seb[2]
bic[i-1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
}

ind = which.min(bic)
# cat("ADF t-value","lags")
round1 = c(round(adft[ind],2))

# ADF test second round

y = dy

n = length(y)
dy = diff(y)


z = embed(dy,maxp)

zz = embed(y,maxp); y1 = zz[,2]
xx = cbind(z[,1],y[maxp:(n-1)],z[,2:maxp])
nobs = nrow(xx)
# DF test (0 lags)
c = rep(1,nrow(xx))
xvars = cbind(c,y[maxp:(n-1)])
yvar = xx[,1]
ixx = solve(t(xvars)%*%xvars)
bh = ixx%*%t(xvars)%*%yvar
yh = xvars%*%bh
res = yvar - yh
rss = t(res)%*%res
k = ncol(xvars)
s2 = as.numeric(rss/(nobs-k))
covb = s2*ixx
seb = sqrt(diag(covb))

bic = rep(0,maxp)
adft = bic
adft[1] = bh[2]/seb[2]
bic[1] = log(rss/nobs) + log(nobs)*(k+1)/nobs

for (i in 3:(maxp+1)) {
xvars = cbind(c,xx[,2:i])
ixx = solve(t(xvars)%*%xvars)
bh = ixx%*%t(xvars)%*%yvar
yh = xvars%*%bh
res = yvar - yh
rss = t(res)%*%res
k = ncol(xvars)
s2 = as.numeric(rss/(nobs-k))
covb = s2*ixx
seb = sqrt(diag(covb))
adft[i-1] = bh[2]/seb[2]
bic[i-1] = log(rss/nobs) + log(nobs)*(k+1)/nobs
}
bic
ind = which.min(bic)
cat("ADF t-value","lags")
round2 = c(round(adft[ind],2))
rbind(round1,round2)

ADF.statistics = cbind(round1,round2)

# MacKinnon critical values
c1 = -3.43035 - 6.5393/nobs - 16.786/nobs^2 - 79.433/nobs^3
c5 = -2.86154 - 2.8903/nobs - 4.234/nobs^2 - 40.04/nobs^3
c10 = -2.56677 - 1.5384/nobs - 2.809/nobs^2

# cat("10%, 5% and 1% critical values")
Critical.values = round(c(c10,c5,c1),2)


line1<-expression(paste("   ",y,"           ", Delta * y,"        ","10%       5%	      1%"))
line2<-paste(ADF.statistics[1],"    ",ADF.statistics[2],"  "
             ,Critical.values[1],"   ",Critical.values[2],"   ",Critical.values[3])

legend("center","(x,y)", # places a legend at the appropriate place 
       c(line1,line2), # puts text in the legend 
       lty=c(1,2), # gives the legend appropriate symbols (lines)       
       lwd=c(3,2),col=c("white","white"),bty = "n",cex=1.3) # gives the legend lines the correct color and width

ADF.statistics
Critical.values


list(adf.stat = ADF.statistics, critvals = Critical.values)



}

