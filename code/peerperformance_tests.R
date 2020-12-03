library(tiderverse)
library(PeerPerformance)
library(skimr)
library(summarytools)

data(hfdata)
skim(hfdata)
descr(hfdata)

out = msharpe(hfdata)
print(out)

out = msharpe(hfdata, na.rm = FALSE)
print(out)

rets = hfdata[,1:10]ctr = list(nCore = 1)
alphaScreening(rets, control = ctr)

## Run alpha screening with HAC standard deviation
ctr = list(nCore = 1, hac = TRUE)
alphaScreening(rets, control = ctr)



msharpeScreening(rets, control = list(nCore = 1))

## Modified Sharpe screening with bootstrap and HAC standard deviation
msharpeScreening(rets, control = list(nCore = 1, type = 2, hac = TRUE))


x = hfdata[,1]
y = hfdata[,2]

ctr = list(type = 1)
out = msharpeTesting(x, y, level = 0.95, control = ctr)
print(out)

## Run modified Sharpe testing (asymptotic hac)
ctr = list(type = 1, hac = TRUE)
out = msharpeTesting(x, y, level = 0.95, control = ctr)
print(out)

out = sharpe(hfdata)
print(out)

## Run Sharpe testing (asymptotic)
ctr = list(type = 1)
out = sharpeTesting(x, y, control = ctr)
print(out)
## Run Sharpe testing (asymptotic hac)
ctr = list(type = 1, hac = TRUE)
out = sharpeTesting(x, y, control = ctr)
print(out)
## Run Sharpe testing (iid bootstrap)
set.seed(1234)
ctr = list(type = 2, nBoot = 250)
out = sharpeTesting(x, y, control = ctr)
print(out)
## Run Sharpe testing (circular bootstrap)
set.seed(1234)
ctr = list(type = 2, nBoot = 250, bBoot = 5)
out = sharpeTesting(x, y, control = ctr)
print(out)

