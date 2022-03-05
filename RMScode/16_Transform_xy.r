## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('areg')


## ----eval=FALSE---------------------------------------------------------------
## f <- areg.boot(Y ~ monotone(age) +
##                sex + weight + I(blood.pressure))
## 
## plot(f)       #show transformations, CLs
## Function(f)   #generate S functions
##               #defining transformations
## predict(f)    #get predictions, smearing estimates
## summary(f)    #compute CLs on effects of each X
## smearingEst() #generalized smearing estimators
## Mean(f)       #derive S function to
##               #compute smearing mean Y
## Quantile(f)   #derive function to compute smearing quantile
## 


## ----sim,results='asis'-------------------------------------------------------
require(rms)
set.seed(7)
n <- 400
x1 <- runif(n)
x2 <- runif(n)
x3 <- factor(sample(c('a','b','c'), n, TRUE))
y  <- exp(x1 + 2*abs(x2 - .5) + .3*(x3=='b') + .5*(x3=='c') +
          .5*rnorm(n))
# For reference fit appropriate OLS model
print(ols(log(y) ~ x1 + rcs(x2, 5) + x3), coefs=FALSE,
      latex=TRUE)


## ----aregboot,results='hide'--------------------------------------------------
f  <- areg.boot(y ~ x1 + x2 + x3, method='avas', B=300)

## ----pareg--------------------------------------------------------------------
f


## ----trans,mfrow=c(2,2),h=4,w=5,ps=10,cap='\\fu{avas} transformations: overall estimates, pointwise $0.95$ confidence bands, and $20$ bootstrap estimates (red lines).',scap='Transformations estimated by \\fu{avas}'----
plot(f, boot=20) # Figure (*\ref{fig:areg-trans}*)


## ----ytrans,w=3,h=2,bty='l',cap='Checking estimated against optimal transformation'----
ys <- seq(.8, 20, length=200)
ytrans <- Function(f)$y   # Function outputs all transforms
plot(log(ys), ytrans(ys), type='l')   # Figure (*\ref{fig:areg-ytrans}*)
abline(lm(ytrans(ys) ~ log(ys)), col=gray(.8))


## -----------------------------------------------------------------------------
summary(f, values=list(x1=c(.2, .8), x2=c(.1, .5)))


## ----pred,h=3.5,w=4.75,cap='Predicted median (left panel) and mean (right panel) \\co{y} as a function of \\co{x2} and \\co{x3}.  True population values are shown in gray.',scap='Predicted \\co{y} as a function of \\co{x2} and \\co{x3}'----
newdat <- expand.grid(x2=seq(.05, .95, length=200), 
                      x3=c('a','b','c'), x1=.5,
                      statistic=c('median','mean'))
yhat <- c(predict(f, subset(newdat, statistic=='median'),
                  statistic='median'),
          predict(f, subset(newdat, statistic=='mean'),
                  statistic='mean'))
newdat <-
  upData(newdat,
         lp = x1 + 2*abs(x2 - .5) + .3*(x3=='b') +
              .5*(x3=='c'),
         ytrue = ifelse(statistic=='median', exp(lp),
           exp(lp + 0.5*(0.5^2))), pr=FALSE)

# Use Hmisc function xYplot to produce Figure (*\ref{fig:areg-pred}*)
xYplot(yhat ~ x2 | statistic, groups=x3,
       data=newdat, type='l', col=1,
       ylab=expression(hat(y)),
       panel=function(...) {
         panel.xYplot(...)
         dat <- subset(newdat,
           statistic==c('median','mean')[current.column()])
         for(w in c('a','b','c')) 
           with(subset(dat, x3==w),
                llines(x2, ytrue, col=gray(.7), lwd=1.5))
       }
     )

