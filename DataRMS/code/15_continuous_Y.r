## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('continuousY', cache=TRUE)
knitr::read_chunk('~/doc/rms/continuousY/shared.R')


## ----lmassump,h=3.25,w=7,mfrow=c(1,2),ps=10,mgp=c(.5, .365, 0),left=1,cap='Assumptions of the linear model (left panel) and semiparametric ordinal probit or logit (proportional odds) models (right panel).  Ordinal models do not assume any shape for the distribution of $Y$ for a given $X$; they only assume parallelism.  The linear model can relax the parallelism assumption if $\\sigma$ is allowed to vary, but in practice it is difficult to know how to vary it except for the unequal variance two-sample $t$-test.',scap='Assumptions of linear vs.\\ semiparametric models',echo=FALSE----
pinv <- expression(paste(Phi^{-1},  '(F(y', '|', 'X))'))
plot(0, 0, xlim=c(0, 1), ylim=c(-2, 2), type='n', axes=FALSE,
     xlab=expression(y), ylab='')
mtext(pinv, side=2, line=1)
axis(1, labels=FALSE, lwd.ticks=0)
axis(2, labels=FALSE, lwd.ticks=0)
abline(a=-1.5, b=1)
abline(a=0, b=1)
arrows(.5, -1.5+.5, .5, 0+.5, code=3, length=.1)
text(.525, .5*(-1.5+.5+.5), expression(-Delta*X*beta/sigma), adj=0)
g <- function(x) -2.2606955+11.125231*x-37.772783*x^2+56.776436*x^3-
  26.861103*x^4
x <- seq(0, .9, length=150)
pinv <- expression(atop(paste(Phi^{-1},  '(F(y', '|', 'X))'),
    paste(logit, '(F(y', '|', 'X))')))
plot(0, 0, xlim=c(0, 1), ylim=c(-2, 2), type='n', axes=FALSE,
     xlab=expression(y), ylab='')
mtext(pinv, side=2, line=1)
axis(1, labels=FALSE, lwd.ticks=FALSE)
axis(2, labels=FALSE, lwd.ticks=FALSE)
lines(x, g(x))
lines(x, g(x)+1.5)
arrows(.5, g(.5), .5, g(.5)+1.5, code=3, length=.1)
text(.525, .5*(g(.55) + g(.55)+1.5), expression(-Delta*X*beta), adj=0)


## ----desc,results='asis',cache=FALSE------------------------------------------
require(rms)
options(prType='latex')   # for print, summary, anova
getHdata(nhgh)
w <- subset(nhgh, age >= 21 & dx==0 & tx==0, select=-c(dx,tx))
latex(describe(w), file='')
dd <- datadist(w); options(datadist='dd')


## ----lookdist,w=5.5,h=5.5,cap='Examination of normality and constant variance assumption, and assumptions for various ordinal models',scap='Examining normality and ordinal model assumptions'----
f <- ols(gh ~ rcs(age,5) + sex + re + rcs(bmi, 3), data=w)
pgh <- fitted(f)

p <- function(fun, row, col) {
  f <- substitute(fun); g <- function(F) eval(f)
  z <- Ecdf(~ gh, groups=cut2(pgh, g=6),
            fun=function(F) g(1 - F),
            ylab=as.expression(f), xlim=c(4.5, 7.75), data=w,
            label.curve=FALSE)
  print(z, split=c(col, row, 2, 2), more=row < 2 | col < 2)
}
p(log(F/(1-F)),   1, 1)
p(qnorm(F),       1, 2)
p(-log(-log(F)),  2, 1)
p(log(-log(1-F)), 2, 2)
# Get slopes of pgh for some cutoffs of Y
# Use glm complementary log-log link on Prob(Y < cutoff) to
# get log-log link on Prob(Y >= cutoff)
r <- NULL
for(link in c('logit','probit','cloglog'))
  for(k in c(5, 5.5, 6)) {
    co <- coef(glm(gh < k ~ pgh, data=w, family=binomial(link)))
    r <- rbind(r, data.frame(link=link, cutoff=k,
                             slope=round(co[2],2)))
}
print(r, row.names=FALSE)


## ----comparemany,cache=TRUE,h=6.5,w=6.75,cap='Three estimated quantiles and estimated mean using 6 methods, compared against caliper-matched sample quantiles/means (circles).  Numbers are mean absolute differences between predicted and sample quantities using overlapping intervals of age and caliper matching\\index{caliper matching}.  QR:quantile regression.',scap='Six methods for estimating quantiles or means.'----
ag <- 25:75
lag <- length(ag)
q2 <- q3 <- p90 <- means <- numeric(lag)
for(i in 1:lag) {
  s <- which(abs(w$age - ag[i]) < 5)
  y <- w$gh[s]
  a <- quantile(y, probs=c(.5, .75, .9))
  q2[i]    <- a[1]
  q3[i]    <- a[2]
  p90[i]   <- a[3]
  means[i] <- mean(y)
}
fams <- c('logistic', 'probit', 'loglog', 'cloglog')
fe   <- function(pred, target) mean(abs(pred$yhat - target))
mod  <- gh ~ rcs(age,6)
P    <- Er <- list()
for(est in c('q2', 'q3', 'p90', 'mean')) {
  meth <- if(est == 'mean') 'ols' else 'QR'
  p <- list()
  er <- rep(NA, 5)
  names(er) <- c(fams, meth)
  for(family in fams) {
    h  <- orm(mod, family=family, data=w)
    fun <- if(est == 'mean') Mean(h)
    else {
      qu <- Quantile(h)
      switch(est, q2  = function(x) qu(.5,  x),
                  q3  = function(x) qu(.75, x),
                  p90 = function(x) qu(.9,  x))
    }
    p[[family]] <- z <- Predict(h, age=ag, fun=fun, conf.int=FALSE)
    er[family] <- fe(z, switch(est, mean=means, q2=q2, q3=q3, p90=p90))
  }
  h <- switch(est,
              mean= ols(mod, data=w),
              q2  = Rq (mod, data=w),
              q3  = Rq (mod, tau=0.75, data=w),
              p90 = Rq (mod, tau=0.90, data=w))
  p[[meth]] <- z <- Predict(h, age=ag, conf.int=FALSE)
  er[meth] <- fe(z, switch(est, mean=means, q2=q2, q3=q3, p90=p90))

  Er[[est]] <- er
  pr <- do.call('rbind', p)
  pr$est <- est
  P <- rbind.data.frame(P, pr)
}

xyplot(yhat ~ age | est, groups=.set., data=P, type='l', # Figure (*\ref{fig:continuousY-comparemany}*)
       auto.key=list(x=.75, y=.2, points=FALSE, lines=TRUE),
       panel=function(..., subscripts) {
         panel.xyplot(..., subscripts=subscripts)
         est <- P$est[subscripts[1]]
         lpoints(ag, switch(est, mean=means, q2=q2, q3=q3, p90=p90),
                 col=gray(.7))
         er <- format(round(Er[[est]],3), nsmall=3)
         ltext(26, 6.15, paste(names(er), collapse='\n'),
               cex=.7, adj=0)
         ltext(40, 6.15, paste(er, collapse='\n'),
               cex=.7, adj=1)})


## ----predobs,w=4.75,h=4,cap='Observed (dashed lines, open circles) and predicted (solid lines, closed circles) exceedance probability distributions from a model using 6-tiles of OLS-predicted \\hba.  Key shows quantile group intervals of predicted mean \\hba.',scap='Observed and predicted distributions'----
w$pghg <- cut2(pgh, g=6)
f  <- orm(gh ~ pghg, family=loglog, data=w)
lp <- predict(f, newdata=data.frame(pghg=levels(w$pghg)))
ep <- ExProb(f)  # Exceedance prob. functn. generator in rms
z  <- ep(lp)
j  <- order(w$pghg)  # puts in order of lp (levels of pghg)
plot(z, xlim=c(4, 7.5), data=w[j,c('pghg', 'gh')]) # Fig. (*\ref{fig:continuousY-predobs}*)


## ----lookprobit,w=3,h=2.5,bty='l',cap='Estimated intercepts from probit model.  Linearity would have indicated Gaussian residuals.',scap='Estimated intercepts from probit model'----
f <- orm(gh ~ rcs(age,6), family=probit, data=w)
g <- ols(gh ~ rcs(age,6), data=w)
s <- g$stats['Sigma']
yu <- f$yunique[-1]
r <- quantile(w$gh, c(.005, .995))
alphas <- coef(f)[1:num.intercepts(f)]
plot(-yu / s, alphas, type='l', xlim=rev(- r / s), # Fig. (*\ref{fig:continuousY-lookprobit}*)
     xlab=expression(-y/hat(sigma)), ylab=expression(alpha[y]))


## ----htwtcoef,results='asis'--------------------------------------------------
f <- orm(gh ~ rcs(age,5) + log(ht) + log(wt),
         family=loglog, data=w)
f

## ----aichtwt------------------------------------------------------------------
aic <- NULL
for(mod in list(gh ~ rcs(age,5) + rcs(log(bmi),5),
                gh ~ rcs(age,5) + rcs(log(ht),5) + rcs(log(wt),5),
                gh ~ rcs(age,5) + rcs(log(ht),4) * rcs(log(wt),4)))
  aic <- c(aic, AIC(orm(mod, family=loglog, data=w)))
print(aic)


## ----coxhtwtcoef,results='asis'-----------------------------------------------
print(cph(Surv(gh) ~ rcs(age,5) + log(ht) + log(wt), data=w))


## ----redun,cap='Variable clustering for all potential predictors'-------------
v <- varclus(~ wt + ht + bmi + leg + arml + armc + waist +
             tri + sub + age + sex + re, data=w)
plot(v)   # Figure (*\ref{fig:continuousY-redun}*)
# Omit wt so it won't be removed before bmi
redun(~ ht + bmi + leg + arml + armc + waist + tri + sub,
      data=w, r2=.75)


## ----htchange,fig.align='right',cap="Estimated median height as a smooth function of age, allowing age to interact with sex, from a proportional odds model",scap="Median height vs.\\ age"----
f <- orm(ht ~ rcs(age,4)*sex, data=w)  # Prop. odds model
qu <- Quantile(f); med <- function(x) qu(.5, x)
ggplot(Predict(f, age, sex, fun=med, conf.int=FALSE),
       ylab='Predicted Median Height, cm')


## ----allocadf,w=4,cap='Generalized squared rank correlations',top=1-----------
s <- spearman2(gh ~ age + sex + re + wt + leg + arml + armc +
               waist + tri + sub, data=w, p=2)
plot(s)


## ----fitfullcasewise,results='asis'-------------------------------------------
f <- orm(gh ~ rcs(age,5) + sex + re + rcs(wt,3) + rcs(leg,3) + arml +
         rcs(armc,3) + rcs(waist,4) + tri + rcs(sub,3),
         family=loglog, data=w, x=TRUE, y=TRUE)
print(f, coefs=FALSE)
## Composite test:
anova(f, leg, arml, armc, waist, tri, sub)


## ----casewisemeanmed,h=4,w=5,cap='Estimated mean and 0.5 and 0.9 quantiles from the log-log ordinal model using casewise deletion, along with predictions of 0.5 and 0.9 quantiles from quantile regression (QR).  Age is varied and other predictors are held constant to medians/modes.',scap='Estimated mean and quantiles from casewise deletion model.'----
M      <- Mean(f)
qu     <- Quantile(f)
med    <- function(x) qu(.5, x)
p90    <- function(x) qu(.9, x)
fq     <- Rq(formula(f), data=w)
fq90   <- Rq(formula(f), data=w, tau=.9)
pmean  <- Predict(f,    age, fun=M,   conf.int=FALSE)
pmed   <- Predict(f,    age, fun=med, conf.int=FALSE)
p90    <- Predict(f,    age, fun=p90, conf.int=FALSE)
pmedqr <- Predict(fq,   age, conf.int=FALSE)
p90qr  <- Predict(fq90, age, conf.int=FALSE)
z <- rbind('orm mean'=pmean, 'orm median'=pmed, 'orm P90'=p90,
           'QR median'=pmedqr, 'QR P90'=p90qr)
ggplot(z, groups='.set.',
       adj.subtitle=FALSE, legend.label=FALSE)

## ----prbw---------------------------------------------------------------------
print(fastbw(f, rule='p'), estimates=FALSE)

## ----valbworm,cache=TRUE------------------------------------------------------
set.seed(13)  # so can reproduce results
v <- validate(f, B=100, bw=TRUE, estimates=FALSE, rule='p')

## ----prval,results='asis'-----------------------------------------------------
# Show number of variables selected in first 30 boots
latex(v, B=30, file='', size='small')


## ----sanova,cache=TRUE,results='asis',cap='ANOVA for reduced model, after multiple imputation, with addition of a combined effect for four size variables',scap='ANOVA for reduced model'----
a <- aregImpute(~ gh + wt + ht + bmi + leg + arml + armc + waist +
                tri + sub + age +re, data=w, n.impute=5, pr=FALSE)
g <- fit.mult.impute(gh ~ rcs(age,5) + re + rcs(leg,3) +
                     rcs(waist,4) + tri + rcs(sub,4),
                     orm, a, family=loglog, data=w, pr=FALSE)
print(g, needspace='1.5in')
an <- anova(g)
print(an, caption='ANOVA for reduced model after multiple imputation, with addition of a combined effect for four size variables')
b  <- anova(g, leg, waist, tri, sub)
# Add new lines to the plot with combined effect of 4 size var.
s <- rbind(an, size=b['TOTAL', ])
class(s) <- 'anova.rms'
plot(s)


## ----peffects,cache=TRUE,results='asis',cap='Partial effects (log hazard or log-log cumulative probability scale) of all predictors in reduced model, after multiple imputation',scap='Partial effects after multiple imputation',w=6.75,h=4.5,cache=TRUE----
ggplot(Predict(g), abbrev=TRUE, ylab=NULL)   # Figure (*\ref{fig:continuousY-peffects}*)


## ----cfmissmeth,cache=TRUE----------------------------------------------------
gc <- orm(gh ~ rcs(age,5) + re + rcs(leg,3) +
          rcs(waist,4) + tri + rcs(sub,4),
          family=loglog, data=w, x=TRUE, y=TRUE)
gb <- bootcov(gc, B=300)

## ----peffects2,cap='Partial effect for age from multiple imputation (center red line) and casewise deletion (center blue line) with symmetric Wald 0.95 confidence bands using casewise deletion (gray shaded area), basic bootstrap confidence bands using casewise deletion (blue lines), percentile bootstrap confidence bands using casewise deletion (dashed blue lines), and symmetric Wald confidence bands accounting for multiple imputation (red lines).',scap='Partial effect for age with bootstrap and Wald confidence bands',w=5,h=4,bot=1----
bootclb <- Predict(gb, age, boot.type='basic')
bootclp <- Predict(gb, age, boot.type='percentile')
multimp <- Predict(g,  age)
plot(Predict(gc, age), addpanel=function(...) {
  with(bootclb, {llines(age, lower, col='blue')
                 llines(age, upper, col='blue')})
  with(bootclp, {llines(age, lower, col='blue', lty=2)
                 llines(age, upper, col='blue', lty=2)})
  with(multimp, {llines(age, lower, col='red')
                 llines(age, upper, col='red')
                 llines(age, yhat, col='red')} ) },
     col.fill=gray(.9), adj.subtitle=FALSE)   # Figure (*\ref{fig:continuousY-peffects2}*)


## ----meanvs,cap='Predicted mean \\hba vs.\\ predicted median and 0.9 quantile along with their marginal distributions',scap='Predicted mean, median, and 0.9 quantile of \\hba',w=4.5,h=3.5----
M  <- Mean(g)
qu <- Quantile(g)
med <- function(lp) qu(.5, lp)
q90 <- function(lp) qu(.9, lp)
lp  <- predict(g)
lpr <- quantile(predict(g), c(.002, .998), na.rm=TRUE)
lps <- seq(lpr[1], lpr[2], length=200)
pmn <- M(lps)
pme <- med(lps)
p90 <- q90(lps)
plot(pmn, pme,   # Figure (*\ref{fig:continuousY-meanvs}*)
     xlab=expression(paste('Predicted Mean ',  HbA["1c"])),
     ylab='Median and 0.9 Quantile', type='l',
     xlim=c(4.75, 8.0), ylim=c(4.75, 8.0), bty='n')
box(col=gray(.8))
lines(pmn, p90, col='blue')
abline(a=0, b=1, col=gray(.8))
text(6.5, 5.5, 'Median')
text(5.5, 6.3, '0.9', col='blue')
nint <- 350
scat1d(M(lp),   nint=nint)
scat1d(med(lp), side=2, nint=nint)
scat1d(q90(lp), side=4, col='blue', nint=nint)


## ----nomogram,cap='Nomogram for predicting median, mean, and 0.9 quantile of glycohemoglobin, along with the estimated probability that \\hba $\\ge 6.5, 7$, or $7.5$, all from the log-log ordinal model',scap='Nomogram of log-log ordinal model for \\hba',w=6.75,h=5.75,ps=9----
g      <- Newlevels(g, list(re=abbreviate(levels(w$re))))
exprob <- ExProb(g)
nom <-
  nomogram(g, fun=list(Mean=M,
                'Median Glycohemoglobin' = med,
                '0.9 Quantile'           = q90,
                'Prob(HbA1c >= 6.5)'=
                     function(x) exprob(x, y=6.5),
                'Prob(HbA1c >= 7.0)'=
                     function(x) exprob(x, y=7),
                'Prob(HbA1c >= 7.5)'=
                     function(x) exprob(x, y=7.5)),
           fun.at=list(seq(5, 8, by=.5),
             c(5,5.25,5.5,5.75,6,6.25),
             c(5.5,6,6.5,7,8,10,12,14),
             c(.01,.05,.1,.2,.3,.4),
             c(.01,.05,.1,.2,.3,.4),
             c(.01,.05,.1,.2,.3,.4)))
plot(nom, lmgp=.28)   # Figure (*\ref{fig:continuousY-nomogram}*)

