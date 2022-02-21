## @knitr kprats-cph-np
require(rms)
options(prType='latex')
group <- c(rep('Group 1',19),rep('Group 2',21))
group <- factor(group)
dd    <-  datadist(group); options(datadist='dd')
days <-
  c(143,164,188,188,190,192,206,209,213,216,220,227,230,
    234,246,265,304,216,244,142,156,163,198,205,232,232,
    233,233,233,233,239,240,261,280,280,296,296,323,204,344)
death <- rep(1,40)
death[c(18,19,39,40)] <- 0
units(days) <- 'Day'
df <- data.frame(days, death, group)
S <- Surv(days, death)

f <- npsurv(S ~ group, type='fleming')
for(meth in c('exact', 'breslow', 'efron')) {
  g <- cph(S ~ group, method=meth, surv=TRUE, x=TRUE, y=TRUE)
  # print(g) to see results
}
f.exp <- psm(S ~ group, dist='exponential')
fw    <- psm(S ~ group, dist='weibull')
phform <- pphsm(fw)

co <- gray(c(0, .8))
survplot(f, lty=c(1, 1), lwd=c(1, 3), col=co,
         label.curves=FALSE, conf='none')
survplot(g, lty=c(3, 3), lwd=c(1, 3), col=co,  # Efron approx.
         add=TRUE, label.curves=FALSE, conf.type='none')
legend(c(2, 160), c(.38, .54),
       c('Nonparametric Estimates', 'Cox-Breslow Estimates'),
       lty=c(1, 3), cex=.8, bty='n')
legend(c(2, 160), c(.18, .34), cex=.8,
       c('Group 1', 'Group 2'), lwd=c(1,3), col=co, bty='n')

## @knitr kprats-cumhaz-ratio
f <- cph(S ~ strat(group), surv=TRUE)
# For both strata, eval. S(t) at combined set of death times
times <- sort(unique(days[death == 1]))
est   <- survest(f, data.frame(group=levels(group)),
                 times=times, conf.type="none")$surv
cumhaz  <- - log(est)
plot(times, cumhaz[2,] / cumhaz[1,], xlab="Days", 
     ylab="Cumulative Hazard Ratio", type="s")
abline(h=1, col=gray(.80))

## @knitr kprats-hazard-ratios
hazard.ratio.plot(g$x, g$y, e=12, pr=TRUE, legendloc='none')


## @knitr km-age-sex
n <- 2000
set.seed(3)
age <- 50 + 12 * rnorm(n)
label(age) <- 'Age'
sex <- factor(1 + (runif(n) <= .4), 1:2, c('Male', 'Female'))
cens <- 15 * runif(n)
h <- .02 * exp(.04 * (age - 50) + .8 * (sex == 'Female'))
ft <- -log(runif(n)) / h
e <- ifelse(ft <= cens, 1, 0)
print(table(e))
ft <- pmin(ft, cens)
units(ft) <- 'Year'
Srv <- Surv(ft, e)
age.dec <- cut2(age, g=10, levels.mean=TRUE)
label(age.dec) <- 'Age'
dd <- datadist(age, sex, age.dec);  options(datadist='dd')
f.np <- cph(Srv ~ strat(age.dec) + strat(sex), surv=TRUE)
# surv=TRUE speeds up computations, and confidence limits when
# there are no covariables are still accurate.
p <- Predict(f.np, age.dec, sex, time=3, loglog=TRUE)
# Treat age.dec as a numeric variable (means within deciles)
p$age.dec <- as.numeric(as.character(p$age.dec))
ggplot(p, ylim=c(-5, -.5))

## @knitr spline-age-sex-noia
f.noia <- cph(Srv ~ rcs(age,4) + strat(sex), x=TRUE, y=TRUE) 
# Get accurate C.L. for any age by specifying x=TRUE y=TRUE
# Note: for evaluating shape of regression, we would not
# ordinarily bother to get 3-year survival probabilities -
# would just use X * beta
# We do so here to use same scale as nonparametric estimates
w <- latex(f.noia, file='f.noia.tex', inline=TRUE, digits=3)
print(anova(f.noia), size='normalsize')
p <- Predict(f.noia, age, sex, time=3, loglog=TRUE)
ggplot(p, ylim=c(-5, -.5))


## @knitr spline-age-sex-ia
f.ia <- cph(Srv ~ rcs(age,4) * strat(sex), x=TRUE, y=TRUE,
            surv=TRUE)
w <- latex(f.ia, file='f.ia.tex', inline=TRUE, digits=3)
print(anova(f.ia), size='normalsize')
p <- Predict(f.ia, age, sex, time=3, loglog=TRUE)
ggplot(p, ylim=c(-5, -.5))


## @knitr spline-age-sex-ia-surv
p <- Predict(f.ia, age, sex, time=3)
ggplot(p)


## @knitr spline-age-sex-ia-nomogram
surv    <- Survival(f.ia)
surv.f  <- function(lp) surv(3, lp, stratum='sex=Female')
surv.m  <- function(lp) surv(3, lp, stratum='sex=Male')
quant   <- Quantile(f.ia)
med.f   <- function(lp) quant(.5, lp, stratum='sex=Female')
med.m   <- function(lp) quant(.5, lp, stratum='sex=Male')
at.surv <- c(.01, .05, seq(.1,.9,by=.1), .95, .98, .99, .999)
at.med  <- c(0, .5, 1, 1.5, seq(2, 14, by=2))
n <- nomogram(f.ia, fun=list(surv.m, surv.f, med.m,med.f),
         funlabel=c('S(3 | Male)','S(3 | Female)',
                    'Median (Male)','Median (Female)'),
         fun.at=list(c(.8,.9,.95,.98,.99),
                     c(.1,.3,.5,.7,.8,.9,.95,.98),
                     c(8,10,12),c(1,2,4,8,12)))
plot(n, col.grid=FALSE, lmgp=.2)   # Fig. (*\ref{fig:cox-spline-age-sex-ia-nomogram}*)
latex(f.ia, digits=3)


## @knitr ef-spline
# acath2 <- sas.get('.','acath2')
if(FALSE) {
acath2 <- subset(acath2, ejfx == trunc(ejfx)) # non-imputed values
acath2 <- upData(acath2,
                 lvef = ejfx / 100,
                 labels=c(lvef = 'LVEF'))
nk <- 3
d <- datadist(acath2);  options(datadist='d')
with(acath2,
     rcspline.plot(lvef, d.time, model='cox', event=cdeath,
                   main='', statloc=c(.42, -1), nk=nk,
                   ylim=c(-4, -.9)))
if(nk == 3) print(cph(Surv(d.time, cdeath) ~ pmin(lvef,.5), data=acath2))
}

## @knitr ef.martingale
if(FALSE) {
cox <- cph(Surv(d.time,cdeath) ~ lvef, data=acath2, iter.max=0)
res <- resid(cox)
g <- loess(res ~ lvef, data=acath2)
plot(g, coverage=0.95, confidence=7, xlab='LVEF', ylab='Martingale Residual')

g <- ols(res ~ rcs(lvef, 5), data=acath2)
plot(Predict(g, lvef))  # not added to previous plot as really shown

with(acath2, {
  s <- ! is.na(res + lvef)
  lines(lowess(lvef[s], res[s], iter=0), lty=3)
  ## lowess doesn't handle NAs
})

legend(.20, 1.15, c('loess Fit and 0.95 Confidence Bars',
	'ols Spline Fit and 0.95 Confidence Limits',
	'lowess Smoother'), lty=1:3, bty='n', cex=cex.legend)
box()
}

## @knitr valung-ratios
getHdata(valung)
with(valung, {
  hazard.ratio.plot(1 * (cell == 'Squamous'), Surv(t, dead),
                    e=25, subset=cell != 'Large',
                    pr=TRUE, pl=FALSE)
  hazard.ratio.plot(1 * kps, Surv(t, dead), e=25,
                    pr=TRUE, pl=FALSE) })

## @knitr rel-random
n <- 200
p <-  20
set.seed(6)
xx <- matrix(rnorm(n * p), nrow=n, ncol=p)
y  <- runif(n)
units(y) <- "Year"
e   <- c(rep(0, n / 2), rep(1, n / 2))
f   <- cph(Surv(y, e) ~ xx, x=TRUE, y=TRUE,
           time.inc=.5, surv=TRUE)
cal <- calibrate(f, u=.5, B=200)
plot(cal, ylim=c(.4, 1), subtitles=FALSE)
calkm <- calibrate(f, u=.5, m=40,  cmethod='KM', B=200)
plot(calkm, add=TRUE)   # Figure (*\ref{fig:cox-rel-random}*)

## @knitr val-random
latex(validate(f, B=200), digits=3, file='',
      caption='Bootstrap validation of a Cox model with random predictors',
      table.env=TRUE, label='tab:cox-val-random')
