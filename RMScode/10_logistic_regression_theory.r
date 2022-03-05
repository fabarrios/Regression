## @knitr logistic-fun
x <- seq(-4,4,by=.05)
p <- 1/(1 + exp(-x))   # Figure (*\ref{fig:lrm-logistic-fun}*)
plot(x, p, xlab="X", ylab="P", type="l")


## @knitr or-vs-diff
plot(0, 0, type="n", xlab="Risk for Subject Without Risk Factor",
     ylab="Increase in Risk",
     xlim=c(0,1), ylim=c(0,.6))   # Figure (*\ref{fig:lrm-or-vs-diff}*)
i <- 0
or <- c(1.1,1.25,1.5,1.75,2,3,4,5,10)
for(h in or) {
  i <- i + 1
  p <- seq(.0001, .9999, length=200)
  logit <- log(p/(1 - p))  # same as qlogis(p)
  logit <- logit + log(h)  # modify by odds ratio
  p2 <- 1/(1 + exp(-logit))# same as plogis(logit)
  d <- p2 - p
  lines(p, d, lty=i)
  maxd <- max(d)
  smax <- p[d==maxd]
  text(smax, maxd + .02, format(h), cex=.6)
}


## @knitr lrmodel
require(rms)
getHdata(sex.age.response)
d <- sex.age.response
dd <- datadist(d); options(datadist='dd')
f <- lrm(response ~ sex + age, data=d)
fasr <- f   # Save for later
w <- function(...)
  with(d, {
    m <- sex=='male'
    f <- sex=='female'
    lpoints(age[f], response[f], pch=1)
    lpoints(age[m], response[m], pch=2)
    af <- cut2(age, c(45,55), levels.mean=TRUE)
    prop <- tapply(response, list(af, sex), mean,
                   na.rm=TRUE)
    agem <- as.numeric(row.names(prop))
    lpoints(agem, prop[,'female'],
            pch=4, cex=1.3, col='green')
    lpoints(agem, prop[,'male'],
            pch=5, cex=1.3, col='green')
    x <- rep(62, 4); y <- seq(.25, .1, length=4)
    lpoints(x, y, pch=c(1, 2, 4, 5),
            col=rep(c('blue','green'),each=2))
    ltext(x+5, y,
          c('F Observed','M Observed',
            'F Proportion','M Proportion'), cex=.8)
  } )   # Figure (*\ref{fig:lrm-lrmodel}*)

plot(Predict(f, age=seq(34, 70, length=200), sex, fun=plogis),
     ylab='Pr[response]', ylim=c(-.02, 1.02), addpanel=w)
ltx <- function(fit) latex(fit, inline=TRUE, columns=54,
                           file='', after='$.', digits=3,
        size='Ssize', before='$X\\hat{\\beta}=')
ltx(f)


## @knitr simerr
sigmas  <- c(.5, .75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3, 4)
ns      <- seq(25, 300, by=25)
nsim    <- 1000
xs      <- seq(-1.5, 1.5, length=200)
pactual <- plogis(xs)

dn <- list(sigma=format(sigmas), n=format(ns))
maxerr <- N1 <- array(NA, c(length(sigmas), length(ns)), dn)
require(rms)

i <- 0
for(s in sigmas) {
  i <- i + 1
  j <- 0
  for(n in ns) {
    j <- j + 1
    n1 <- maxe <- 0
    for(k in 1:nsim) {
      x <- rnorm(n, 0, s)
      P <- plogis(x)
      y <- ifelse(runif(n) <= P, 1, 0)
      n1 <- n1 + sum(y)
      beta <- lrm.fit(x, y)$coefficients
      phat <- plogis(beta[1] + beta[2] * xs)
      maxe <- maxe + max(abs(phat - pactual))
    }
    n1 <- n1/nsim
    maxe <- maxe/nsim
    maxerr[i,j] <- maxe
    N1[i,j] <- n1
  }
}
xrange <- range(xs)
simerr <- llist(N1, maxerr, sigmas, ns, nsim, xrange)

maxe <- reShape(maxerr)
# Figure (*\ref{fig:lrm-simerr}*)
xYplot(maxerr ~ n, groups=sigma, data=maxe,
       ylab=expression(paste('Average Maximum  ',
           abs(hat(P) - P))),
       type='l', lty=rep(1:2, 5), label.curve=FALSE,
       abline=list(h=c(.15, .1, .05), col=gray(.85)))
Key(.8, .68, other=list(cex=.7,
               title=expression(~~~~~~~~~~~sigma)))


## @knitr assumptions
plot(0:1, 0:1, xlab=expression(X[2]), ylab='logit{Y=1}',
     axes=FALSE,  type='n')
axis(1, at=0:1, labels=rep('',2))
axis(2, at=0:1, labels=rep('',2))
lines(c(.05, .8), c(.05, .5))
lines(c(.05, .8), c(.30, .75))
text(.9, .5, expression(X[1]==0), adj=.5)
text(.9, .75,expression(X[1]==1), adj=.5)


## @knitr emp-age-sex
getHdata(acath)
acath$sex <- factor(acath$sex, 0:1, c('male','female'))
dd <- datadist(acath); options(datadist='dd')
f <- lrm(sigdz ~ rcs(age, 4) * sex, data=acath)

## @knitr emp-age-sex-kn
kn <- specs(f)$how.modeled['age','Parameters']
kn <- setdiff(strsplit(kn, ' ')[[1]], '')
kn[length(kn)] <- paste('and', kn[length(kn)])
kn <- paste(kn, collapse=', ')

## @knitr emp-age-sex-pl
w <- function(...)
  with(acath, {
    plsmo(age, sigdz, group=sex, fun=qlogis, lty='dotted',
          add=TRUE, grid=TRUE)
    af <- cut2(age, g=10, levels.mean=TRUE)
    prop <- qlogis(tapply(sigdz, list(af, sex), mean,
                          na.rm=TRUE))
    agem <- as.numeric(row.names(prop))
    lpoints(agem, prop[,'female'], pch=4, col='green')
    lpoints(agem, prop[,'male'],   pch=2, col='green')
  } )   # Figure (*\ref{fig:lrm-emp-age-sex-pl}*)
plot(Predict(f, age, sex), ylim=c(-2,4), addpanel=w,
     label.curve=list(offset=unit(0.5, 'cm')))

## @knitr emp-age-sex-plunused
ggplot(Predict(f, age, sex)) +
  stat_plsmo(aes(x=age, y=sigdz), data=acath, fun=qlogis)




## @knitr lrtest
lr <- function(formula)
  {
    f <- lrm(formula, data=acath)
    stats <- f$stats[c('Model L.R.', 'd.f.')]
    cat('L.R. Chi-square:', round(stats[1],1),
        '  d.f.:', stats[2],'\n')
    f
  }
a <- lr(sigdz ~ sex + age)
b <- lr(sigdz ~ sex * age)
c <- lr(sigdz ~ sex + rcs(age,4))
d <- lr(sigdz ~ sex * rcs(age,4))
lrtest(a, b)
lrtest(a, c)
lrtest(a, d)
lrtest(b, d)
lrtest(c, d)


## @knitr dur
dz <- subset(acath, sigdz==1)
dd <- datadist(dz)
f <- lrm(tvdlm ~ rcs(cad.dur, 5), data=dz)
w <- function(...)
  with(dz, {
    plsmo(cad.dur, tvdlm, fun=qlogis, add=TRUE,
          grid=TRUE, lty='dotted')
    x <- cut2(cad.dur, g=15, levels.mean=TRUE)
    prop <- qlogis(tapply(tvdlm, x, mean, na.rm=TRUE))
    xm <- as.numeric(names(prop))
    lpoints(xm, prop, pch=2, col='green')
  } )   # Figure (*\ref{fig:lrm-dur}*)
plot(Predict(f, cad.dur), addpanel=w)


## @knitr problog
f <- lrm(tvdlm ~ log10(cad.dur + 1), data=dz)
w <- function(...)
  with(dz, {
    x <- cut2(cad.dur, m=150, levels.mean=TRUE)
    prop <- tapply(tvdlm, x, mean, na.rm=TRUE)
    xm <- as.numeric(names(prop))
    lpoints(xm, prop, pch=2, col='green')
  } )
#   Figure (*\ref{fig:lrm-problog}*)
plot(Predict(f, cad.dur, fun=plogis), ylab='P',
     ylim=c(.2, .8), addpanel=w)


## @knitr acath
acath <- transform(acath,
                   cholesterol = choleste,
                   age.tertile = cut2(age,g=3),
                   sx = as.integer(acath$sex) - 1)
# sx for loess, need to code as numeric
dd <- datadist(acath); options(datadist='dd')

# First model stratifies age into tertiles to get more
# empirical estimates of age x cholesterol interaction

f <- lrm(sigdz ~ age.tertile*(sex + rcs(cholesterol,4)),
         data=acath)
f
ltx(f)

## @knitr cholxage
print(anova(f), caption='Crudely categorizing age into tertiles',
      size='smaller')
yl <- c(-1,5)
plot(Predict(f, cholesterol, age.tertile),
     adj.subtitle=FALSE, ylim=yl)   # Figure (*\ref{fig:lrm-cholxage}*)


## @knitr iacholxage-loess
# Re-do model with continuous age
f <- loess(sigdz ~ age * (sx + cholesterol), data=acath,
           parametric="sx", drop.square="sx")
ages  <- seq(25,   75, length=40)
chols <- seq(100, 400, length=40)
g <- expand.grid(cholesterol=chols, age=ages, sx=0)
# drop sex dimension of grid since held to 1 value
p <- drop(predict(f, g))
p[p < 0.001] <- 0.001
p[p > 0.999] <- 0.999
zl <- c(-3, 6)   # Figure (*\ref{fig:lrm-iacholxage-loess}*)
wireframe(qlogis(p) ~ cholesterol*age,
          xlab=list(rot=30), ylab=list(rot=-40),
          zlab=list(label='log odds', rot=90), zlim=zl,
          scales = list(arrows = FALSE), data=g)


## @knitr iacholxage-lsp
f <- lrm(sigdz ~ lsp(age,c(46,52,59)) *
         (sex + lsp(cholesterol,c(196,224,259))),
         data=acath)
ltx(f)
print(anova(f), caption='Linear spline surface',
      size='smaller')
perim <- with(acath,
              perimeter(cholesterol, age, xinc=20, n=5))
zl <- c(-2, 4)   # Figure (*\ref{fig:lrm-iacholxage-lsp}*)
bplot(Predict(f, cholesterol, age, np=40), perim=perim,
      lfun=wireframe, zlim=zl, adj.subtitle=FALSE)


## @knitr iacholxage-1
f <- lrm(sigdz ~ rcs(age,4)*(sex + rcs(cholesterol,4)),
         data=acath, tol=1e-11)
ltx(f)
print(anova(f), caption='Cubic spline surface',
      size='smaller')
# Figure (*\ref{fig:lrm-iacholxage-1}*):
bplot(Predict(f, cholesterol, age, np=40), perim=perim,
      lfun=wireframe, zlim=zl, adj.subtitle=FALSE)


## @knitr iacholxage-2
f <- lrm(sigdz ~ sex*rcs(age,4) + rcs(cholesterol,4) +
         rcs(age,4) %ia% rcs(cholesterol,4), data=acath)
print(anova(f), size='smaller',
      caption='Singly nonlinear cubic spline surface')
# Figure (*\ref{fig:lrm-iacholxage-2}*):
bplot(Predict(f, cholesterol, age, np=40), perim=perim,
      lfun=wireframe, zlim=zl, adj.subtitle=FALSE)
ltx(f)


## @knitr iacholxage-3
f <- lrm(sigdz ~ rcs(age,4)*sex + rcs(cholesterol,4) +
         age %ia% cholesterol, data=acath)
print(anova(f), caption='Linear interaction surface',
      size='smaller')
# Figure (*\ref{fig:lrm-iacholxage-3}*):
bplot(Predict(f, cholesterol, age, np=40), perim=perim,
      lfun=wireframe, zlim=zl, adj.subtitle=FALSE)
f.linia <- f  # save linear interaction fit for later
ltx(f)

## @knitr cholxage-model
# Make estimates of cholesterol effects for mean age in
# tertiles corresponding to initial analysis
mean.age <-
  with(acath,
       as.vector(tapply(age, age.tertile, mean, na.rm=TRUE)))
plot(Predict(f, cholesterol, age=round(mean.age,2),
             sex="male"),
     adj.subtitle=FALSE, ylim=yl) #3 curves, Figure (*\ref{fig:lrm-cholxage-model}*)

## @knitr dur-partial-resid
f <- lrm(tvdlm ~ cad.dur, data=dz, x=TRUE, y=TRUE)
resid(f, "partial", pl="loess", xlim=c(0,250), ylim=c(-3,3))
scat1d(dz$cad.dur)
log.cad.dur <- log10(dz$cad.dur + 1)
f <- lrm(tvdlm ~ log.cad.dur, data=dz, x=TRUE, y=TRUE)
resid(f, "partial", pl="loess", ylim=c(-3,3))
scat1d(log.cad.dur)   # Figure (*\ref{fig:lrm-dur-partial-resid}*)


## @knitr asr-influence
f <- update(fasr, x=TRUE, y=TRUE, data=sex.age.response)
## Also try which.influence(f, .4)
round(resid(f, 'dfbetas'), 1)   # Table (*\ref{table:dfbetas}*)


## @knitr sex-age-response-boot
d <- sex.age.response
dd <- datadist(d); options(datadist='dd')
f <- lrm(response ~ sex + age, data=d, x=TRUE, y=TRUE)
set.seed(3)  # for reproducibility
v1  <- validate(f, B=150)

## @knitr sex-age-response-booth
ap1 <- round(v1[,'index.orig'], 2)
bc1 <- round(v1[,'index.corrected'], 2)

## @knitr sex-age-response-bootp
latex(v1,
      caption='Bootstrap Validation, 2 Predictors Without Stepdown',
      insert.bottom='\\label{pg:lrm-sex-age-response-boot}',
      digits=2, size='Ssize', file='')


## @knitr sex-age-response-bootsw
v2 <- validate(f, B=150, bw=TRUE,
               rule='p', sls=.1, type='individual')

## @knitr sex-age-response-bootswh
ap2 <- round(v2[,'index.orig'], 2)
bc2 <- round(v2[,'index.corrected'], 2)

## @knitr sex-age-response-bootswp
latex(v2,
      caption='Bootstrap Validation, 2 Predictors with Stepdown',
      digits=2, B=15, file='', size='Ssize')


## @knitr sex-age-response-bootsw5
set.seed(133)
n  <- nrow(d)
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x4 <- runif(n)
x5 <- runif(n)
f  <- lrm(response ~ age + sex + x1 + x2 + x3 + x4 + x5,
          data=d, x=TRUE, y=TRUE)
v3 <- validate(f, B=150, bw=TRUE,
               rule='p', sls=.1, type='individual')

## @knitr sex-age-response-bootsw5v
k <- attr(v3, 'kept')
# Compute number of x1-x5 selected
nx <- apply(k[,3:7], 1, sum)
# Get selections of age and sex
v <- colnames(k)
as <- apply(k[,1:2], 1,
            function(x) paste(v[1:2][x], collapse=', '))
table(paste(as, ' ', nx, 'Xs'))


## @knitr sex-age-response-bootsw5ph
ap3 <- round(v3[,'index.orig'], 2)
bc3 <- round(v3[,'index.corrected'], 2)

## @knitr sex-age-response-bootsw5p
latex(v3,   # (*\label{pg:lrm-sw5p}*)
 caption='Bootstrap Validation with 5 Noise Variables and Stepdown',
 digits=2, B=15, size='Ssize', file='')


## @knitr sex-age-response-bootsw52
v4 <- validate(f, B=150, bw=TRUE, rule='p', sls=.1,
               type='individual', force=1:2)
ap4 <- round(v4[,'index.orig'], 2)
bc4 <- round(v4[,'index.corrected'], 2)

## @knitr sex-age-response-bootsw52p
latex(v4,
      caption='Bootstrap Validation with 5 Noise Variables and Stepdown, Forced Inclusion of age and sex',
      digits=2, B=15, size='Ssize')


## @knitr calibrations
g <- function(v) v[c('Intercept','Slope'),'index.corrected']
k <- rbind(g(v1), g(v2), g(v3))
co <- c(2,5,4,1)
plot(0, 0, ylim=c(0,1), xlim=c(0,1),
     xlab="Predicted Probability",
     ylab="Estimated Actual Probability", type="n")
legend(.45,.35,c("age, sex", "age, sex stepdown",
                 "age, sex, x1-x5", "ideal"),
       lty=1, col=co, cex=.8, bty="n")
probs <- seq(0, 1, length=200); L <- qlogis(probs)
for(i in 1:3) {
  P <- plogis(k[i,'Intercept'] + k[i,'Slope'] * L)
  lines(probs, P, col=co[i], lwd=1)
}
abline(a=0, b=1, col=co[4], lwd=1)   # Figure (*\ref{fig:lrm-calibrations}*)


## @knitr cholxage-confbar
s <- summary(f.linia)
print(s, size='Ssize')
plot(s)   # Figure (*\ref{fig:lrm-cholxage-confbar}*)

## @knitr iacholxage-3-nomogram
# Draw a nomogram that shows examples of confidence intervals
nom <- nomogram(f.linia, cholesterol=seq(150, 400, by=50),
                interact=list(age=seq(30, 70, by=10)),
                lp.at=seq(-2, 3.5, by=.5),
                conf.int=TRUE, conf.lp="all",
                fun=function(x)1/(1+exp(-x)),  # or plogis
                funlabel="Probability of CAD",
                fun.at=c(seq(.1, .9, by=.1), .95, .99)
                )   # Figure (*\ref{fig:lrm-iacholxage-3-nomogram}*)
plot(nom, col.grid = gray(c(0.8, 0.95)),
     varname.label=FALSE, ia.space=1, xfrac=.46, lmgp=.2)


## @knitr val-prob
set.seed(13)
n <- 200
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
logit <- 2*(x1-.5)
P <- 1/(1+exp(-logit))
y <- ifelse(runif(n) <= P, 1, 0)
d <- data.frame(x1, x2, x3, y)
f <- lrm(y ~ x1 + x2 + x3, subset=1:100)
phat <- predict(f, d[101:200,], type='fitted')
# Figure (*\ref{fig:lrm-val-prob}*)
v <- val.prob(phat, y[101:200], m=20, cex=.5)


## Bayesian analysis

## @knitr bayes-bin_mods
dd <- datadist(sex.age.response)
options(datadist = 'dd')
require(brms)

# Frequentist model
fit_lrm <- lrm(response ~ sex + age, data=sex.age.response)

# Bayesian model
# Distribute chains across cpu cores:
options(mc.cores=parallel::detectCores())

# Set priors
# Solve for SD such that sex effect has only a 0.025 chance of
# being above 5 (or being below -5)

s1 <- 5 / qnorm(0.975)

# Solve for SD such that 10-year age effect has only 0.025 chance
# of being above 20

s2 <- (20 / qnorm(0.975)) / 10   # divide by 10 since ratio on 10b scale

stanvars <- stanvar(s1, name='s1') + stanvar(s2, name='s2')

prs <- c(prior(student_t(3,0,10), class='Intercept'),
         prior(normal(0, s1), class='b', coef='sexmale'),
         prior(normal(0, s2), class='b', coef='age'))

# Full model
fit_brms <- brm(response ~ sex + age, data=sex.age.response,
                family=bernoulli("logit"), prior=prs, iter=5000,
                stanvars=stanvars)


## @knitr bayes-bin_fits
# Frequentist model output
fit_lrm

summary(fit_lrm, age=20:21)


## @knitr bayes-bin_fits2
# Bayesian model output
post_samps <- posterior_samples(fit_brms, c("age","sex")) 

fit_brms
posterior_interval(fit_brms, c("age","sex"))
prior_summary(fit_brms)


## @knitr bayes-post_plt
# display posterior densities for age and sex parameters
plot(fit_brms, c("age","sex"), combo=c("dens","trace","hex"))


## @knitr bayes-margeff
# Marginal effects plot
plot(conditional_effects(fit_brms, "age:sex"))


## @knitr bayes-biv
# Frequentist
# variance-covariance for sex and age parameters
sex_age_vcov <- vcov(fit_lrm)[2:3,2:3]

# Sampling based parameter estimate correlation coefficient
f_cc <- sex_age_vcov[1,2] / (sqrt(sex_age_vcov[1,1]) * sqrt(sex_age_vcov[2,2]))

# Bayesian
# Linear correlation between params from posterior 
b_cc <- cor(post_samps)[1,2]


## @knitr bayes-postprobs
# Define P() as mean() just to provide a nice notation for
# computing posterior probabilities
P <- function(x) mean(x)
b1 <- post_samps[, 'b_sexmale']
b2 <- post_samps[, 'b_age']
(p1 <- P(b1 > 0))   # post prob(sex has positive association with Y)
(p2 <- P(b2 > 0))
(p3 <- P(b1 > 0 & b2 > 0))
(p4 <- P(b1 > 0 | b2 > 0))


## @knitr bayes-bidens
ggplot(post_samps, aes(x=b_sexmale, y = b_age)) + 
  geom_hex() + 
  theme(legend.position="none")


## @knitr bayes-MAP
# Calculate MAP interval
# Code from http://www.sumsar.net/blog/2014/11/how-to-summarize-a-2d-posterior-using-a-highest-density-ellipse/
samples <- as.matrix(post_samps)
coverage = 0.95
fit <- MASS::cov.mve(samples, quantile.used = round(nrow(samples) * coverage))
points_in_ellipse <- samples[fit$best,]
ellipse_boundary <- predict(cluster::ellipsoidhull(points_in_ellipse))
map <- data.frame(ellipse_boundary)
names(map) <- c("y","x")

ggplot(post_samps, aes(x=b_sexmale, y = b_age)) + 
  geom_hex() + 
  geom_polygon(data = map, aes(x=x,y=y), color = "grey", alpha = 0) +
  geom_point(aes(x = fixef(fit_brms)[,1][2], y = fixef(fit_brms)[,1][3]), color = "grey") + 
  theme(legend.position="none")


## @knitr bayes-ellipse
# Function takes in variance-covariance matrix (D), point estimates (d),
# and a level of significance (alpha)
EllipseDF <- function(D, d, alpha = 0.05) {
  delta = sqrt(eigen(D)$values)
  # Root eigenvalues correspond to the half-lengths of the ellipse 
  V = eigen(D)$vectors
  # Eigenvectors give the axes of the confidence ellipse
  R = sqrt(qchisq(1-alpha, df = length(delta)))
  # Scaling factor to get to 0.95 confidence
  a = R*delta[1] # scale the ellipse axes
  b = R*delta[2]
  t <- seq(0, 2*pi, length.out=200)
  # Generate radian measures from 0 to 2pi
  points.proj = V %*% t(cbind(a * cos(t), b * sin(t)))
  # Transform circle into ellipse
  return(data.frame(x = (points.proj)[1, ] + d[1],
                    y = (points.proj)[2, ] + d[2]))
}

D <- vcov(fit_lrm)[-1,-1]
beta <- coef(fit_lrm)[-1]
ci_ellipse <- EllipseDF(D, beta, alpha = 0.05)

ggplot(post_samps, aes(x=b_sexmale, y = b_age)) + 
  geom_hex() + 
  geom_polygon(data = map, aes(x=x,y=y), color = "grey", alpha = 0) +
  geom_polygon(data = ci_ellipse, aes(x = x,y = y), color = "red", alpha = 0) +
  geom_point(aes(x = fixef(fit_brms)[,1][2], y = fixef(fit_brms)[,1][3]), color = "grey") + 
  geom_point(aes(x = coef(fit_lrm)[2], y = coef(fit_lrm)[3]), color = "red") + 
  theme(legend.position="none")
