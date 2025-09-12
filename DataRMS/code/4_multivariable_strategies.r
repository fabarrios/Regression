## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('multivar')


## ----eval=FALSE---------------------------------------------------------------
## f <- lrm(y ~ sex + race + rcs(age,5) + rcs(weight,5) +
##          rcs(height,5) + rcs(blood.pressure,5))
## plot(anova(f))


## ----echo=FALSE---------------------------------------------------------------
n    <- 20:80
low  <- sqrt((n - 1) / qchisq(.975, n - 1))
hi   <- sqrt((n - 1) / qchisq(.025, n - 1))
m    <- pmax(1/low,hi)
nmin <- min(n[m <= 1.2])


## ----r2adj,echo=FALSE,w=6,h=3.5,ps=9,mfrow=c(1,2),cap='Multiple of $p$ that $n$ must be to achieve a relative drop from $R^{2}$ to $R^{2}_{\\rm adj}$ by the indicated relative factor (left panel, 3 factors) or absolute difference (right panel, 6 decrements)',scap='Comparison of $R^{2}_{\\rm adj}$ to $R^2$'----
require(Hmisc)
# Given R2 solve for multiplier of p that n-1 must be so that adj R2 is
# only a factor of k lower than R2 or is lower by an absolute k
pmultols <- function(R2, k, type=c('relative', 'absolute')) {
  type <- match.arg(type)
  if(type == 'relative')
    ((1/R2) - k) / (1 - k)
    else (1 - R2 + k) /  k
}
r2adj <- function(R2, n, p) 1 - (1 - R2)*(n - 1) / (n - p - 1)
dop <- function(k, type) {
  z <- list()
  R2 <- seq(.01, .99, by=.01)
  for(a in k) z[[as.character(a)]] <-
    list(R2=R2, pfact=if(type=='relative') ((1 / R2) - a) / (1 - a) else
         (1 - R2 + a) /  a)
  labcurve(z, pl=TRUE, ylim=c(0,100), adj=0, offset=3,
           xlab=expression(R^2), ylab=expression(paste('Multiple of ',p)))
}
dop(c(.9, .95, .975), 'relative')
dop(c(.075, .05, .04, .025, .02, .01), 'absolute')


## ----shrink-groupmeans2,child='shrink-groupmeans.Rnw'-------------------------

## ----shrink-groupmeans,ps=7,cap='Sorted means from 20 samples of size 50 from a uniform $[0,1]$ distribution.  The reference line at 0.5 depicts the true population value of all of the means.',scap='Means from 20 $U(0,1)$ samples'----
set.seed(123)
n <- 50
y <- runif(20*n)
group <- rep(1:20,each=n)
ybar <- tapply(y, group, mean)
ybar <- sort(ybar)
plot(1:20, ybar, type='n', axes=FALSE, ylim=c(.3,.7),
     xlab='Group', ylab='Group Mean')
lines(1:20, ybar)
points(1:20, ybar, pch=20, cex=.5)
axis(2)
axis(1, at=1:20, labels=FALSE)
for(j in 1:20) axis(1, at=j, labels=names(ybar)[j])
abline(h=.5, col=gray(.85))



## ----physiol-transcan3,child='physiol-transcan.Rnw'---------------------------

## ----physiol-transcan,h=3,w=6,ps=9,mfrow=c(1,2),cap='Transformations fitted using \\co{transcan}.  Tick marks indicate the two imputed values for blood pressure.',scap='\\protect\\co{transcan} transformations for two physiologic variables'----
require(Hmisc)
getHdata(support)   # Get data frame from web site
heart.rate     <- support$hrt
blood.pressure <- support$meanbp
blood.pressure[400:401]
blood.pressure[400:401] <- NA  # Create two missings
d <- data.frame(heart.rate, blood.pressure)
par(pch=46)   # Figure (*\ref{fig:multivar-physiol-transcan}*)
w <- transcan(~ heart.rate + blood.pressure, transformed=TRUE,
              imputed=TRUE, show.na=TRUE, data=d)
w$imputed$blood.pressure
t <- w$transformed
spe <- round(c(spearman(heart.rate, blood.pressure),
               spearman(t[,'heart.rate'],
                        t[,'blood.pressure'])), 2)

## ----physiol-transcan2,h=3,w=6,ps=9,mfrow=c(1,2),cap=paste('The lower left plot contains raw data (Spearman $\\rho=',spe[1],'$); the lower right is a scatterplot of the corresponding transformed values ($\\rho=',spe[2],'$).  Data courtesy of the SUPPORT study~\\cite{kna95sup}.'),scap='Scatterplot before and after \\co{transcan} transformations'----
plot(heart.rate, blood.pressure)   # Figure (*\ref{fig:multivar-physiol-transcan2}*)
plot(t[,'heart.rate'], t[,'blood.pressure'],
     xlab='Transformed hr', ylab='Transformed bp')



require(Hmisc)
getHdata(support)   # Get data frame from web site
heart.rate     <- support$hrt
blood.pressure <- support$meanbp
blood.pressure[400:401]
blood.pressure[400:401] <- NA  # Create two missings
d <- data.frame(heart.rate, blood.pressure)
par(pch=46)
w <- transcan(~ heart.rate + blood.pressure, transformed=TRUE,
              imputed=TRUE, show.na=TRUE, data=d)
w$imputed$blood.pressure
plot(heart.rate, blood.pressure)
t <- w$transformed
plot(t[,'heart.rate'], t[,'blood.pressure'],
     xlab='Transformed hr', ylab='Transformed bp')
spe <- round(c(spearman(heart.rate, blood.pressure),
               spearman(t[,'heart.rate'],
                        t[,'blood.pressure'])), 2)


