## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('val')


## ----cdf-ecdf,echo=FALSE,cap='Empirical and population cumulative distribution function'----
set.seed(6)
x <- rnorm(30, 100, 20)
xs <- seq(50, 150, length=150)
cdf <- pnorm(xs, 100, 20)
plot(xs, cdf, type='l', ylim=c(0,1),
     xlab=expression(x),
     ylab=expression(paste("Prob[", X <= x, "]")))
lines(ecdf(x), cex=.5)


## ----boot,echo=FALSE,h=3,w=6,ps=9,mfrow=c(1,2),cap='Estimating properties of sample median using the bootstrap'----
options(digits=3)
y <- c(2,5,6,7,8,9,10,11,12,13,14,19,20,21)
y <- c(1,5,6,7,8,9)
set.seed(17)
n <- length(y)
n2 <- n/2
n21 <- n2+1
B <- 400
M <- double(B)
plot(0, 0, xlim=c(0,B), ylim=c(3,9),
     xlab="Bootstrap Samples Used", 
     ylab="Mean and 0.1, 0.9 Quantiles", type="n")
for(i in 1:B) {
  s <- sample(1:n, n, replace=T)
  x <- sort(y[s])
  m <- .5*(x[n2]+x[n21])
  M[i] <- m
  if(i <= 20) {
    w <- as.character(x)
    cat(w, "& &", sprintf('%.1f',m), 
        if(i < 20) "\\\\\n" else "\\\\ \\hline\n",
        file='~/doc/rms/validate/tab.tex', append=i > 1)
  }
  points(i, mean(M[1:i]), pch=46)
  if(i>=10)	{
    q <- quantile(M[1:i], c(.1,.9))
    points(i, q[1], pch=46, col='blue')
    points(i, q[2], pch=46, col='blue')
  }
}
hist(M, nclass=length(unique(M)), xlab="", main="")


## ----bootrank,cache=TRUE,cap='Bootstrap percentile 0.95 confidence limits for ranks of predictors in an OLS model.  Ranking is on the basis of partial $\\chi^2$ minus d.f.  Point estimates are original ranks',scap='Bootstrap confidence limits for ranks of predictors'----
# Use the plot method for anova, with pl=FALSE to suppress
# actual plotting of chi-square - d.f. for each bootstrap
# repetition.  Rank the negative of the adjusted chi-squares
# so that a rank of 1 is assigned to the highest.  It is
# important to tell plot.anova.rms not to sort the results,
# or every bootstrap replication would have ranks of 1,2,3,
# ... for the partial test statistics.
require(rms)
n <- 300
set.seed(1)
d <- data.frame(x1=runif(n), x2=runif(n),  x3=runif(n),
   x4=runif(n), x5=runif(n), x6=runif(n),  x7=runif(n),
   x8=runif(n), x9=runif(n), x10=runif(n), x11=runif(n),
   x12=runif(n))
d$y <- with(d, 1*x1 + 2*x2 + 3*x3 +  4*x4  + 5*x5 + 6*x6 +
               7*x7 + 8*x8 + 9*x9 + 10*x10 + 11*x11 +
              12*x12 + 9*rnorm(n))

f <- ols(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12, data=d)
B <- 1000
ranks <- matrix(NA, nrow=B, ncol=12)
rankvars <- function(fit)
  rank(plot(anova(fit), sort='none', pl=FALSE))
Rank <- rankvars(f)
for(i in 1:B) {
  j <- sample(1:n, n, TRUE)
  bootfit <- update(f, data=d, subset=j)
  ranks[i,] <- rankvars(bootfit)
  }
lim <- t(apply(ranks, 2, quantile, probs=c(.025,.975)))
predictor <- factor(names(Rank), names(Rank))
w <- data.frame(predictor, Rank, lower=lim[,1], upper=lim[,2])
require(ggplot2)
ggplot(w, aes(x=predictor, y=Rank)) + geom_point() +
  coord_flip() + scale_y_continuous(breaks=1:12) +
  geom_errorbar(aes(ymin=lim[,1], ymax=lim[,2]), width=0)


## ----eval=FALSE---------------------------------------------------------------
## require(rms)
## set.seed(1)  # so can reproduce results
## 
## n    <- 200	        # Size of training sample
## reps <- 200	        # Simulations
## npop <- 50000       # Size of validation gold standard sample
## methods <- c('Boot 40','Boot 200','632a 40','632a 200',
##              '632b 40','632b 200','10-fold x 4','4-fold x 10',
##              '10-fold x 20','4-fold x 50')
## R <- expand.grid(sim    = 1:reps,
##                  p      = c(15,30),
##                  method = methods)
## R$Dxy <- R$Intercept <- R$Slope <- R$D <- R$U <- R$Q <-
##   R$repmeth <- R$B <- NA
## R$n <- n
## 
## ## Function to do r overall reps of B resamples, averaging to
## ## get estimates similar to as if r*B resamples were done
## 
## val <- function(fit, method, B, r) {
##   contains <- function(m) length(grep(m, method)) > 0
##   meth <- if(contains('Boot')) 'boot' else
##           if(contains('fold')) 'crossvalidation' else
##           if(contains('632')) '.632'
##   z <- 0
##   for(i in 1:r) z <- z + validate(fit, method=meth, B=B)[
##           c("Dxy","Intercept","Slope","D","U","Q"),
##           'index.corrected']
##   z/r
## }


## ----eval=FALSE---------------------------------------------------------------
## for(p in c(15, 30)) {
## 
##   ## For each p create the true betas, the design matrix,
##   ## and realizations of binary y in the gold standard
##   ## large sample
##   Beta <- rep(.5, p)  # True betas
##   X    <- matrix(runif(npop*p), nrow=npop) - 0.5
##   LX   <- matxv(X, Beta)
##   Y    <- ifelse(runif(npop) <= plogis(LX), 1, 0)
## 
##   ## For each simulation create the data matrix and
##   ## realizations of y
##   for(j in 1:reps) {
## 
##     ## Make training sample
##     x <- matrix(runif(n*p), nrow=n) - 0.5
##     L <- matxv(x, Beta)
##     y <- ifelse(runif(n) <= plogis(L), 1, 0)
##     f <- lrm(y ~ x, x=TRUE, y=TRUE)
##     beta <- f$coef
##     forecast <- matxv(X, beta)
##     ## Validate in population
##     v <- val.prob(logit=forecast, y=Y, pl=FALSE)[
##                     c("Dxy","Intercept","Slope","D","U","Q")]
## 
##     for(method in methods) {
##       repmeth <- 1
##       if(method %in% c('Boot 40','632a 40','632b 40'))
##         B <- 40
##       if(method %in% c('Boot 200','632a 200','632b 200'))
##         B <- 200
##       if(method == '10-fold x 4') {
##         B <- 10
##         repmeth <- 4
##       }
##       if(method == '4-fold x 10') {
##         B <- 4
##         repmeth <- 10
##       }
##       if(method == '10-fold x 20') {
##         B <- 10
##         repmeth <- 20
##       }
##       if(method == '4-fold x 50') {
##         B <- 4
##         repmeth <- 50
##       }
## 
##       z <-  val(f, method, B, repmeth)
##       k <- which(R$sim == j & R$p == p & R$method == method)
##       if(length(k) != 1) stop('program logic error')
##       R[k, names(z)] <- z - v
##       R[k, c('B','repmeth')] <- c(B=B, repmeth=repmeth)
##     } # end over methods
##   } # end over reps
## } # end over p


## ----eval=FALSE---------------------------------------------------------------
## statnames <- names(R)[6:11]
## w <- reshape(R, direction='long', varying=list(statnames),
##              v.names='x', timevar='stat', times=statnames)
## w$p <- paste('p', w$p, sep='=')
## require(lattice)
## s <- with(w, summarize(abs(x), llist(p, method, stat),
##                        smean.cl.boot,stat.name='mae'))
## Dotplot(method ~ Cbind(mae, Lower, Upper) | stat*p, data=s,
##         xlab='Mean |error|')
## s <- with(w, summarize(x^2, llist(p, method, stat),
##                        smean.cl.boot, stat.name='mse'))
## Dotplot(method ~ Cbind(sqrt(mse), sqrt(Lower), sqrt(Upper)) |
##         stat*p, data=s,
##         xlab=expression(sqrt(MSE)))

