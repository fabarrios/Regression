## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('prostate', width=80)


## ----results='asis'-----------------------------------------------------------
require(Hmisc)
getHdata(prostate)  # Download and make prostate accessible
# Convert an old date format to R format
prostate$sdate <- as.Date(prostate$sdate)
d <- describe(prostate[2:17])
latex(d, file='')


## -----------------------------------------------------------------------------
# Allow only 1 d.f. for three of the predictors
prostate <-
  transform(prostate,
            ekg.norm = 1*(ekg %in% c("normal","benign")),
            rxn = as.numeric(rx),
            pfn = as.numeric(pf))
# Force pfn, rxn to be linear because of difficulty of placing
# knots with so many ties in the data
# Note: all incomplete cases are deleted (inefficient)
redun(~ stage + I(rxn) + age + wt + I(pfn) + hx +
      sbp + dbp + ekg.norm + hg + sz + sg + ap + bm,
      r2=.3, type='adjusted', data=prostate)


## -----------------------------------------------------------------------------
x <- with(prostate,
          cbind(stage, rx, age, wt, pf, hx, sbp, dbp,
                ekg.norm, hg, sz, sg, ap, bm))
# If no missing data, could use cor(apply(x, 2, rank))
r <- rcorr(x, type="spearman")$r    # rcorr in Hmisc
maxabsr <- max(abs(r[row(r) != col(r)]))

## ----spearman,h=3,w=3,bot=-1,top=-.5,right=-.5,xpd=NA,cap=paste('Matrix of Spearman $\\rho$ rank correlation coefficients between predictors.  Horizontal gray scale lines correspond to $\\rho=0$.  The tallest bar corresponds to $|\\rho|=',round(maxabsr,2),'$.',sep=''),scap='Spearman $\\rho$ rank correlations of predictors'----
p <- nrow(r)
plot(c(-.35,p+.5),c(.5,p+.25), type='n', axes=FALSE,
     xlab='',ylab='')   # Figure (*\ref{fig:prostate-spearman}*)
v <- dimnames(r)[[1]]
text(rep(.5,p), 1:p, v, adj=1)
for(i in 1:(p-1)) {
  for(j in (i+1):p) {
    lines(c(i,i),c(j,j+r[i,j]/maxabsr/2),
          lwd=3, lend='butt')
    lines(c(i-.2,i+.2),c(j,j), lwd=1, col=gray(.7))
  }
  text(i, i, v[i], srt=-45, adj=0)
}


## ----hclust,h=2.5,w=4,cap="Hierarchical clustering using Hoeffding's $D$ as a similarity measure.  Dummy variables were used for the categorical variable \\co{ekg}.  Some of the dummy variables cluster together since they are by definition negatively correlated.",scap="'Hierarchical clustering"----
vc <- varclus(~ stage + rxn + age + wt + pfn + hx +
              sbp + dbp + ekg.norm + hg + sz + sg + ap + bm,
              sim='hoeffding', data=prostate)
plot(vc)   # Figure (*\ref{fig:prostate-hclust}\label{pg:ex.varclus.D}*)


## ----transcan,w=6.5,h=5,bot=1,cap='Simultaneous transformation and single imputation of all candidate predictors using \\fu{transcan}.   Imputed values are shown as red plus signs.  Transformed values are arbitrarily scaled to $[0,1]$.',scap='Simultaneous transformation and imputation using \\fu{transcan}'----
# Combine 2 levels of ekg (one had freq. 1)
levels(prostate$ekg)[levels(prostate$ekg) %in%
                     c('old MI', 'recent MI')] <- 'MI'

prostate$pf.coded <- as.integer(prostate$pf)
# make a numeric version; combine last 2 levels of original
levels(prostate$pf) <- levels(prostate$pf)[c(1,2,3,3)]

ptrans <-
  transcan(~ sz + sg + ap + sbp + dbp + 
           age + wt + hg + ekg + pf + bm + hx, imputed=TRUE,
           transformed=TRUE, trantab=TRUE, pl=FALSE,
           show.na=TRUE, data=prostate, frac=.1, pr=FALSE)
summary(ptrans, digits=4)
ggplot(ptrans, scale=TRUE) +
  theme(axis.text.x=element_text(size=6))   # Figure (*\ref{fig:prostate-transcan}*)


## ----pc,top=1,w=4,h=3,cap='Variance of the system of raw predictors (black) explained by individual principal components (lines) along with cumulative proportion of variance explained (text), and variance explained by components computed on \\fu{transcan}-transformed variables (red)',scap='Variance of the system explained by principal components.'----
# Impute all missing values in all variables given to transcan
imputed <- impute(ptrans, data=prostate, list.out=TRUE)
imputed <- as.data.frame(imputed)

# Compute principal components on imputed data.
# Create a design matrix from ekg categories
Ekg <- model.matrix(~ ekg, data=imputed)[, -1]
# Use correlation matrix
pfn <- prostate$pfn
prin.raw <- princomp(~ sz + sg + ap + sbp + dbp + age +
                     wt + hg + Ekg + pfn + bm + hx,
                     cor=TRUE, data=imputed)

plot(prin.raw, type='lines', main='', ylim=c(0,3))#Figure (*\ref{fig:prostate-pc}*)
# Add cumulative fraction of variance explained
addscree <- function(x, npcs=min(10, length(x$sdev)),
                     plotv=FALSE,
                     col=1, offset=.8, adj=0, pr=FALSE) {
  vars <- x$sdev^2
  cumv <- cumsum(vars)/sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset*par('cxy')[2],
       as.character(round(cumv[1:npcs], 2)),
       srt=45, adj=adj, cex=.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type='b', col=col)
}
addscree(prin.raw)
prin.trans <- princomp(ptrans$transformed, cor=TRUE)
addscree(prin.trans, npcs=10, plotv=TRUE, col='red',
         offset=-.8, adj=1)


## ----aic,cap='AIC of Cox models fitted with progressively more principal components.  The solid blue line depicts the AIC of the model with all original covariates.  The dotted blue line is positioned at the AIC of the full spline model.',scap='AIC vs.\ number of principal components',bty="l"----
require(rms)
S <- with(prostate, Surv(dtime, status != "alive"))
# two-column response var.

pcs <- prin.raw$scores         # pick off all PCs
aic <- numeric(16)
for(i in 1:16) {
  ps <- pcs[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}   # Figure (*\ref{fig:prostate-aic}*)
plot(1:16, aic, xlab='Number of Components Used',
     ylab='AIC', type='l', ylim=c(3950,4000))
f <- cph(S ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + 
         ekg + pf + bm + hx, data=imputed)
abline(h=AIC(f), col='blue')
## The following model in the 2nd edition no longer converges
# f <- cph(S ~ rcs(sz,5) + rcs(sg,5) + rcs(log(ap),5) +
#          rcs(sbp,5) + rcs(dbp,5) + rcs(age,3) + rcs(wt,5) +
#          rcs(hg,5) + ekg + pf + bm + hx,
#          tol=1e-14, data=imputed)
f <- cph(S ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),5) +
         rcs(sbp,4) + rcs(dbp,4) + rcs(age,3) + rcs(wt,4) +
         rcs(hg,4) + ekg + pf + bm + hx,
         tol=1e-14, data=imputed)
abline(h=AIC(f), col='blue', lty=2)


## -----------------------------------------------------------------------------
# Compute PC1 on a subset of transcan-transformed predictors
pco <- function(v) {
  f <- princomp(ptrans$transformed[,v], cor=TRUE)
  vars <- f$sdev^2
  cat('Fraction of variance explained by PC1:',
      round(vars[1]/sum(vars),2), '\n')
  f$scores[,1]
}
tumor   <- pco(c('sz','sg','ap','bm'))
bp      <- pco(c('sbp','dbp'))
cardiac <- pco(c('hx','ekg'))
# Get transformed individual variables that are not clustered
other   <- ptrans$transformed[,c('hg','age','pf','wt')]
f <- cph(S ~ tumor + bp + cardiac + other)  # other is matrix
AIC(f)

## ----results='asis'-----------------------------------------------------------
print(f, latex=TRUE, long=FALSE, title='')


## ----spca,cap='Variance explained by individual sparse principal components (lines) along with cumulative proportion of variance explained (text)',scap='Sparse principal components'----
require(pcaPP)
s <- sPCAgrid(ptrans$transformed, k=10, method='sd',
              center=mean, scale=sd, scores=TRUE,
              maxiter=10)
plot(s, type='lines', main='', ylim=c(0,3))   # Figure (*\ref{fig:prostate-spca}*)
addscree(s)
lo <- unclass(s$loadings)
flo <- format(round(lo, 2), zero.print=FALSE)
print(flo, quote=FALSE)    # These loadings are on the orig. transcan scale


## ----spc,cap='Performance of sparse principal components in Cox models',scap='Performance of sparse principal components',bty='l'----
pcs <- s$scores         # pick off sparse PCs
aic <- numeric(10)
for(i in 1:10) {
  ps <- pcs[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}   # Figure (*\ref{fig:prostate-spc}*)
plot(1:10, aic, xlab='Number of Components Used',
     ylab='AIC', type='l',  ylim=c(3950,4000))


## ----ace,h=4.5,w=6,mfrow=c(4,3),left=-2,cap='Simultaneous transformation of all variables using ACE.',scap='Transformation of variables using ACE'----
x <- with(imputed,
          cbind(sz, sg, ap, sbp, dbp, age, wt, hg, ekg, pf,
                bm, hx))
monotonic <- c("sz","sg","ap","sbp","dbp","age","pf")
transace(x, monotonic,   # Figure (*\ref{fig:prostate-ace}*)
         categorical="ekg", binary=c("bm","hx"))

