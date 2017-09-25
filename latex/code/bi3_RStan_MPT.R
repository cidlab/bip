# set working directory
setwd("/Users/udobohm/Paper Bayesian parameter estimation")
# load R2Stan package
library(rstan)

# create vector that contains the observed number of responses in each category for each condition
cons <- c(78,70,7,15)
inco <- c(102,55,40,53)
neut <- c(63,45,13,21)

# create a vector that contains the total number of responses in each condition
N <- c(170,250,142)

# create a named list that contains the data to be passed to Stan
mydata <- list(cons=cons,inco=inco,neut=neut)

# define a function that generates the initial values for the unobserved stochastic nodes
myinits <- function(){list(p=runif(1,0,1), q=runif(1,0,1), c=runif(1,0,1))}

# specify parameters of interest
myparameters <- c("p","q","c")


# call Stan
samples <- stan(data = mydata, init = myinits, pars = myparameters, file = 'code/noconflict.stan', chains = 3, iter = 3500, warmup = 500, thin = 5)

# plot density of posterior samples
library(polspline)
x <- seq(0,1,.0001)
samps <- extract(samples, pars=myparameters)
pfit <- logspline(samps$p)
qfit <- logspline(samps$q)
cfit <- logspline(samps$c)

par(mar=c(5,5,4,0.5))
plot(pfit, type="l", lty=1, lwd=2, bty="n", xlab="MTP parameters", ylab="Density", cex.lab=1.5,main="Posterior Distributions", cex.main=1.5, xlim=c(0,1))
plot(qfit, lty=2, lwd=2, add=T)
plot(cfit, lty=3, lwd=2, add=T)
#lines(c(0,1),c(1,1),col="#AAAAAA", lwd=2)
legend(x=.7,y=12, c("p","q","c"), lty=c(1,2,3), lwd=2, cex=1.5)

# display the first 15 samples from the first chain for p
extract(samples, "p")$p[1:15]
# plot a histogram of the posterior distribution of p
hist(extract(samples, "p")$p)
# display summary statistics of the posterior distributions
print(samples)
# plot traceplot
traceplot(samples)


# do the Dora plot
postscript("mpt_posteriors_stan.eps",paper = "special" ,width=7, height=7,horizontal=F)
par(cex.lab=1.5,cex.axis=1.2,las=1,cex.main=1.6,mar = c(5,6, 4, 2) + 0.1)
plot(density(samps$p),xlim=c(0,1),main="Posterior distributions \nfrom Stan",ylim=c(0,13),bty="n",lwd=3,
                ylab="Density",xlab="MPT parameters")
lines(density(samps$q),lwd=3,lty=2)
lines(density(samps$c),lwd=3,lty=3)
dev.off()