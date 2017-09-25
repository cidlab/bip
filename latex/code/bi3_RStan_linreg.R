# set working directory
setwd("/Users/udobohm/Paper Bayesian parameter estimation")
# load R2Stan package
library(rstan)

# create vector that contains the precicted nubmer of attendees
x<-c(51,44,57,41,53,56,49,58,50,32,
24,21,23,28,22,30,29,35,18,25,
32,42,27,38,32,21,21,12,29,14)
# create a  vector that contains the observed number of attendees
y<-c(33,55,49,56,58,61,46,82,53,33,
35,18,14,31,13,23,15,20,20,33,
32,31,37,17,11,24,17,5,16,7)
# create a scalar that contains the number of sessions
N <- 30
# create a named list that contains the data to be passed to Stan
mydata <- list(x=x, y=y, N=N)
# define a function that generates the initial values for the unobserved stochastic nodes
myinits <- function(){list(beta=rnorm(2,0,10), sigma=runif(1,0,5))}
# specify parameters of interest
myparameters <- c("beta", "tau")
# specify stan model as a string vector
linreg_model <- "data{
	int<lower=1>	N;
	vector[N]		x;
	vector[N]		y;
}
parameters{
	vector[2]		beta;
	real<lower=0>	sigma2;
}
transformed parameters{
	real<lower=0>	tau;
	real<lower=0>	sigma;
	tau <- pow(sigma2, -1);
	sigma <- pow(sigma2, 0.5);
}
model{
	# prior definitions
	beta[1] ~ normal(0,sqrt(1000));
	beta[2] ~ normal(0,sqrt(1000));
	//note the different prior for the standard deviation
	sigma2 ~ inv_gamma(0.001,0.001);
	# linear regression
	for(i in 1:N){
		y[i] ~ normal(beta[1] + beta[2]*x[i], sigma);
	}
}"

# call Stan
samples <- stan(data = mydata, init = myinits, pars = myparameters, model_code = linreg_model, chains = 3, iter = 1000, warmup = 500, thin = 1)
# display the first 15 samples from the first chain for tau2
extract(samples, pars="tau", inc_warmup=F)$tau2[1:15]
# plot a histogram of the posterior distribution of tau2
hist(extract(samples, pars="tau")$tau)
# display summary statistics of the posterior distributions
print(samples)
# plot traceplot
traceplot(samples)
