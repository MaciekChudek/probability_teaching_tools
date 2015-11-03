
current_sample = c()
means = c()
sample_generator_functions = list()


reset = function(){
	current_sample <<- c()
	means <<- c()
	sample_generator_functions <<- list()
}

add_distro = function(z){	
	sample_generator_functions <<- append(sample_generator_functions, z)
	
	par(mfrow=c(2,1))
	
	x = z()
	xs = seq(min(x),max(x),length.out=100)
	hist(x, freq=F, col='lightblue', main='Distribution of data just added')
	lines(density(x), col='blue', lwd=2)
	
	current_sample <<- unlist(sapply(sample_generator_functions, function(x) x()))
	means <<- mean(current_sample)
	plot_current_sample()
}

add_one_sample = function(){
	current_sample <<- unlist(sapply(sample_generator_functions, function(x) x()))
	means <<- c(means, mean(current_sample))
	par(mfrow=c(1,1))
	plot_current_sample()
}

add_samples = function(n){
	new_means = sapply(1:n, function(i) mean(unlist(sapply(sample_generator_functions, function(x) x()))))
	means <<- c(means, new_means)
	par(mfrow=c(1,1))
	plot_current_sample()
}

plot_current_sample = function(){
	hist(current_sample, freq=F, col='lightgreen', main='Distribution of all added data, and its mean')
	if(length(means) > 1) abline(v=means, col='red',lty=3);
	abline(v=head(means,1), col='blue',lty=3, lwd=2)
}

plot_distro_of_means = function(){
	hist(means, freq=F, col='lightgreen')
	xs = seq(min(means),max(means), length.out=100)
	lines(density(means), col='blue', lwd=3)
	lines(xs, dnorm(xs,mean(means), sd(means)), col='red', lwd=2, lty=3)
}

reset()


		#examples
	#Bernouli
#add_distro( function() m * rbinom(n, 1,p) + k )
add_distro( function() 10 * rbinom(1000, 1,.2) + 12 )

	#Beta
#add_distro( function() m * rbeta(n, alpha, beta) + k )
add_distro( function() -4.2 * rbeta(427, 2, .1) -17 )

	#Binomial
#add_distro( function() m * rbeta(n, N, p) + k )

	#Chi-sq
#add_distro( function() m * rchisq(n, k) + k )

	#Exponential
#add_distro( function() m * rexp(n, lambda) + k )

	#F
#add_distro( function() m * rf(n, d1, d2) + k )
add_distro( function() 8.72 * rf(326, 3, 87) + 9 )

	#Poisson
#add_distro( function() m * rpois(n, lambda) + k )
add_distro( function() -12 * rpois(644, .2) -177 )

	#t
#add_distro( function() m * rexp(n, nu) + k )

	#Uniform
#add_distro( function() m * runif(n, a, b) + k )



#Once you've finished adding distributions, sample means from the data you generated:

add_one_sample()
add_one_sample()
add_one_sample()
add_samples(1000)

#The plot the distribution of those means:

plot_distro_of_means()

#try adding more even samples to see the distro of means approach the limiting distribution: the normal

add_samples(10000)
plot_distro_of_means()
