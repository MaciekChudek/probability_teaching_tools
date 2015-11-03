

simulate_expon = function(l, n){
	dens_col = 'lightgreen'
	line_col = rgb(0,0,1,.4)
	d = rexp(n,l)
	#xs = seq(0,floor(qexp(.99,l)), length.out=100)
	xs = seq(0,max(d)+1, length.out=100)
	ys = dexp(xs, l)
	hist(d, 20,freq=F, col=dens_col, main='', xlab='How many miles it drove')
	lines(xs,ys, lwd=4, col=line_col)
}

simulate_expon(.2, 10)
simulate_expon(.2, 100)
simulate_expon(.2, 100000)
