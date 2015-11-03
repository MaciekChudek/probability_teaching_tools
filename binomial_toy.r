
n = 10
p = .5
d = c()
xs = 0:10
ys = dbinom(xs, n, p)
margin = .1
bars = rep(0,11)
target_n = 10
line_col = rgb(0,0,1,.4)
box_col = 'lightgreen'


add_datum = function(x){
	d <<- c(d,x)
	#add_bar(x)
	plot_stacked_boxes()
}

add_data = function(N, total_redraws = 10){
	 data_per_redraw = N/total_redraws
	 for(i in 1:total_redraws){ 
		add_datum(rbinom(data_per_redraw,n,p))
	}
}

add_bar = function(i){
	x = i-1
	bars[x] <<- bars[x]+1
	y = bars[x]
	polygon(c(x-1+margin, x-1+margin, x-margin,x-margin), c(y-1+margin, y-margin,y-margin, y-1+margin), col=box_col)
	if(length(d) > target_n) establish_stacked_boxes();
}

establish_stacked_boxes = function(){
	#establish bounds
	target_n <<- 20*ceiling((length(d))/20)
	if(target_n == 0) target_n = 10
	max_height = ceiling(max(ys*target_n)+1)
	
	#draw predictions
	Ys =ys*target_n
	plot(NA, xlab='',ylab='', xlim=c(0,10), ylim=c(0,max_height), main=paste('Prediction after',target_n,'samples'))
	lines(xs,Ys, lwd=4, col=line_col)
	points(xs,Ys, lwd=4, col=line_col)	
	
	#draw boxes
	if(length(d)>0){ bars <<- hist(d, c(-.5,xs+.5), plot=F)$counts;}else{bars <<- rep(0,11)}
	for(x in 1:length(bars)){		
		if(bars[x]>0){
			for(y in 1:bars[x]){
				polygon(c(x-1+margin, x-1+margin, x-margin,x-margin), c(y-1+margin, y-margin,y-margin, y-1+margin), col='lightgreen')
			}
		}
	}
}

plot_stacked_boxes = function(){
	N = length(d)	
	Ys =ys*N
	M = 20*ceiling((max(Ys)+1)/20)
	plot(NA, xlab='Number of heads',ylab='How many times', xlim=c(0,10), ylim=c(0,M))
	
	#draw predictions
	lines(xs,Ys, lwd=4, col=line_col)
	points(xs,Ys, lwd=4, col=line_col)
	
	#draw boxes
	if(length(d)>0){ bars <<- hist(d, c(-.5,xs+.5), plot=F)$counts;}else{bars <<- rep(0,11)}
	for(x in 1:length(bars)){		
		if(bars[x]>0){
			for(y in 1:bars[x]){
				polygon(c(x-1+margin-.5, x-1+margin-.5, x-margin-.5,x-margin-.5), c(y-1+margin, y-margin,y-margin, y-1+margin), col='lightgreen')
			}
		}
	}
}

plot_hist = function(){
	N = length(d)	
	Ys =ys*N
	hist(d, c(-.5,xs+.5), col=box_col)
	lines(xs,Ys, lwd=4, col=line_col)
	points(xs,Ys, lwd=4, col=line_col)
}


plot_stacked_boxes()

add_datum(5)
#add_data(100)

