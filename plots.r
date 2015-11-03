


#BASIC PLOTS


xs = seq(-8,8, length.out=100)
ys1 = dnorm(xs)
ys2 = dnorm(xs, sd=3)

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/var.png', width=480*2, height=480)

plot(xs, ys1, type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n')
lines(xs, ys2, col='lightblue', lwd=6)

arrows(-1, dnorm(-1), 1, dnorm(1), code=3, col='red', lwd=4)
arrows(-3, dnorm(-3, sd=3), 3, dnorm(3, sd=3), code=3, col='blue', lwd=4)
dev.off()



png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/mean.png', width=480*2, height=480)

plot(xs, ys1, type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n')
lines(xs, ys2, col='lightblue', lwd=6)

arrows(-1, dnorm(-1), 1, dnorm(1), code=3, col='red', lwd=4)
arrows(-3, dnorm(-3, sd=3), 3, dnorm(3, sd=3), code=3, col='blue', lwd=4)
dev.off()



###PSYCH
png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/psych.png')
b = barplot(c(1.1,5.4), ylim=c(0,7), col='lightgreen', main="Participants' answers on a 7-point scale (with std. dev.)", ylab='7-point scale', names.arg=c('Condition A', 'Condition B'))
arrows(b[1],0.3,b[1],1.9, angle=90, code=3, lwd=3)
arrows(b[2],4.8,b[2],6, angle=90, code=3, lwd=3)
dev.off()

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/psych2.png')

xs = seq(-8,8, length.out=100)
ys1 = dnorm(xs, 1.1,.8)
xs2 = seq(-8, 0, length.out=100)
ys2 = dnorm(xs2, 1.1,.8)

plot(xs, ys1, type='l', lwd=6, xlab=expression(sigma),ylab='', xaxt='n', yaxt='n', bty='n')
polygon(c(xs2, rev(xs2)), c(ys2, rep(0,100)), col='red')
polygon(c(xs2, rev(xs2)), c(ys2, rep(0,100)), angle=60, density=6)
polygon(c(xs2, rev(xs2)), c(ys2, rep(0,100)), angle=-60, density=6)
lines(xs, ys1, lwd=6)
axis(1, at=c(-8, 0, 1.1, 8)) 
abline(v=1.1, col='blue', lty=3)
text(-1,dnorm(0, 1.1,.8)/4, 'Impossible values', col='red', cex=1, pos=2)
dev.off()

###NORMAL PLOTS

plot_norm_region = function(alpha){
	xs = seq(-8,8, length.out=100)
	ys1 = dnorm(xs)
	xs2 = seq(qnorm(alpha), qnorm(1-alpha), length.out=100)
	ys2 = dnorm(xs2)
	plot(xs, ys1, type='l', lwd=6, xlab=expression(sigma),ylab='', xaxt='n', yaxt='n', bty='n')
	polygon(c(xs2, rev(xs2)), c(ys2, rep(0,100)), col='lightgreen')
	lines(xs, ys1, lwd=6)
	axis(1, at=round(c(qnorm(alpha),0,qnorm(1-alpha)),2)) 
	text(0, dnorm(0)/4, paste(round(100-alpha*200,1), '%', sep=''),cex=2)
}

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/normA.png')
plot_norm_region(.025); dev.off()
png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/normB.png')
plot_norm_region(0.1586553); dev.off()
png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/normC.png')
plot_norm_region(.05); dev.off()
png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/normD.png')
plot_norm_region(.005); dev.off()




### CENTRAL LIMIT THEORUM PLOTS:


z = runif(1000, -1000, 1000)
z = c(z, (rbeta(1000, .4, .2)*2000 - 1000))
z = c(z, (rbeta(1000, 3, 2)*2000 - 1000))

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/crazy_data.png')
plot(density(z), type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n', main='', col='blue')
abline(h=0, lwd=2)
dev.off()

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/sane_mean.png')
xs = seq(-8,8, length.out=100)
ys1 = dnorm(xs)
plot(xs,ys1, type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n', main='', col='red')
abline(h=0, lwd=2)
dev.off()


png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/crazy_data2.png')
plot(density(z), type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n', main='', col='blue')
yy = density(z)$y[which(abs(density(z)$x) == min(abs(density(z)$x)))]/2
arrows(-600,yy, 600,yy, code=3, lwd=3, col='darkgreen')
text(0,yy*1.1,expression(sigma), cex=2, col='darkgreen')
abline(h=0, lwd=2)
dev.off()

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/sane_mean2.png')
xs = seq(-8,8, length.out=100)
ys1 = dnorm(xs)
plot(xs,ys1, type='l', lwd=6, xlab='',ylab='', xaxt='n', yaxt='n', bty='n', main='', col='red')
yy = dnorm(0)/3
arrows(-1,yy, 1,yy, code=3, lwd=3, col='darkgreen')
text(0,yy*1.4,expression(frac(sigma,sqrt(n))), cex=2, col='darkgreen')
abline(h=0, lwd=2)
dev.off()




#Mean stacks

png('/home/maciek/Work/teaching/ASU_STATS_MODULE/lecture2/images/mean_stack_bars.png', width=480, height=480*1.5)
plot(NA, xlim=c(0,4), ylim=c(0,10), xlab='',ylab='', xaxt='n', yaxt='n', bty='n')

l = .25
r = 1.75
m = l + (r-l)/2


polygon(c(l,l,r,r),c(0,3,3,0), col='lightgreen', lwd=3)
text(m,1.5,'3', cex=6)
polygon(c(l,l,r,r),c(3,5,5,3), col='lightblue', lwd=3)
text(m,4,'2', cex=6)
polygon(c(l,l,r,r),c(5,6,6,5), col='pink', lwd=3)
text(m,5.5,'1', cex=6)
polygon(c(l,l,r,r),c(6,10,10,6), col='lightyellow', lwd=3)
text(m,8,'4', cex=6)

l = 2.25
r = 3.75
m = l + (r-l)/2

polygon(c(l,l,r,r),c(0,2.5,2.5,0), col='coral', lwd=3)
text(m,1.25,'2.5', cex=6)
polygon(c(l,l,r,r),c(2.5,5,5,2.5), col='coral', lwd=3)
text(m,2.5+1.25,'2.5', cex=6)
polygon(c(l,l,r,r),c(5,7.5,7.5,5), col='coral', lwd=3)
text(m,5 + 1.25,'2.5', cex=6)
polygon(c(l,l,r,r),c(7.5,10,10,7.5), col='coral', lwd=3)
text(m,10-1.25,'2.5', cex=6)
dev.off()

### Binom plot?

ys1 = dbinom(xs, 10, .2)
ys2 = dbinom(xs, 10, .5)
ys3 = dbinom(xs, 10, .8)
plot(xs, ys1, lwd=2, col='red', type='l')
points(xs, ys1, col='red')

lines(xs, ys2, lwd=2, col='green')
points(xs, ys2, col='green')

lines(xs, ys3, lwd=2, col='blue')
points(xs, ys3, col='blue')




#MORE BINOM PLOTS, FOR LECTURE 3

png('/home/monkey/Work/teaching/ASU_STATS_MODULE/lecture3/images/nhst1.png', width=900, height=700)
line_col = rgb(0,1,0,.4)
line_col2 = rgb(0,0,1,.4)

xs = seq(0,60, by=1)
xsB = seq(0,60, by=3)
xsC1 = xsB[!1:20%%2]
xsC2 = xsB[!!1:20%%2]
ys1 = dbinom(xs, 60, .5)
ys2 = pbinom(xs, 60, .5)
ys1B = dbinom(xsB, 60, .5)
ys2B = pbinom(xsB, 60, .5)
ys2C1 = pbinom(xsC1, 60, .5)
ys2C2 = pbinom(xsC2, 60, .5)
plot(xs, ys1, lwd=2, col='red', ylim=c(-.03,.15), xlab='R.A.G. score', ylab='Probability (if really random)', yaxt='n', bty='n')
lines(xs, ys1, lwd=2, col=rgb(1,0,0,.4))
text(xsB, -0.015, paste(round(ys1B*100,1),'%',sep=''), pos=3)
text(xsC1, -0.0275, paste(round(ys2C1*100,1),'%',sep=''), col='blue', pos=3)
text(xsC2, -0.0325, paste(round(ys2C2*100,1),'%',sep=''), col='blue', pos=3)
mtext('PMF', 2, -1, at=-0.01, las=1)
mtext('CDF', 2, -1, at=-0.025, las=1, col='blue')


critX1 = qbinom(.95, 60, .5)
lines(c(critX1,critX1), c(0, .14), col=line_col, lwd=2)
arrows(critX1-.5, .135, critX1-10, .135, col=line_col, lwd=2)
arrows(critX1+.5, .135, critX1+5, .135, col=line_col2, lwd=2)
text(critX1-.5, .14, '95% of probability mass', col='green', cex=1.3, pos=2)
text(critX1, 0, critX1, col='green', cex=1.3, pos=1)
text(critX1+.5, .14, '5% of probability mass', col='blue', cex=1.3, pos=4)
dev.off()





png('/home/monkey/Work/teaching/ASU_STATS_MODULE/lecture3/images/nhst2.png', width=900, height=700)

xs = seq(0,60, by=1)
xsB = seq(0,60, by=3)
xsC1 = xsB[!1:20%%2]
xsC2 = xsB[!!1:20%%2]
ys1 = dbinom(xs, 60, .5)
ys2 = pbinom(xs, 60, .5)
ys1B = dbinom(xsB, 60, .5)
ys2B = pbinom(xsB, 60, .5)
ys2C1 = pbinom(xsC1, 60, .5)
ys2C2 = pbinom(xsC2, 60, .5)
plot(xs, ys1, lwd=2, col='red', ylim=c(-.03,.15), xlab='R.A.G. score', ylab='Probability (if really random)', yaxt='n', bty='n')
lines(xs, ys1, lwd=2, col=rgb(1,0,0,.4))
text(xsB, -0.015, paste(round(ys1B*100,1),'%',sep=''), pos=3)
text(xsC1, -0.0275, paste(round(ys2C1*100,1),'%',sep=''), col='blue', pos=3)
text(xsC2, -0.0325, paste(round(ys2C2*100,1),'%',sep=''), col='blue', pos=3)
mtext('PMF', 2, -1, at=-0.01, las=1)
mtext('CDF', 2, -1, at=-0.025, las=1, col='blue')



critX2a = qbinom(.025, 60, .5)
critX2b = qbinom(.975, 60, .5)

lines(c(critX2a,critX2a), c(0, .14), col=line_col, lwd=2)
lines(c(critX2b,critX2b), c(0, .14), col=line_col, lwd=2)
arrows(critX2a+.5, .135, critX2a+5, .135, col=line_col, lwd=2)
arrows(critX2b-.5, .135, critX2b-5, .135, col=line_col, lwd=2)
arrows(critX2a-.5, .135, critX2a-5, .135, col=line_col2, lwd=2)
arrows(critX2b+.5, .135, critX2b+5, .135, col=line_col2, lwd=2)

text(critX2a + (critX2b-critX2a)/2, .14, '95% of probability mass', col='green', cex=1.4)
text(critX2a-.5, .14, '2.5% of probability mass', col='blue', cex=1.3, pos=2)
text(critX2b+.5, .14, '2.5% of probability mass', col='blue', cex=1.3, pos=4)
text(critX2a, 0, critX2a, col='green', cex=1.3, pos=1)
text(critX2b, 0, critX2b, col='green', cex=1.3, pos=1)
dev.off()




png('/home/monkey/Work/teaching/ASU_STATS_MODULE/lecture3/images/confint.png', width=900, height=700)
X = 40
xs = seq(0,1, length.out=100)
ys1 = sapply(xs, function(x) binom.test(40,60,x)$p.value)
plot(xs, ys1, lwd=2, col='red', ylim=c(-.03,1), xlab='Probability of putting money in own cup', ylab='p-value', bty='n')
lines(xs, ys1, lwd=3, col=rgb(1,0,0,.4))

abline(h=.05, col=line_col, lwd=2)

#critX1 = xs[which(xs == min(xs[ys1 >= .05]))-1]
#critX2 = xs[which(xs == max(xs[ys1 >= .05]))+1]

critX1 = xs[which(xs == min(xs[ys1 >= .05]))]
critX2 = xs[which(xs == max(xs[ys1 >= .05]))]
mid_x = critX1 + (critX2 - critX1)/2

lines(c(critX1,critX1), c(0, .9), col=line_col, lwd=3)
lines(c(critX2,critX2), c(0, .9), col=line_col, lwd=3)


arrows(critX1, 0.02, mid_x-.02, 0.02, col=line_col, lwd=3)
arrows(critX2, 0.02, mid_x+.02, 0.02, col=line_col, lwd=3)
text(mid_x, 0, 'Models that predict 40 with p<0.05', col='green', cex=1.3, pos=1)

text(critX1,  binom.test(40,60,critX1)$p.value+.02, round(critX1,2), col='green', cex=1.3, pos=2)
text(critX2,  binom.test(40,60,critX2)$p.value+.02, round(critX2,2), col='green', cex=1.3, pos=4)


dev.off()


png('/home/monkey/Work/teaching/ASU_STATS_MODULE/lecture3/images/normDiff.png', width=900, height=700)
#NORMAL DIFFERENCE THINGY
par(mfrow=c(2,2))
d1= rnorm(1000, -1, 3)+30
d2= rnorm(1000, 1, 3)+30
hist(c(d1,d2), col='lightgreen', breaks=20, xlim=c(20,40), xlab='')
h2a = hist(d1, plot=F, breaks=20)
h2b = hist(d2, plot=F, breaks=20)
plot(h2a, col=rgb(1,0,0,.4), xlim=c(20,40), xlab='', ylab='')
plot(h2b, col=rgb(0,0,1,.4), add=T, xlim=c(-10,10))
xs = seq(-10,10,length.out=100)
ys1 = dnorm(xs, 0, 3.2)
ys2a = dnorm(xs, -1, 3)
ys2b = dnorm(xs, 1, 3)
plot(xs+30, ys1, col='lightgreen', xlim=c(20,40), type='l', ylab='Density', xlab='R.A.G. Score', lwd=4)
plot(xs+30, ys2a, col=rgb(1,0,0,.4), xlim=c(20,40), type='l', xlab='R.A.G. Score', ylab='', lwd=4)
lines(xs+30, ys2b, col=rgb(0,0,1,.4), type='l', lwd=4)
dev.off()

