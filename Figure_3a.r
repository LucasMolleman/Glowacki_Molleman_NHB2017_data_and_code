#set path and read data
#setwd('')
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')
# only consider those choices where social information was observed
a<-subset(a, a$infoChoice==1)

pos<-c(1,3,2) # shuffle order of dots around within cohort (same as Figure 2)

# define colour parameters and set up empty plot
cols<-c('#76A94C', '#A279BA', '#D06147')
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1, yaxs='i')
plot(0, type='n', xlim=c(-0.2,3.2), ylim=c(0,1), xlab='Number of demonstrators observed choosing optimal option', ylab='Fraction of choices for optimal option', axes=FALSE)

# create some helping lines
for (i in 1:10/10) arrows(-1,i,4,i, code=0, col='grey90')
arrows(-1,0.5,4,0.5, lwd=3, code=0, col='grey90')

axis(1, at=-1:4)
axis(2)
else axis(2, labels=FALSE)

#loop through subsistence styles and plot the dots and a line
for (subs in 1:3)
{
	b<-subset(a, a$subsistence==subs)

	fracB<-rep(0,4); l<-rep(0,4);
	for (i in 0:3)
	{
		d<-subset(b, b$sumDemoOpt==i)
		fracB[i+1]<-mean(d$opt);
		l[i+1]<-length(d$opt)
	}
	
	# add a simple logistic regression line to go through the dots
	m1<-glm(opt ~ sumDemoOpt, family='binomial', data=b)
	a1<-summary(m1)$coef[2]
	b1<-summary(m1)$coef[1]
	
	x<- -100:400/100
	y<- exp(a1*x+b1)/(1+exp(a1*x+b1))
	lines(x,y, col=cols[subs], lty=1, lwd=3)	
	
	# finally, add the dots themselves (with error bars)
	for (i in 0:3)
	{
		p<-fracB[i+1];
		n<-l[i+1];
		se<- sqrt( p * (1-p) / n);
		x<-i-0.1+0.1*(pos[subs]-1)
		arrows(x, p-se, x, p+se, code=0, col='black', lwd=2)
		points(x, p, pch=14+subs, col=cols[subs], cex=2)
	}
}

