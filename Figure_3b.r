#set path and read data
#setwd('')
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')

# add subsistence strategy to each of the decisions
b<-matrix(nrow=0, ncol=(length(a)+3))
for (subj in unique(a$playerNr))
{
	switched<-c();
	prevPay<-c();
	prevDec<-c();
	prevOpt<-c();
	d<-subset(a, a$playerNr==subj)
	d<- d[ order(d$treatment, d$per), ]
	for (i in 1:length(d$decision))
	{
		switched[i]<-NA;
		prevPay[i]<-NA;
		prevDec[i]<-NA;
		if (d$per[i]>1) 
		{
			switched[i]<-abs( d$decision[i] - d$decision[i-1])
			prevPay[i]<- d$payoff[i-1];
			prevDec[i]<-d$decision[i-1];
			prevOpt[i]<-d$opt[i-1];
		}
	}
	d$switched<-switched;
	d$prevPay<-prevPay;
	d$prevDec<-prevDec;
	d$prevOpt<-prevOpt;
	b<-rbind(b, d);
}
b<-data.frame(b)
names(b)<-c(names(a), 'switched', 'prevPay', 'prevDec', 'prevOpt');

a<-b;

a<-subset(a, a$infoChoice==2)
cols<-c('#76A94C', '#A279BA', '#D06147')
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1, yaxs='i')

plot(0, type='n', xlim=c(0,70), ylim=c(0,1), xlab='Previous payoff', ylab='Switching rate', axes=FALSE)
axis(1, at=0:10*10)
#axis(1, at=0:5*20)
axis(2);

for (i in 1:10/10)
{
	arrows(0,i,100,i, col='grey90', code=0)
}

rect(29, 0, 31, 1, col='grey80', border=F)
rect(37, 0, 39, 1, col='grey80', border=F)

pos<-c(1,3,2) # shuffle order of bars around within cohort (now pastoralist on the right)
slopes<-c(); slopeSEs<-c();

nPerSubs<-c(0,0,0)

for (subs in 1:3)
{
	b<-subset(a, a$subsistence==subs)
#	b<-subset(b, b$prevOpt==0)
	nPerSubs[subs]<-length(unique(b$playerNr))

	fracB<-c(); l<-c(); se<-c(); cnt<-1;
	for (i in 0:10)
	{
		d<-subset(b, floor(b$prevPay/10)==i)
		l[cnt]<-0; fracB[cnt]<-NA;
		if (length(d$switched>0))
		{
			p<-mean(d$switched)
			fracB[cnt]<- p
			n<-length(d$switched)
			l[cnt]<-n
			se[cnt]<- sqrt(p * (1 - p) / n)
		}
		cnt<-cnt+1;
	}
	
	m1<-glm(switched ~ prevPay, family='binomial', data=b)
	a1<-summary(m1)$coef[1]
	b1<-summary(m1)$coef[2]
	
	slopes<-c(slopes,b1)
	slopeSEs<-c(slopeSEs,summary(m1)$coef[4])
	
	
	x<- 0:1000/10
	y<- exp(a1+b1*x)/(1+exp(a1+b1*x))

# show predicted confidence intervals using SE of slope estimate
#	xx<-c(x, rev(x))
#	a1SE<-summary(m1)$coef[1]
#	b1SE<-summary(m1)$coef[4]
		
#	y1<- exp(a1+(b1+b1SE)*x)/(1+exp(a1+(b1+b1SE)*x))
#	y2<- exp(a1+(b1-b1SE)*x)/(1+exp(a1+(b1-b1SE)*x))
#	yy<- c(y1, rev(y2))
#	polygon(xx,yy, col=adjustcolor(cols[subs], alpha=0.3), border=FALSE)
	lines(x,y, col=cols[subs], lty=1, lwd=3)	


	xs<-0:10*10+5 - 2 + 2*(pos[subs]-1)

	for (i in 1:length(se))
	{
		x1<-xs[i];
		y1<-fracB[i]; s<-se[i]
		arrows(x1, y1-s, x1, y1+s, code=0, col='black', lwd=2)	
		points(x1, y1, pch=14+subs, cex=2, col=cols[subs])
	
	}
	
#	text(0:3, fracB, l)
}
nPerSubs

#legend('topright', c('pastoralist', 'farming', 'citydwelling'), col=adjustcolor(cols, alpha=0.75), pch=15:17, cex=1.5, bg='white')

slopes
slopeSEs

a$subsis<-factor(a$subsistence);

estMat<-matrix(nrow=3, ncol=9)
library('lme4')

for (i in 1:3)
{
	b<-subset(a, a$subsistence==i)
	m3<- glmer(switched ~ + per + prevPay + (1|playerNr), data=b, family='binomial')
	m3<- glmer(switched ~ + per + prevPay + orderTreatments + IOSingroup + IOSoutgroup + age + understanding*prevPay + (1|playerNr), data=b, family='binomial')
	coefs<-summary(m3)$coefficients
	estMat[,(i-1)*3+1]<-coefs[,1]
	estMat[,(i-1)*3+2]<-coefs[,2]
	estMat[,(i-1)*3+3]<-coefs[,4]
}

estMat<-data.frame(estMat)
names(estMat)<-c('Past_b', 'SE', 'P', 'Farm_b', 'SE', 'P', 'City_b', 'SE', 'P')



m2<-glmer(switched ~ per + treatment * prevPay + subsis + prevPay + (1|playerNr), data=a, family='binomial')
summary(m2)
