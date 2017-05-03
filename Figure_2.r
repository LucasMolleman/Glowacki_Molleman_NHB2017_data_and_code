#set path and read data
#setwd('')
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')

# create matrix counting for each individual his 'reliance on social learning'
mat<-matrix(0, nrow=0, ncol=2)

for (ind in unique(a$playerNr)){
	b<-subset(a, a$playerNr==ind)
	# in period 1, no social information was available yet
	d<-subset(b, b$per>1)
	mat<-rbind(mat, c(b$subsistence[1], 2-mean(d$infoChoice)))
}
mat<-data.frame(mat)
names(mat)<-c('subsistence', 'socialInfoRate')

#create an empty plot with some help lines
cols<-c('#76A94C', '#A279BA', '#D06147')
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1, yaxs='i')
plot(0, type='n', xlim=c(0.5,3.5), ylim=c(0,1), xlab='', ylab='Rate of requests for social information', axes=FALSE, yaxs='i')

for (i in 1:10/10) arrows(-1,i,6,i, code=0, col='grey90')

#set the order of the bars
pos<-c(3,1,2)

subs<-c('pastoralist', 'urban \ndwelling', 'horticulturalist')

#### pooled ingroup and outgroup
ys<-c();
for (subsis in 1:3)
{
	x<-pos[subsis]
	
	b<-subset(mat, mat$subsistence==subsis)$socialInfoRate
	y<-mean(b); n<-length(b)
	ys<-c(ys, y)
	# note that using SEs here is only for descriptive purposes (model in paper uses GLMMs)
	s<- sqrt(y*(1-y)/n)
	rect(4-x-0.2, 0, 4-x+0.2, y, col=cols[subsis])
	arrows(4-x, y-s, 4-x, y+s, lwd=3, code=0)
}
axis(1, at=0:4, labels=FALSE)
mtext(side=1, at=1:3, text=subs, line=c(1.5,2.5,1.5), cex=1.5)
axis(2)

ys
