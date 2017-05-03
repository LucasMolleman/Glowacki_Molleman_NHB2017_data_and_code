#set the path and read the data
#setwd('')
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')

# specify likelihood of fit (function whose output value will be optimised)
estimateD<- function(v) {
	D<-v[1]

	if (D > 1 || D < (-1)) { return (100000)} # D must be between -1 and 1
	else {
		
		# binomial model (Boyd and Richerson 1985; McElreath et al 2005)
		N<-3;
		x<-0:N;
		y<-c();
		cnt<-1;
		for (i in x){
			
			if (i < (N/2)) {
				y[cnt]<- i * (1-D) / N;
			}
			else{
				y[cnt]<- i*(1-D) / N + D ;
			}
			cnt<-cnt+1;
		}

		# an alternative logit model
		# uncomment next line for alternative logit model (with D as 'temperature')!
		# y<-exp(D*x) / (exp(D*x) + exp(D*(N-x))) 

		p<-c();
		for (i in 1:nrow(data0)){
			case<-data0$sumDemoOpt[i]+1;
			p[i]<- y[case]
		}
		
		data0$p<-p
		
		data0$ProbCorrect <- ifelse(data0$opt == 1,  data0$p, 1-data0$p)
		data0$ProbCorrect <- ifelse(data0$ProbCorrect < 0.00001, 0.00001, data0$ProbCorrect) # remove close to zero for log transform
  		data0$loglikelihood <- log(data0$ProbCorrect)
		LL <- sum(data0$loglikelihood, na.rm = TRUE)
		G2 <- -2*LL 
		
		return (G2)
	}
}

#create parameter matrix storing the optimal fits for each individual
parMat<-data.frame(matrix(nrow=0, ncol=10))
names(parMat)<-c('subj', 'subs_style', 'D', 'optRate', 'numDecs', 'd0', 'd1', 'd2', 'd3', 'fit');
cnt<-1;

# calculate best-fitting D for each individual
for (ind in unique(a$playerNr)){
	data00<-subset(a, a$playerNr==ind)
	data0<-subset(data00, data00$infoChoice==1)
	if (length(data0$opt)>3){ # only consider those for which we have some meaningful amount of data data
		bestFit<- 100000
		output<-optim(c(0), fn = estimateD, method = c("Brent"), control=c(maxit=10000), lower=-1, upper=1)
		if (output$value < bestFit){
		
			bestFit <-output$value
		
			parMat[cnt,1]<-ind;
			parMat[cnt,2]<-data0$subsistence[1];
			parMat[cnt,3]<-output$par[1]
			parMat[cnt,4]<-mean(data00$opt)
			parMat[cnt,5]<-length(data0$decision)
			
			for (i in 0:3){
				d<-subset(data0, data0$sumDemoOpt==i)
				parMat[cnt,6+i]<-mean(d$opt)
			}
			
			parMat[cnt, 10]<- output$value;
			
			# track progress of fitting process
			print(paste('player ', ind, 
			'   D=', round(output$par[1],2) ))
			flush.console();
		}

		cnt<-cnt+1;
	}
} 

round(parMat,3)

# only consider those individuals who had more than 10 decisions based on social information
b<-subset(parMat, parMat$numDecs>10)
bins<-rep(0,11)
cnt<-1;
for (i in -5:5){
	d<-subset(b, round(b$D * 5) == i)
	bins[cnt]<-nrow(d);
	cnt<-cnt+1;
}
bins

y<-bins/sum(bins)

#plot the outcome
cols<-c('#76A94C', '#A279BA', '#D06147')
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1, yaxs='i')
plot(0, type='n', xlim=c(-1,1), axes=FALSE, ylim=c(0,0.4), xlab='Conformist tendency(D)', ylab='Frequency')
axis(1, at=-2:2/2)
axis(2)
for (i in 1:length(bins)){
	x<- (i-6)/5;
	arrows(x,0,x,y[i], col='firebrick', lwd=50, code=0) 
}
arrows(0,-1, 0, 3, lty=2, code=0)

# fraction not 'positively conformist'
nrow(subset(b, b$D<0))/nrow(b)

# are there differences between subsistence styles wrt D? (note that we have quite different sample sizes for each subsistence style)
parMat$subs<-factor(parMat$subs_style)
m1<-lm(D ~ subs, data=parMat)
summary(m1)
library('multcomp')
m1contrast<-glht(m1, linfct = mcp(subs = "Tukey"))
summary(m1contrast)

# did more conformist social learners do better? (data set NOT ideal to answer this question)
m2<-lm(optRate ~ D, data=parMat)
summary(m2)
