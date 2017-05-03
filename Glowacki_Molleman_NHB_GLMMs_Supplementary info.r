#set the path and read data
#setwd('')
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')
library('lme4')
a$socInfo<-2-a$infoChoice;
a$subsistence<-factor(a$subsistence)

# requesting information (Supplementary Table 2)
b<-subset(a, a$per>1)
m1<-glmer(socInfo ~ subsistence + treatment + per + age + (1|playerNr), family='binomial', data=b)
summary(m1)

library('multcomp')
m1contrast<-glht(m1, linfct = mcp(subsistence = "Tukey"))
summary(m1contrast)


# responding to social information (Supplementary Table 3)
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')
a$subsistence<-factor(a$subsistence)
# use only those instances where social information was requested
a<-subset(a, a$infoChoice==1)
b<-subset(a, a$per>1)
m1<-glmer(opt ~ sumDemoOpt * subsistence + per + age  + (1|playerNr), family='binomial', data=b)
round(summary(m1)$coefficients,3)



# responding to individual payoffs (Supplementary Table 4)
a<-read.table('Experimental_data.txt', header=TRUE, sep='\t')
a$subsistence<-factor(a$subsistence)
b<-matrix(nrow=0, ncol=(length(a)+3))
for (subj in unique(a$playerNr))
{
	switched<-c();
	prevPay<-c();
	d<-subset(a, a$playerNr==subj)
	d<- d[ order(d$treatment, d$per), ]
	for (i in 1:length(d$decision))
	{
		switched[i]<-NA;
		prevPay[i]<-NA;
		if (d$per[i]>1) 
		{
			switched[i]<-abs( d$decision[i] - d$decision[i-1])
			prevPay[i]<- d$payoff[i-1];
		}
	}
	d$switched<-switched;
	d$prevPay<-prevPay;
	b<-rbind(b, d);
}
b<-data.frame(b)
names(b)<-c(names(a), 'switched', 'prevPay');
a<-b;
# use only those instances where a participant observed his own PREVIOUS payoff
a<-subset(a, a$infoChoice==2) 
b<-subset(a, a$per>1)
m1<-glmer(switched ~ prevPay * subsistence + per + age + (1|playerNr), family='binomial', data=b)
round(summary(m1)$coefficients,3)

#in some versions of R, this model fails to converge. A different optimizer ('bobyqa') can be used, yielding the same results
m2<-glmer(switched ~ prevPay * subsistence + per + age + (1|playerNr), family='binomial', data=b, control=glmerControl(optimizer="bobyqa"))
round(summary(m2)$coefficients,3)
