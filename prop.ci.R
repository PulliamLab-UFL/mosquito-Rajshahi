
# 95% confidence intervals for light trap proportion data for species in Table 1


# standard
ci<-function(p,N){
	prop<-p/100
	int<-1.96*sqrt(prop*(1-prop)/N)
	lower<-(prop-int)*100
	upper<-(prop+int)*100
	return(c(lower,upper))
}

# light traps

common.spp<-table1.a

perc.ci.lt<-sapply(unique(common.spp$Species),function(id){a<-subset(common.spp,Species==id)
																												confint<-ci(p=a[,3],N=81934)})

# resting collection
perc.ci.rest<-sapply(unique(common.spp$Species),function(id){a<-subset(common.spp,Species==id)
																												confint<-ci(p=a[,5],N=572)})

