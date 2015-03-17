
# Calculation of Hill's Diversity Numbers for light trap and resting collection methods

# requried packages
library("plyr")

# Hill's diversity number of order a is defined as:
ha<-function(p.i,a){
  h.a<- (sum((p.i)^a))^(1/(1-a))
  return(h.a)
}
# where p.i is the proportion of species i, when a is two h.a.2 is the reciprocal of Simpson's index
# when a is 0 h.a.0 is the number of species, when a is 1 h.a.1 is undefined by the above equation
# h.a.1 = exp(H') where H' is the Shannon-Weiner diversity index.
ha1<-function(p.i){
  h.a.1<- exp(-(sum((p.i*log(p.i)))))
  return(h.a.1)
}

# calculation of Hill's diversity numbers for resting box and BPD hop cage
dat<-read.table("rest_dat_hills")

#  proportion of each species by method
dat1<-ddply(dat,.(Method),transform,Total=sum(Females))
dat2<-ddply(dat1,.(),transform,Props=Females/Total)

box.props<-subset(dat2,Method=="Box")
hop.props<-subset(dat2,Method=="Hop")
box.h0<-ha(box.props$Props,0)
hop.h0<-ha(hop.props$Props,0)
box.h2<-ha(box.props$Props,2)
hop.h2<-ha(hop.props$Props,2)
box.h1<-ha1(box.props$Props)
hop.h1<-ha1(hop.props$Props)

# calculation of Hill's diversity numbers for resting box and BPD hop cage
# species proportions for each light trap only for those identified to species (or known to be single sp)
dat3<-read.table("all.moz")
dat3a<-dat3[!dat3$LTid %in% 1:70,] # removing first 2 villages
dat4<-dat3a[!dat3a$Species %in% c("Aedes sp", "Anopheles sp.","Armigeres sp","Culex sp.","Mansonia sp",
                                  "Unidentifiable","Cx Cx vish grp","Anopheles Cellia vagus/ subpictus"),]

dat5<-ddply(dat4,.(Species),summarize,spp.tots=sum(Females, na.rm=T))
dat6a<-ddply(dat5,.(),transform,props=spp.tots/sum(spp.tots,na.rm=T))
dat6<-dat6a[!dat6a$spp.tots %in% 0,]
LT.h0<-ha(dat6$props,0)
LT.h0
LT.h1<-ha1(dat6$props[!dat6$props %in% 0])
LT.h1
LT.h2<-ha(dat6$props,2)
LT.h2

