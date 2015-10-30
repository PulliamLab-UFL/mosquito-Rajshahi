
# required packages
library("effects")
require(plyr)

tritae<-read.table("tritae")
gelidus<-read.table("gelidus")
pseudo<-read.table("pseudo")
pedit<-read.table("pedi")

dat.func<-function(dat){
  # sum all chicken, pigeons and duck in a household into a new column (Bird)
  dat.bird<-ddply(dat,.(LTid),transform, Bird=sum(Chick,Duck,Pigeon))
  # take off one row for the chicken farm (500 birds)- does not fit with
  # typical household data
  dat<-dat.bird[-110,]
  return(dat)
}



# Culex tritaeniorhynchus
tritae1<-dat.func(tritae)	
t1<-lm(log(tritae1$Females +1 ) ~ tritae1$Cattle*tritae1$Goat*tritae1$Bird*tritae1$Human)
drop1(t1,test="F")
t2<-update(t1,.~. -tritae1$Cattle:tritae1$Goat:tritae1$Bird:tritae1$Human)
drop1(t2,test="F")
t3<-update(t2,.~. -tritae1$Cattle:tritae1$Goat:tritae1$Human)
drop1(t3,test="F")
t4<-update(t3, .~. -tritae1$Cattle:tritae1$Goat:tritae1$Bird)
drop1(t4,test="F")
t5<-update(t4,.~. -tritae1$Cattle:tritae1$Goat)
drop1(t5,test="F")
t6<-update(t5,.~. -tritae1$Cattle:tritae1$Bird:tritae1$Human)
drop1(t6,test="F")
t7<-update(t6,.~. -tritae1$Cattle:tritae1$Human)
drop1(t7,test="F")
t8<-update(t7,.~. -tritae1$Goat:tritae1$Bird:tritae1$Human)
drop1(t8,test="F")
t9<-update(t8,.~. -tritae1$Goat:tritae1$Human)
drop1(t9,test="F")
t10<-update(t9,.~. -tritae1$Bird:tritae1$Human)
drop1(t10,test="F")
t11<-update(t10,.~. -tritae1$Goat:tritae1$Bird)
drop1(t11,test="F")
t12<-update(t11,.~. -tritae1$Human)
drop1(t12,test="F")
t13<-update(t12,.~. -tritae1$Goat)
drop1(t13,test="F")
summary(t13)

tnull<-lm(log(Females +1) ~ 1, data=tritae1)



# Culex gelidus
gel<-dat.func(gelidus)
g1<-lm(log(gel$Females + 1) ~gel$Cattle*gel$Goat*gel$Bird*gel$Human)
drop1(g1,test="F")
g2<-update(g1,.~. -gel$Cattle:gel$Goat:gel$Bird:gel$Human)
drop1(g2,test="F")
g3<-update(g2,.~. -gel$Cattle:gel$Goat:gel$Bird)
drop1(g3,test="F")
g4<-update(g3,.~. -gel$Cattle:gel$Bird:gel$Human)
drop1(g4,test="F")
g5<-update(g4,.~. -gel$Cattle:gel$Goat:gel$Human)
drop1(g5,test="F")
g6<-update(g5,.~. -gel$Cattle:gel$Goat)
drop1(g6,test="F")
g7<-update(g6,.~. -gel$Cattle:gel$Human)
drop1(g7,test="F")
g8<-update(g7,.~. - gel$Goat:gel$Bird:gel$Human)
drop1(g8,test="F")
g9<-update(g8,.~. -gel$Goat:gel$Human)
drop1(g9,test="F")
g10<-update(g9,.~.-gel$Bird:gel$Human)
drop1(g10,test="F")
g11<-update(g10,.~. -gel$Human)
drop1(g11,test="F")
g12<-update(g11,.~. -gel$Goat:gel$Bird)
drop1(g12,test="F")
g13<-update(g12,.~. -gel$Goat)
drop1(g13,test="F")

gelnull<-lm(log(Females +1) ~ 1,data=gel)



# Culex pseudovishnui
psv<-dat.func(pseudo)
p1<-lm(log(psv$Females + 1) ~psv$Cattle*psv$Goat*psv$Bird*psv$Human)
drop1(p1,test="F")
p2<-update(p1,.~. -psv$Cattle:psv$Goat:psv$Bird:psv$Human)
drop1(p2,test="F")
p3<-update(p2,.~. -psv$Cattle:psv$Goat:psv$Bird)
drop1(p3,test="F")
p4<-update(p3,.~. -psv$Cattle:psv$Bird:psv$Human)
drop1(p4,test="F")
p5<-update(p4,.~. -psv$Cattle:psv$Goat:psv$Human)
drop1(p5,test="F")
p6<-update(p5,.~. -psv$Cattle:psv$Goat)
drop1(p6,test="F")
p7<-update(p6,.~. -psv$Cattle:psv$Human)
drop1(p7,test="F")
p8<-update(p7,.~. -psv$Goat:psv$Bird:psv$Human)
drop1(p8,test="F")
p9<-update(p8,.~. -psv$Goat:psv$Human)
drop1(p9,test="F")
p10<-update(p9,.~. -psv$Bird:psv$Human)
drop1(p10,test="F")
p11<-update(p10,.~. -psv$Human)
drop1(p11,test="F")
p12<-update(p11,.~. -psv$Goat:psv$Bird)
drop1(p12,test="F")
p13<-update(p12,.~. -psv$Goat)
drop1(p13,test="F")

psvnull<-lm(log(Females +1) ~ 1,data=psv)



# An peditaeniatus
pedi<-dat.func(pedit)
a1<-lm(log(pedi$Females + 1) ~pedi$Cattle*pedi$Goat*pedi$Bird*pedi$Human)
drop1(a1,test="F")
a2<-update(a1,.~. -pedi$Cattle:pedi$Goat:pedi$Birdpedi$Human)
drop1(a2,test="F")
a3<-update(a2,.~. -pedi$Cattle:pedi$Goat:pedi$Bird)
drop1(a3,test="F")
a4<-update(a3,.~. -pedi$Cattle:pedi$Bird:pedi$Human)
drop1(a4,test="F")
a5<-update(a4,.~. -pedi$Cattle:pedi$Goat:pedi$Human)
drop1(a5,test="F")
a6<-update(a5,.~. -pedi$Cattle:pedi$Goat)
drop1(a6,test="F")
a7<-update(a6,.~. -pedi$Cattle:pedi$Human)
drop1(a7,test="F")
a8<-update(a7,.~. -pedi$Goat:pedi$Bird:pedi$Human)
drop1(a8,test="F")
a9<-update(a8,.~. -pedi$Goat:pedi$Bird)
drop1(a9,test="F")
a10<-update(a9,.~. -pedi$Goat:pedi$Human)
drop1(a10,test="F")
a11<-update(a10,.~. -pedi$Goat)
drop1(a11,test="F")
a12<-update(a11,.~. -pedi$Bird:pedi$Human)
drop1(a12,test="F")
a13<-update(a12,.~. -pedi$Human)
drop1(a13,test="F")

pedinull<-lm(log(Females +1) ~ 1,data=pedi)

# Plotting


# fig. 3
Bird<-c(1:120)
Cattle<-rep(0,120)
new.data<-cbind.data.frame(Bird,Cattle)

t<-lm(log(Females +1) ~ Bird*Cattle,data=tritae1)
p<-lm(log(Females +1) ~ Bird*Cattle,data=psv)
g<-lm(log(Females +1) ~ Bird*Cattle,data=gel)
a<-lm(log(Females +1) ~ Bird*Cattle,data=pedi)

t.predict<-predict(t,type="response",newdata=new.data,interval="confidence")
psv.predict<-predict(p,type="response",newdata=new.data,interval="confidence")
gel.predict<-predict(g,type="response",newdata=new.data,interval="confidence")
pedi.predict<-predict(a,type="response",newdata=new.data,interval="confidence")

bird.plot<-function(bird=Bird,dat,spp=expression(paste(italic("Cx. tritaeniorhynchus")))){
  plot(bird,exp(dat[,1]),type="l",log="y",bty="n",lwd=1,cex.main=0.8,cex.axis=0.6,col="#604A7B",ylim=c(1,60000),main=spp)
  lines(x=bird,y=exp(dat[,2]),lty=2,lwd=1,col="black")
  lines(x=bird,y=exp(dat[,3]),lty=2,lwd=1,col="black")
  legend("topleft",legend=c("Predicted value","95% confidence limits"),bty="n",lty=c(1,2),col=c("#604A7B","black"),cex=0.6,lwd=1)
}

tiff("Fig3.tiff", height = 4, width = 4, units = 'in', compression="lzw", res=400)
par(mai=c(1,1,1,0),mar=c(2,2,2,1),mfcol=c(2,2),oma=c(1,1,0,1),cex=0.5,ylog=T)

bird.plot(dat=t.predict)
bird.plot(dat=psv.predict,spp=expression(paste(italic("Cx. pseudovishnui"))))
bird.plot(dat=gel.predict,spp=expression(paste(italic("Cx. gelidus"))))
bird.plot(dat=pedi.predict,spp=expression(paste(italic("An. peditaeniatus"))))
mtext("Number of birds",side=1,outer=T,line=0,cex=0.5)
mtext("Number of female mosquitoes per trap",side=2,outer=T,line=0,cex=0.5)

dev.off()


# fig. 4
Bird<-rep(0,12)
Cattle<-c(1:12)
new.data<-cbind.data.frame(Bird,Cattle)
t.predict2<-predict(t,type="response",newdata=new.data,interval="confidence")
psv.predict2<-predict(p,type="response",newdata=new.data,interval="confidence")
gel.predict2<-predict(g,type="response",newdata=new.data,interval="confidence")
pedi.predict2<-predict(a,type="response",newdata=new.data,interval="confidence")

tiff("Fig4.tiff", height = 3.5, width = 3, units = 'in', compression="lzw", res=400)
par(mai=c(1,1,0.8,0),mar=c(2,2,1,0),mfcol=c(2,2),oma=c(1,1,0,0),cex=0.5,ylog=T)

cattle.plot<-function(cattle=Cattle,dat,spp=expression(paste(italic("Cx. tritaeniorhynchus")))){
  plot(cattle,exp(dat[,1]),type="l",lwd=1,log="y",cex.main=1,col="#604A7B",bty="n",ylim=c(1,30000),main=spp,cex.axis=0.6,xlab="Number of cattle",ylab="Number of female mosquitoes per trap")
  lines(x=cattle,y=exp(dat[,2]),lty=2,lwd=1, col="#604A7B")
  lines(x=cattle,y=exp(dat[,3]),lty=2,lwd=1, col="#604A7B")
  legend("topleft",legend=c("Predicted value","95% confidence limits"),bty="n",lty=c(1,2),lwd=1,col=c("#604A7B","black"),cex=0.4)
}

cattle.plot(dat=t.predict2)

cattle.plot(dat=psv.predict2,spp=expression(paste(italic("Cx. pseudovishnui"))))
cattle.plot(dat=gel.predict2,spp=expression(paste(italic("Cx. gelidus"))))
cattle.plot(dat=pedi.predict2,spp=expression(paste(italic("An. peditaeniatus"))))
mtext("Number of cattle",side=1,outer=T,line=0,cex=0.5)
mtext("Number of female mosquitoes per trap",side=2,outer=T,line=0,cex=0.5)


dev.off()




