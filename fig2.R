library("plyr")


LT<-read.table("LT.prop")
rest<-read.table("Rest.prop")

# remove villages where resting collection data not available
LT2<-LT[!LT$Village %in% c("1","2","9"),]
LT3<-LT2[,c("Village","Species","Sum","Tot","Prop")]
names(LT3)<-c("Village","Species_name","Sum","Tot","Prop")

OR.func<-function(spp,dat1=rest,dat2=LT3){
  rest2<-subset(rest,Species==spp)
  rest3<-ddply(rest2,.(),transform,minus=Tot-Sumspp)
  LT<-subset(dat2,Species_name==spp)
  names(LT)<-c("LTVillage","LTSpecies","LTSumspp","LTTot","LTProp")
  LT2<-ddply(LT,.(),transform,LTminus=LTTot-LTSumspp)
  both<-cbind.data.frame(LT2,rest3)
  # OR = successes1 x failures2 / successes2 x failures1
  ORa<-ddply(both,.(Village),transform,numer=LTSumspp*minus)
  ORb<-ddply(ORa,.(Village),transform,denom=Sumspp*LTminus)
  OR<-ddply(ORb,.(Village),transform,ORLT.rest=numer/denom)
  OR2<-ddply(OR,.(Village),transform,ORrest.LT=denom/numer)
  OR3<-ddply(OR2,.(Village),transform,logOR.LT.rest=log(ORLT.rest),logOR.rest.LT=log(ORrest.LT))
  #SE of lnOR = sqrt(1/a + 1/b + 1/c + 1/d) 95% CI for lnOR= lnOR +/1 1.96*SE
  OR4<-ddply(OR3,.(Village),transform, SE=sqrt(1/Sumspp + 1/LTSumspp + 1/LTminus + 1/minus))
  OR5<-ddply(OR4,.(Village),transform,CIL=logOR.LT.rest-1.96*SE,CIH=logOR.LT.rest+1.96*SE)
  OR6<-ddply(OR5,.(Village),transform,ORCIL=exp(CIL),ORCIH=exp(CIH))
  return(OR6)
}

tritae.OR<-OR.func(spp="Culex Culex tritaeniorhynchus")
psv.OR<-OR.func(spp="Culex Culex pseudovishnui")
gel.OR<-OR.func(spp="Culex Culex gelidus")

# other common spp but that were not found in all villages
pedi<-subset(rest,Species=="Anopheles Anopheles peditaeniatus")
pediLT<-subset(LT3,Species_name=="Anopheles Anopheles peditaeniatus")

# no pedi caught in resting in villages 3, 5 and 7
pedi.4<-pedi[1,]
pedi.6<-pedi[2,]
pedi.8<-pedi[3,]
pedi.10<-pedi[4,]

pedi[1,]<-c(3,"Anopheles Anopheles peditaeniatus",0,109,0)
pedi[2,]<-pedi.4
pedi[3,]<-c(5,"Anopheles Anopheles peditaeniatus",0,111,0)
pedi[4,]<-pedi.6
pedi[5,]<-c(7,"Anopheles Anopheles peditaeniatus",0,59,0)
pedi[6,]<-pedi.8
pedi[7,]<-pedi.10
pedi$Tot<-as.numeric(pedi$Tot)
pedi$Sumspp<-as.numeric(pedi$Sumspp)

names(pediLT)<-c("Village","Species","LTsum","LTtot","LTprop")
pedi.both<-cbind.data.frame(pediLT,pedi)

pedi.both2<-ddply(pedi.both,.(Village),transform, LTminus=LTtot-LTsum,minus=Tot-Sumspp)
pedi.both3<-ddply(pedi.both2,.(Village),transform, numer=LTsum*minus,denom=Sumspp*LTminus)
pedi.OR<-ddply(pedi.both3,.(Village),transform, ORLT.rest=numer/denom)
p.OR3<-ddply(pedi.OR,.(Village),transform,logOR.LT.rest=log(ORLT.rest))
p.OR4<-ddply(p.OR3,.(Village),transform, SE=sqrt(1/Sumspp + 1/LTsum + 1/LTminus + 1/minus))
p.OR5<-ddply(p.OR4,.(Village),transform,CIL=logOR.LT.rest-1.96*SE,CIH=logOR.LT.rest+1.96*SE)
p.OR6<-ddply(p.OR5,.(Village),transform,ORCIL=exp(CIL),ORCIH=exp(CIH))

# Armigeres subalbatus
Ar.sub<-subset(rest,Species=="Armigeres Armigeres subalbatus")
Ar.3<-Ar.sub[1,]
Ar.5<-Ar.sub[2,]
Ar.6<-Ar.sub[3,]
Ar.7<-Ar.sub[4,]
Ar.8<-Ar.sub[5,]
Ar.10<-Ar.sub[6,]

Ar.sub[1,]<-Ar.3
Ar.sub[2,]<-c(4,"Armigeres Armigeres subalbatus",0,50,0)
Ar.sub[3,]<-Ar.5
Ar.sub[4,]<-Ar.6
Ar.sub[5,]<-Ar.7
Ar.sub[6,]<-Ar.8
Ar.sub[7,]<-Ar.10
Ar.sub$Tot<-as.numeric(Ar.sub$Tot)
Ar.sub$Sumspp<-as.numeric(Ar.sub$Sumspp)

Ar.subLT2<-subset(LT3,Species_name=="Armigeres Armigeres subalbatus")
names(Ar.subLT2)<-c("Village","Species","LTsum","LTtot","LTprop")

Ar.subboth<-cbind.data.frame(Ar.subLT2,Ar.sub)

a.both2<-ddply(Ar.subboth,.(Village),transform, LTminus=LTtot-LTsum,minus=Tot-Sumspp)
a.both3<-ddply(a.both2,.(Village),transform, numer=LTsum*minus,denom=Sumspp*LTminus)
a.OR<-ddply(a.both3,.(Village),transform, ORLT.rest=numer/denom)
a.OR3<-ddply(a.OR,.(Village),transform,logOR.LT.rest=log(ORLT.rest))
#SE of lnOR = sqrt(1/a + 1/b + 1/c + 1/d) 95% CI for lnOR= lnOR +/1 1.96*SE
a.OR4<-ddply(a.OR3,.(Village),transform, SE=sqrt(1/Sumspp + 1/LTsum + 1/LTminus + 1/minus))
a.OR5<-ddply(a.OR4,.(Village),transform,CIL=logOR.LT.rest-1.96*SE,CIH=logOR.LT.rest+1.96*SE)
a.OR6<-ddply(a.OR5,.(Village),transform,ORCIL=exp(CIL),ORCIH=exp(CIH))

# Culex vishnui
vish<-subset(rest,Species=="Culex Culex vishnui")
vish.3<-vish[1,]
vish.4<-vish[2,]
vish.5<-vish[3,]
vish.7<-vish[4,]

vish[1,]<-vish.3
vish[2,]<-vish.4
vish[3,]<-vish.5
vish[4,]<-c(6,"Culex Culex vishnui",0,94,0)
vish[5,]<-vish.7
vish[6,]<-c(8,"Culex Culex vishnui",0,74,0)
vish[7,]<-c(10,"Culex Culex vishnui",0,78,0)
vish$Tot<-as.numeric(vish$Tot)
vish$Sumspp<-as.numeric(vish$Sumspp)

vishLT2<-subset(LT3,Species_name=="Culex Culex vishnui")
names(vishLT2)<-c("Village","Species","LTsum","LTtot","LTprop")

vishboth<-cbind.data.frame(vishLT2,vish)
v.both2<-ddply(vishboth,.(Village),transform, LTminus=LTtot-LTsum,minus=Tot-Sumspp)
v.both3<-ddply(v.both2,.(Village),transform, numer=LTsum*minus,denom=Sumspp*LTminus)
v.OR<-ddply(v.both3,.(Village),transform, ORLT.rest=numer/denom)
v.OR3<-ddply(v.OR,.(Village),transform,logOR.LT.rest=log(ORLT.rest))
v.OR4<-ddply(v.OR3,.(Village),transform, SE=sqrt(1/Sumspp + 1/LTsum + 1/LTminus + 1/minus))
v.OR5<-ddply(v.OR4,.(Village),transform,CIL=logOR.LT.rest-1.96*SE,CIH=logOR.LT.rest+1.96*SE)
v.OR6<-ddply(v.OR5,.(Village),transform,ORCIL=exp(CIL),ORCIH=exp(CIH))

# Armigeres kesseli
Ar.kess<-subset(rest,Species=="Armigeres Armigeres kesseli")
Ar.k3<-Ar.kess[1,]
Ar.k6<-Ar.kess[2,]
Ar.k7<-Ar.kess[3,]
Ar.k8<-Ar.kess[4,]
Ar.k10<-Ar.kess[5,]

Ar.kess[1,]<-Ar.k3
Ar.kess[2,]<-c(4,"Armigeres Armigeres kesseli",0,50,0)
Ar.kess[3,]<-c(5,"Armigeres Armigeres kesseli",0,50,0)
Ar.kess[4,]<-Ar.k6
Ar.kess[5,]<-Ar.k7
Ar.kess[6,]<-Ar.k8
Ar.kess[7,]<-Ar.k10
Ar.kess$Tot<-as.numeric(Ar.kess$Tot)
Ar.kess$Sumspp<-as.numeric(Ar.kess$Sumspp)

Ar.kessLT2<-subset(LT3,Species_name=="Armigeres Armigeres kesseli")
names(Ar.kessLT2)<-c("Village","Species","LTsum","LTtot","LTprop")

Ar.kL3<-Ar.kessLT2[1,]
Ar.kL5<-Ar.kessLT2[2,]
Ar.kL6<-Ar.kessLT2[3,]
Ar.kL7<-Ar.kessLT2[4,]
Ar.kL8<-Ar.kessLT2[5,]
Ar.kL10<-Ar.kessLT2[6,]

Ar.kessLT2[1,]<-Ar.k3
Ar.kessLT2[2,]<-c(4,"Armigeres Armigeres kesseli",0,50,0)
Ar.kessLT2[3,]<-Ar.kL5
Ar.kessLT2[4,]<-Ar.kL6
Ar.kessLT2[5,]<-Ar.kL7
Ar.kessLT2[6,]<-Ar.kL8
Ar.kessLT2[7,]<-Ar.kL10
Ar.kessLT2$LTtot<-as.numeric(Ar.kessLT2$LTtot)
Ar.kessLT2$LTsum<-as.numeric(Ar.kessLT2$LTsum)

Ar.kessboth<-cbind.data.frame(Ar.kessLT2,Ar.kess)

ak.both2<-ddply(Ar.kessboth,.(Village),transform, LTminus=LTtot-LTsum,minus=Tot-Sumspp)
ak.both3<-ddply(ak.both2,.(Village),transform, numer=LTsum*minus,denom=Sumspp*LTminus)
ak.OR<-ddply(ak.both3,.(Village),transform, ORLT.rest=numer/denom)
ak.OR3<-ddply(ak.OR,.(Village),transform,logOR.LT.rest=log(ORLT.rest))
ak.OR4<-ddply(ak.OR3,.(Village),transform, SE=sqrt(1/Sumspp + 1/LTsum + 1/LTminus + 1/minus))
ak.OR5<-ddply(ak.OR4,.(Village),transform,CIL=logOR.LT.rest-1.96*SE,CIH=logOR.LT.rest+1.96*SE)
ak.OR6<-ddply(ak.OR5,.(Village),transform,ORCIL=exp(CIL),ORCIH=exp(CIH))

tiff("Fig2.tiff", height = 4, width = 2.5, units = 'in', compression="lzw", res=400)

# Plotting 
par(mai=c(1.5,1.5,2,2),mar=c(1.5,1.5,2,2),oma=c(2,1.5,0,0),mfrow=c(4,2),cex=0.5)
plot.or<-function(dat,species){
  nr<-nrow(dat)
  plot(dat$logOR.LT.rest,1:nr,type="n",xlim=c(-6,6),main=species,mgp=c(3,0.3,0),yaxt="n",bty="n",cex.main=0.7, cex.axis=0.7)
  points(x=dat$logOR.LT.rest,y=1:nr,pch=20,col="blue",cex=0.5)
  abline(v=0)
  segments(dat$CIH, 1:nr, dat$CIL,1:nr,lwd=0.5)
  ylabels<-c(1,2,3,4,5,6,8)
  axis(2, 1:nr, ylabels,cex.axis=0.7)
  mtext("Method 2",side=3,line=0,cex=0.3,at=4)
  mtext("Method 1",side=3,line=0,cex=0.3,at=-4)

}

plot.or(dat=tritae.OR,species=expression(paste(italic("Cx. tritaeniorhynchus"))))
plot.or(psv.OR,species=expression(paste(italic("Cx. pseudovishnui"))))
plot.or(gel.OR,species=expression(paste(italic("Cx. gelidus"))))
plot.or(p.OR6,species=expression(paste(italic("An. peditaeniatus"))))
plot.or(a.OR6,species=expression(paste(italic("Ar. subalbatus"))))
plot.or(v.OR6,species=expression(paste(italic("Cx. vishnui"))))
plot.or(ak.OR6,species=expression(paste(italic("Ar. kesseli"))))

mtext("Log odds ratio (method 2: method 1) and 95% CI",side=1,outer=T,line=0,cex=0.4)
mtext("Village",side=2,outer=T,line=0,cex=0.4)

dev.off()
