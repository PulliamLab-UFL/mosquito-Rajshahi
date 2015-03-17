

# required packages
library("plyr")

moz<-read.table("all.moz")
hosts<-read.table("hosts")

# from light trap 71 onwards
moz1<-moz[moz$LTid %in% 71:196,]

tot.moz<-ddply(moz1,.(LTid),summarize,Tot=sum(Females))
tot.moz.host<-cbind.data.frame(tot.moz,Cattle=hosts$Cattle)

dat.func<-function(dat=tot.moz.host,moz.dat=moz1,spp){
	
	moz.spp<-subset(moz.dat,Species==spp)
	moz.host<-cbind.data.frame(dat, Females=moz.spp$Females)
	
		no.cows<-subset(moz.host,Cattle==0)
		with.cows<-moz.host[!moz.host$Cattle %in% 0,]

	with.cow.prop<-ddply(with.cows,.(),summarize,prop=(sum(Females)/sum(Tot)))
	no.cows.prop<-ddply(no.cows,.(),summarize,prop=(sum(Females)/sum(Tot)))
	
	return(c(with.cow.prop[,2],no.cows.prop[,2]))
}

tritae<-dat.func(spp="Culex Culex tritaeniorhynchus")
pedi<-dat.func(spp="Anopheles Anopheles peditaeniatus")
gel<-dat.func(spp="Culex Culex gelidus")
psv<-dat.func(spp="Culex Culex pseudovishnui")
vish<-dat.func(spp="Culex Culex vishnui")
sub<-dat.func(spp="Armigeres Armigeres subalbatus")
bitae<-dat.func(spp="Culex Culex bitaeniorhynchus")
kess<-dat.func(spp="Armigeres Armigeres kesseli")
cata<-dat.func(spp="Aedeomyia Aedeomyia catasticta")

# from resting collection

rest<-read.csv("supptable.csv",sep=",")
t<-rest[rest$Species %in% "Culex Culex tritaeniorhynchus",5]/100
p<-rest[rest$Species %in% "Anopheles Anopheles peditaeniatus",5]/100
g<-rest[rest$Species %in% "Culex Culex gelidus",5]/100
ps<-rest[rest$Species %in% "Culex Culex pseudovishnui",5]/100
v<-rest[rest$Species %in% "Culex Culex vishnui",5]/100
s<-rest[rest$Species %in% "Armigeres Armigeres subalbatus",5]/100
b<-rest[rest$Species %in% "Culex Culex bitaeniorhynchus",5]/100
k<-rest[rest$Species %in% "Armigeres Armigeres kesseli",5]/100
c<-rest[rest$Species %in% "Aedeomyia Aedeomyia catasticta",5]/100

props<-c(cata[1],pedi[1],kess[1],sub[1],bitae[1],gel[1],psv[1],tritae[1],vish[1],
         cata[2],pedi[2],kess[2],sub[2],bitae[2],gel[2],psv[2],tritae[2],vish[2],
         c,p,k,s,b,g,ps,t,v)

other1<-1-sum(props[1:9])
other2<-1-sum(props[10:18])
other3<-1-sum(props[19:27])

props<-c(cata[1],pedi[1],kess[1],sub[1],bitae[1],gel[1],psv[1],tritae[1],vish[1],other1,
         cata[2],pedi[2],kess[2],sub[2],bitae[2],gel[2],psv[2],tritae[2],vish[2],other2,
         c,p,k,s,b,g,ps,t,v,other3)

Method.names<-c("LT at HH with cows","LT at HH with no cows","Resting")
prop.matrix<-matrix(data=props, nrow=10,ncol=3)

colors.plot<-c("#8dd3c7",
               "#ffffb3",
               "#bebada",
               "#fb8072",
               "#80b1d3",
               "#fdb462",
               "#b3de69",
               "#fccde5",
               "#d9d9d9",
               "#bc80bd")
tiff("Fig5.tiff", height = 2.5, width = 3, units = 'in', compression="lzw", res=400)
par(mai=c(1.5,1.8,2,1),mar=c(4,5,2,8),mfcol=c(1,1),oma=c(1,1,0,0),cex=0.5,ylog=T,xpd=T)

barplot(prop.matrix,xaxt="n",border=NA,cex.axis=1,cex.lab=1,beside=FALSE,ylim=c(0,1),bty="L",ylab="Proportion of sampled community",col=colors.plot,space=0.7,mar=c(5,4,4,2))

mtext(text=c("Light traps", "at households","with cows", "Light traps", "at households","with no cows","Resting"),side=1,line=c(1,2,3,1,2,3,1),at=c(1.2,1.2,1.2,2.9,2.9,2.9,4.6),cex=0.5)

legend(cex=0.6,y=1,xjust=0.3,x=6,legend=c(expression(paste(italic("Ad. catasticta"))),
                                          expression(paste(italic("An. peditaeniatus"))),
                                          expression(paste(italic("Ar. kesseli"))),
                                          expression(paste(italic("Ar. subalbatus"))),
                                          expression(paste(italic("Cx. bitaeniorhynchus"))),
                                          expression(paste(italic("Cx. gelidus"))),
                                          expression(paste(italic("Cx. pseudovishnui"))),
                                          expression(paste(italic("Cx. tritaeniorhynchus"))),
                                          expression(paste(italic("Cx. vishnui"))),
                                          "Other" ), bty="n",fill=colors.plot)
dev.off()
