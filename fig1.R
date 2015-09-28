library("plyr")

LTa<-read.table("all.moz")
LT<-LTa[!LTa$LTid %in% 1:70,]
# remove Cx spp, Mansonia spp etc in Spp_ID 
LT2<-LT[!LT$Species %in% c("Aedes sp","Anopheles Cellia vagus/ subpictus","Anopheles sp.","Armigeres sp",
                           "Culex sp.","Cx Cx vish grp","Mansonia sp","Unidentifiable","Mansonia sp unidentified","Mimomyia sp")
        & !LT$Females %in% 0,]
LT3<-ddply(LT2,.(LTid),transform,tot=sum(Females))

max<-seq(71,196,1)
a<-matrix(rep(0,length(max)*2),ncol=2)

for (i in 1:length(max)){
  samples<-seq(71,max[i],1)
  dat1<-LT3[LT3$LTid %in% samples,]
  a[i,1]<-sum(dat1$Females)
  a[i,2]<-length(unique(dat1$Species))
}

tiff("Fig1.tiff", height = 2.75, width = 3, units = 'in', compression="lzw", res=400)

par(mai=c(1,1,0.5,0),mar=c(4,4,1,1),cex=0.7)
options(scipen=10)
plot(a[,1],a[,2],log="x",ylim=c(0,35),pch=20,cex.points=0.5,col="blue",bty="n",cex.lab=0.8,cex.axis=0.8,ylab="Number of species collected by method 2", xlab="Number of female mosquitoes collected by method 2")
#abline(v=38,col="grey",lty=3)
#abline(v=3500,col="grey",lty=2)
#legend(x=4e+3,y=10,cex=0.5,title="Number collected by:",legend=c("1 light trap","resting collection","7 light traps"),bty="n",lty=c(3,1,2),col=c("grey","grey40","grey"))

#abline(v=575,col="grey40")
lm.sp<-lm(a[,2] ~ log(a[,1] + 1))
summary(lm.sp)
curve(coef(lm.sp)[1] + log(x)*coef(lm.sp)[2],from=1,to=80000,add=T,col="blue")

dev.off()