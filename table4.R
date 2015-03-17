
# Mosquito relative abundance estimates according to method

library("plyr")

moz<-read.table("all.moz")
hosts<-read.table("hosts")

# from light trap 71 onwards
moz1<-moz[moz$LTid %in% 71:196,]

average.func<-function(dat1=hosts,dat2=moz1,spp){
	moz<-subset(dat2,Species==spp)
	dat<-cbind.data.frame(Cattle=hosts$Cattle,moz)
	
	sem <- function(x) {sd(x)/sqrt(length(x))}
	
	nocow<-subset(dat,Cattle=="0")
	cow<-dat[!dat$Cattle %in% "0",]
	
	mean.nocow<-mean(nocow$Females)
	mean.cow<-mean(cow$Females)
	med.nocow<-median(nocow$Females)
	med.cow<-median(cow$Females)
	tot.cow<-sum(cow$Females)
	tot.nocow<-sum(nocow$Females)
	sem.cow<-sem(x=cow$Females)
	sem.nocow<-sem(x=nocow$Females)
	return(c(mean.nocow,sem.nocow,mean.cow,sem.cow,med.nocow,med.cow,tot.cow,tot.nocow))
}

tritae.av<-average.func(spp="Culex Culex tritaeniorhynchus")
psv.av<-average.func(spp="Culex Culex pseudovishnui")
gel.av<-average.func(spp="Culex Culex gelidus")
pedi.av<-average.func(spp="Anopheles Anopheles peditaeniatus")
vish.av<-average.func(spp="Culex Culex vishnui")
cata.av<-average.func(spp="Aedeomyia Aedeomyia catasticta")
subalb.av<-average.func(spp="Armigeres Armigeres subalbatus")
kess.av<-average.func(spp="Armigeres Armigeres kesseli")
bitae.av<-average.func(spp="Culex Culex bitaeniorhynchus")

col1<-c("Cx. tritaeniorhyncus","An. peditaeniatus","Cx. gelidus","Cx. pseudovishnui","Cx. vishnui",
				"Ar. subalbatus","Cx. bitaeniorhynchus","Ar. kesseli","Ad. catasticta")
col2<-round(c(tritae.av[3],pedi.av[3],gel.av[3],psv.av[3],vish.av[3],subalb.av[3],bitae.av[3],kess.av[3],cata.av[3]),1)
col3<-round(c(tritae.av[4],pedi.av[4],gel.av[4],psv.av[4],vish.av[4],subalb.av[4],bitae.av[4],kess.av[4],cata.av[4]),1)
col4<-c(1:9)
col5<-round(col2[1]/col2,2)
col6<-round(c(tritae.av[1],pedi.av[1],gel.av[1],psv.av[1],vish.av[1],subalb.av[1],bitae.av[1],kess.av[1],cata.av[1]),1)
col7<-round(c(tritae.av[2],pedi.av[2],gel.av[2],psv.av[2],vish.av[2],subalb.av[2],bitae.av[2],kess.av[2],cata.av[2]),1)
col8<-c(1,4,3,2,6,8,7,9,5)
col9<-round(col6[1]/col6,2)

# from resting collections
h<-read.table("hop")
b<-read.table("box")


Ind.spp.func<-function(hop,box){
	hop.per.hr<-((sum(hop$Females))/19.48333) # 19.4833 total hop time
	box.per.hr<-((sum(box$Females))/12.65)  # 12.65 #total box time
	sum.hop<-sum(hop$Females)
	sum.box<-sum(box$Females)
	prop.hop<-(sum.hop/422)*100
	prop.box<-(sum.box/153)*100
	return(c(hop.per.hr,box.per.hr,sum.hop,sum.box,prop.hop,prop.box))
}

tritae<-Ind.spp.func(hop=subset(h,Species=="91"),box=subset(b,Species=="91"))
subalb<-Ind.spp.func(hop=subset(h,Species=="67"),box=subset(b,Species=="67"))
pseudo<-Ind.spp.func(hop=subset(h,Species=="86"),box=subset(b,Species=="86"))
gelid<-Ind.spp.func(hop=subset(h,Species=="83"),box=subset(b,Species=="83"))
bitae<-Ind.spp.func(hop=subset(h,Species=="80"),box=subset(b,Species=="80"))
vish<-Ind.spp.func(hop=subset(h,Species=="93"),box=subset(b,Species=="93"))
pedi<-Ind.spp.func(hop=subset(h,Species=="36"),box=subset(b,Species=="36"))
kess<-Ind.spp.func(hop=subset(h,Species=="132"),box=subset(b,Species=="132"))
cata<-Ind.spp.func(hop=subset(h,Species=="36"),box=subset(b,Species=="36"))

col10<-round(c(tritae[1],pedi[1],gelid[1],pseudo[1],vish[1],subalb[1],bitae[1],kess[1],cata[1]),2)
col11<-round(c(tritae[2],pedi[2],gelid[2],pseudo[2],vish[2],subalb[2],bitae[2],kess[2],cata[2]),2)
col12<-c(2,8,3,1,5,4,7,6,9)

table4<-cbind.data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11,col12)
names(table4)<-c("Species that form less than 5% of either resting or light trap samples",
								 "Mean/LT","S.E.M","Rank","Mean abundance of dominant species: other species",
								 "Mean/LT","S.E.M", "Rank","Mean abundance of dominant species: other species","Mosquitoes/hr (BPD hop)",
								 "Mosquitoes/ hr (resting box)","Rank")


