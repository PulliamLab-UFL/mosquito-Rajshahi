
# Preparation of data returned from querying the database using db_queries.R for production of 
# tables and figures

# preceeding script:
source("db_queries.R")

# required packages:
library("reshape")
library("plyr")

#****************************** Village information ********************************************

write.table(village,file="village.info",sep="\t")



#******************************* light trap (lt) data ******************************************
# data for numbers of domestic animals and humans in a houeshold (hh) only available for lt 71 onwards (not from
# first two villages surveyed)

# household host data
hosts<-lt.3[lt.3$LT_individual_ID %in% c(71:196),] #select from lt 71 onwards
tot.hosts.lt<-ddply(hosts,.(LT_individual_ID),summarize,Tot.H=sum(Number)) # total number of animals in each hh

# only presence recorded in the database so need to incorporate absence
pres.abs<-as.data.frame(table(hosts$Hosts,hosts$LT_individual_ID))
names(pres.abs)<-c("Host","LTid","Present")
p.a.vil<-merge(pres.abs, hosts[,c(1,2)],by.x="LTid",by.y="LT_individual_ID")

# Function that creates a new column of counts to include 0's where the host in question
# is absent.
host.freq.func<-function(host.nam,dat1=p.a.vil,dat2=hosts,tab.nam1){
	# get presence absence data for host in question
	only.p.a<-subset(dat1,Host==host.nam)
	# need to remove duplicated rows
	o.p.a.rm<-only.p.a[!duplicated(only.p.a),]
	# then sort by LTid
	sorted<-o.p.a.rm[with(o.p.a.rm, order(LTid)), ]
	# then take counts where host in question are present
	only.num<-subset(dat2,Hosts==host.nam)
	# merge the 2 data sets
	merge<-merge(sorted, only.num, by.x="LTid",by="LT_individual_ID", all.x=T)
	# take only columns required (e.g. village repeated)
	drop<-merge[,c("LTid","Host","Present","Village.x","Number")]
	# replace NAs with 0 (absent)
	drop$Number[drop$Number %in% NA] <- 0
	final<-drop[,c(1,4:5)]
	names(final)<-c("LTid","Village",tab.nam1)
	return(final)
}

Chicken<-host.freq.func("Chicken",tab.nam1="Chick.num")
Cattle<-host.freq.func("Cattle",tab.nam1="Cattle.num")
Human<-host.freq.func("Human",tab.nam1="Human.num")
Duck<-host.freq.func("Duck",tab.nam1="Duck.num")
Pigeon<-host.freq.func("Pigeon",tab.nam1="Pigeon.num")
Goat<-host.freq.func("Goat",tab.nam1="Goat.num")
Sheep<-host.freq.func("Sheep",tab.nam1="Sheep.num")
Pig<-host.freq.func("Pig",tab.nam1="Pig.num")

all.host.data<-cbind.data.frame(Cattle,Chicken[,"Chick.num"],Human[,"Human.num"],
																Duck[,"Duck.num"],Pigeon[,"Pigeon.num"],
																Goat[,"Goat.num"],Sheep[,"Sheep.num"],Pig[,"Pig.num"])
# All.host.data now contains data for how many of each animal were present in each household

names(all.host.data)<-c("LTid", "Village","Cattle","Chick","Human","Duck","Pigeon","Goat","Sheep","Pig")
write.table(all.host.data,file="hosts",sep="\t")



# mosquito data

# insert 0 and 1 for each mosquito species presence/ absence for each trap
p.a<-as.data.frame(table(lt.1$Species_name,lt.1$LT_individual_ID))
names(p.a)<-c("Species","LTid","Present")

# table of total counts and proportions for each species in each trap
all.moz<-merge(p.a,lt.1[,c(2,3,4)],by.x=c("LTid","Species"),by.y=c("LT_individual_ID","Species_name"),all.x=T)
all.moz$Females[all.moz$Females %in% NA] <- 0
all.moz.tots<-ddply(all.moz,.(LTid),transform,Tot=sum(Females))
all.moz.props<-ddply(all.moz.tots,.(LTid),transform,Prop=Females/Tot)
write.table(all.moz.props,file="all.moz",sep="\t")
 
# for traps only will counts of each host species present in a household
all.moz.props.71<-all.moz.props[all.moz.props$LTid %in% c(71:196),]
write.table(all.moz.props.71,file="all.moz.71")

# create tables for specific species from all.moz.props
ind.spp.func<-function(dat=all.moz.props.71,species,file.nam){
	spp<-subset(dat,Species==species)
	spp1<-cbind.data.frame(all.host.data,spp)
	write.table(spp1,file=file.nam,sep="\t")
}

ind.spp.func(species="Culex Culex tritaeniorhynchus",file.nam="tritae")
ind.spp.func(species="Culex Culex pseudovishnui",file.nam="pseudo")
ind.spp.func(species="Culex Culex gelidus",file.nam="gelidus")
ind.spp.func(species="Culex Culex bitaeniorhynchus",file.nam="bitae")
ind.spp.func(species="Aedeomyia Aedeomyia catasticta",file.nam="cata")
ind.spp.func(species="Anopheles Anopheles peditaeniatus",file.nam="pedi")
ind.spp.func(species="Armigeres Armigeres subalbatus",file.nam="subalb")
ind.spp.func(species="Armigeres Armigeres kesseli",file.nam="kess")
ind.spp.func(species="Culex Culex vishnui",file.nam="vish")

# species proportions by village
p.a<-as.data.frame(table(lt.1$Species_name,lt.1$Village))
names(p.a)<-c("Species","Village","Present")

lt.moz.vil<-merge(p.a,lt.1[,c(1,3,4)],by.x=c("Village","Species"),by.y=c("Village","Species_name"),all.x=T)
lt.moz.vil$Females[lt.moz.vil$Females %in% NA] <- 0
lt.moz.vil2<-ddply(lt.moz.vil,.(Village,Species),summarize,Sum=sum(Females))
lt.moz.vil3<-ddply(lt.moz.vil2,.(Village),transform,Tot=sum(Sum))
vil.moz.props<-ddply(lt.moz.vil3,.(Village),transform,Prop=Sum/Tot)
write.table(vil.moz.props, file="LT.prop",sep="\t")



#****************************** resting collection data ***********************************

# combining data from both resting collection methods
h.num<-rep("Hop",206)
b.num<-rep("Box",77)
h.species<-hop[,c(2,4,7,10:16)]
b.species<-box[,c(2,4,6,9:15)]
h.s<-cbind.data.frame(h.species,h.num)
names(h.s)<-c("Village","Habitat","Time","Females","HG","F","SG","U","Species","Species_name","Method")
b.s<-cbind.data.frame(b.species,b.num)
names(b.s)<-c("Village","Habitat","Time","Females","HG","F","SG","U","Species","Species_name","Method")
h.b.s<-rbind(h.s,b.s)
hbs.melt<-melt(h.b.s,measured=c("Females","HG","F","SG","U"),id=c("Village","Habitat","Time","Species","Species_name","Method"),na.rm=T)
hbs.spp1<-cast(hbs.melt,Species_name + Method ~ variable,fun.aggregate=sum)
hbs.spp2<-ddply(hbs.spp1,.(Species_name),summarize,Total=sum(Females))
hbs.spp3<-ddply(hbs.spp2,.(),transform,All=sum(Total))
hbs.spp4<-ddply(hbs.spp3,.(),transform,Props=round((Total/All)*100,2))
write.table(hbs.spp4,file="all.moz.rest",sep="\t")

# data for Hill's diversity number calculations
# take only counts for those identified to species level (incl. ind. spp. e.g. Mimomyia sp but not groups e.g. vish grp/ Cx sp/ Armigeres sp)
hbs.spp<-hbs.spp1[!hbs.spp1$Species_name %in% c("Anopheles sp.","Armigeres sp","Culex sp.",
																							 "Cx Cx vish grp","None"),]
write.table(hbs.spp,file="rest_dat_hills",sep="\t")

# hop and box data for Table 5
write.table(hop,file="hop",sep="\t")
write.table(box,file="box",sep="\t")


# Species proportions by village
hp<-hop[,c(2,8,10,16)]
hop.m<-rep("Hop",length(hp$Females))
hp2<-cbind.data.frame(hp,hop.m)
names(hp2)<-c("Village","ID","Females","Species","Method")

bp<-box[,c(2,8,9,15)]
box.m<-rep("Box",length(bp$Females))
bp2<-cbind.data.frame(bp,box.m)
names(bp2)<-c("Village","ID","Females","Species","Method")

hb<-rbind.data.frame(bp2,hp2)
hb2<-ddply(hb,.(Village, Species),transform,Sumspp=sum(Females))
hb3<-ddply(hb2,.(Village),transform,Tot=sum(Females))
hb4<-ddply(hb3,.(Village,Species),transform,Prop=Sumspp/Tot)

vil.func<-function(dat=hb4,nam){
	vil<-subset(dat,Village==nam)
	vila<-vil[!duplicated(vil$Species),]
	return(vila)
}

vil3<-vil.func(nam="3")
vil4<-vil.func(nam="4")
vil5<-vil.func(nam="5")
vil6<-vil.func(nam="6")
vil7<-vil.func(nam="7")
vil8<-vil.func(nam="8")
vil10<-vil.func(nam="10")
hb5<-rbind.data.frame(vil3,vil4,vil5,vil6,vil7,vil8,vil10)
hb6<-hb5[,c("Village","Species","Sumspp","Tot","Prop")]
write.table(hb6,file="Rest.prop",sep="\t")

#*****************************Data w.r.t. sampling by village***************************
all.moz<-read.table("all.moz")
# number of light traps per village
num.lt.per.vil<-ddply(lt.1,.(Village),summarize,tot=length(unique(LT_individual_ID)))
max.lt.per.vil<-ddply(lt.1,.(Village),summarize,max.ltid=max(LT_individual_ID))

# resting collections time spent # from resting.R
# see resting.R
#Village tot.time
#1       3      373
#2       4      173
#3       5      547
#4       6      245
#5       7      165
#6       8      221
#7      10      204



