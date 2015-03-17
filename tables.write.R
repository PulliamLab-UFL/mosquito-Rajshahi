
#************************************** TABLES *****************************************************
# this script will write the tables contained within the manuscript to file

# required packages
library("plyr")
library("reshape")
library("boot")

# run the line below to create the data tables used in this script from db_queries.R and data_prep.R
# these tables are already provided in the github repository
 source("data_prep.R") 
#***************** Table 1 + supplementary table ************************
# mosquito community composition according to resting and light trap collections
# light trap data
all.moz<-read.table("all.moz")
all.moz1<- all.moz[!all.moz$LTid %in% 1:70,] # removing first 2 villages where no host data
all.moz.subset<-all.moz1[,c(1,2,4)]
all.moz.melt<-melt(all.moz.subset, id=c("LTid", "Species"))
spp.totals<-cast(all.moz.melt, Species ~ variable, fun.aggregate=sum)
tot.moz<-sum(all.moz1$Females)
spp.props<-spp.totals$Females/tot.moz*100
all.moz.lt<-cbind.data.frame(spp.totals,spp.props)

# resting collection data
all.moz.rest<-read.table("all.moz.rest")

# merge resting and light trap data into one table
supp.table<-merge(all.moz.lt,all.moz.rest[,c(2,3,5)],by.x=c("Species"),by.y=c("Species_name"),all.x=T,all.y=T)
supp.table$Total[supp.table$Total %in% NA] <- 0
supp.table$Props[supp.table$Props %in% NA] <- 0
names(supp.table)<-c("Species","Light trap total","Percentage of light trap community","Resting collection total","Percentage of resting community")

write.table(supp.table,file="supptable.csv",sep=",",qmethod="double")

table1.a<-supp.table

# confidence intervals for lt proportions 
source("prop.ci.R")
table.1<-cbind.data.frame(table1.a[,1:3],"Light trap lower C.L."=perc.ci.lt[1,],"Light trap upper C.L."=perc.ci.lt[2,],
                          table1.a[,c(4,5)],"Resting collection lower C.L."=perc.ci.rest[1,],"Resting collection upper C.L."=perc.ci.rest[2,])

write.table(table.1,file="table.1.csv",sep=",",qmethod="double")




#********************* Table 2 *************************************
# Hill's diversity numbers according to method used
# the script in the line below runs the analysis to calculate Hill's diversity numbers
source("hills.d.R")
lt<-c(LT.h0,LT.h1,LT.h2)
hop<-c(hop.h0,hop.h1,hop.h2)
box<-c(box.h0,box.h1,box.h2)
all<-rbind.data.frame(lt,hop,box)
table.2<-cbind.data.frame(c("Light trap","Hop","Box"),all)
names(table.2)<-c("Method","H0","H1","H2")

write.table(table.2,file="table.2.csv",sep=",",qmethod="double")

rm(list=ls())



#********************** Table 3 ***************************************
# Linear regression results
source("fig3.4.R")

col1<-c("Cx. tritaeniorhynchus","Cx. gelidus","Cx. pseudovishnui", "An. peditaeniatus")
col2<-rep("Number of cattle*Number of birds",4)

# summary of linear regression results for minimal adequate model obtained from fig3.4.R
summary.t<-summary(t) # Cx. tritaeniorhynchus
summary.p<-summary(p) # Cx. pseudovishnui
summary.g<-summary(g) # Cx. gelidus
summary.a<-summary(a) # An. peditaeniatus

# R^2 values
col3<-c(summary.t[[9]],summary.g[[9]],summary.p[[9]],summary.a[[9]])

# F statistic
col4<-round(c(as.numeric(summary.t[[10]][1]),as.numeric(summary.g[[10]][1]),as.numeric(summary.p[[10]][1]),as.numeric(summary.a[[10]][1])),2)

# p-values 
col5<-c(summary.t[[4]][16],summary.g[[4]][16],summary.p[[4]][16],summary.a[[4]][16])
col6<-c(summary.t[[4]][15],summary.g[[4]][15],summary.p[[4]][15],summary.a[[4]][15])
col7<-c(summary.t[[4]][14],summary.g[[4]][14],summary.p[[4]][14],summary.a[[4]][14])

col8<-c(AIC(t),AIC(g),AIC(p),AIC(a))
col9<-c(AIC(tnull),AIC(gelnull),AIC(psvnull),AIC(pedinull))

table.3<-cbind.data.frame(col1,col2,col4,col5,col6,col7,col8,col9)
names(table.3)<-c("Species","Minimal adequate model","R squared", "Cattle:Bird p-value","Cattle p-value","Bird p-value", "AIC of minimal adequate model","AIC of null model")

write.table(table.3,file="table.3.csv",sep=",",qmethod="double")

rm(list=ls())



#********************** Table 4 ****************************************
# Mosquito relative abundance estimates according to method
source("table4.R")

write.table(table4,file="table.4.csv",sep=",",qmethod="double")

rm(list=ls())

