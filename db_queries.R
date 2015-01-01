
# Querying Moz.db to retrieve data required for production of figures and tables

# packages required to run the script:
library("tools")
library("DBI")
library("RSQLite") 

# connect to the database Moz.db
drv<-dbDriver("SQLite") 
moz<-dbConnect(drv,"Moz.db")

# function to allow queries to be sent and retrieved from Moz.db
query.func<-function(db=moz,query){
	Q<-query
	sendquery<-dbSendQuery(db,query)
	sendquery
	table<-fetch(sendquery,n=-1)
	dbClearResult(sendquery)
	return(table)
}

# village information query
village<-query.func(,"select * 
										from Village")

# light-trap queries

lt.1<-query.func(,"select Village, LT_individual_ID, Host, Females, Species_name, Spp_ID
											from LT_night N, LT_individual I, LT_species S, Species SP
											where N.LT_night_ID=I.LT_night and I.LT_individual_ID=S.LT_individual 
											and S.Species=SP.Spp_ID")

lt.2<-query.func(,"select Village, LT_individual_ID, Hosts, Number,  Females, Species_name, Spp_ID
											from LT_night N, LT_individual I, Other_hosts O, LT_species S, Species SP
											where N.LT_night_ID=I.LT_night and I.LT_individual_ID=O.LT_Ind_ID and I.LT_individual_ID=S.LT_individual 
												and S.Species=SP.Spp_ID")

lt.3<-query.func(,"select Village, LT_individual_ID, Hosts, Number
											from LT_night N, LT_individual I, Other_hosts O
											where N.LT_night_ID=I.LT_night and I.LT_individual_ID=O.LT_Ind_ID
													order by LT_individual_ID
													")

# resting collection queries
hop<-query.func(,"select Village_name, Village_ID, Date, Habitat, Number_hops, Transect_length, Time, R_BPD_ID, R_species_ID, Females, Half_gravid, Fed, Sub_gravid, Unfed, Species, Species_name
								from Village V, R_day D, R_BPD B, R_species S, Species P
								where V.Village_ID=D.Village and D.R_day_ID=B.R_day and B.R_BPD_ID=S.R_BPD and S.Species=P.Spp_ID")

box<-query.func(,"select Village_name, Village_ID, Date, Habitat, Number_Boxes, Time, R_species_ID, R_Box_ID, Females, Half_gravid, Fed, Sub_gravid, Unfed, Species, Species_name
								from Village V, R_day D, R_Box B, R_species S, Species P
								where V.Village_ID=D.Village and D.R_day_ID=B.R_day and B.R_box_ID=S.R_box and S.Species=P.Spp_ID")

hop.details<-query.func(,"select Village_name, Village_ID, Date, Habitat, Number_hops, Transect_length, Time, R_BPD_ID
											from Village V, R_day D, R_BPD B
											where V.Village_ID=D.Village and D.R_day_ID=B.R_day")

box.details<-query.func(,"select Village_name, Village_ID, Date, Habitat, Number_Boxes, Time, R_Box_ID
								from Village V, R_day D, R_Box B
								where V.Village_ID=D.Village and D.R_day_ID=B.R_day")
