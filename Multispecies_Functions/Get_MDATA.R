## Created 1/26/2017 by Steve Barbeaux 
## This function pulls RACE survey haul and length data for later analysis
## species is a list of species codes for all the species you wish to investigate, the species codes must be in quotes.
## survey 52=AI, 98=EBS Shelf, 78=EBS Slope, and 47 = GOA
## bins can either be specified as in bins=c(0,100,200,300,400) or as percentiles based on time series length composition frequency 
## with the number of divisions entered in bins e.g. bins=5 would be 5 percentiles
## Example use code below for EBS Pcod and EBS pollock and with 5 percentile bins 
## afsc_username=""
## afsc_password=""
## dataM<-Get_MDATA(afsc_username,afsc_password,species=c("21740","21720"),survey = 98,bins = 5,yr=1982:2021)

Get_MDATA<- function(username=afsc_username,password=afsc_password,species=c("21740,21720,10110"),survey = 47,bins = 5, FIG=T,yr=c(1991:2021)) {


  require(RODBC)
  require(data.table)

  years<-paste(yr,collapse=",")
  species<-paste(species,collapse=",")

  if(R.Version()$arch=="i386"){
    AFSC=odbcConnect("AFSC",username,password)
    }
  else {
    AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)
    }

 
  spec<-paste0("SELECT RACEBASE.SPECIES.COMMON_NAME, 
    RACEBASE.SPECIES.SPECIES_NAME, 
    RACEBASE.SPECIES.SPECIES_CODE
    FROM RACEBASE.SPECIES 
    WHERE RACEBASE.SPECIES.SPECIES_CODE IN (", noquote(species),")")

   SN=sqlQuery(AFSC,spec)

     
   test<-paste("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
   "RACEBASE.HAUL.STRATUM,\n",
   "RACEBASE.HAUL.GEAR_TEMPERATURE AS TEMP, \n",
   "RACEBASE.HAUL.SURFACE_TEMPERATURE AS STEMP, \n",
   "RACEBASE.HAUL.BOTTOM_DEPTH AS DEPTH, \n",
   "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
   "RACEBASE.HAUL.END_LONGITUDE AS LON \n",
   "FROM RACE_DATA.V_CRUISES \n",
   "INNER JOIN RACEBASE.HAUL \n",
   "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
   "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID = ", survey," \n",
   "AND RACEBASE.HAUL.HAUL_TYPE = 3 \n",
   "AND RACEBASE.HAUL.PERFORMANCE >= 0 \n", 
   "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n",
   "AND RACEBASE.HAUL.STATIONID IS NOT NULL",sep="")  



  test2<-paste("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
    "RACEBASE.HAUL.STRATUM,\n",
    "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
    "RACEBASE.HAUL.END_LONGITUDE AS LON, \n",
    "RACEBASE.CATCH.NUMBER_FISH/(RACEBASE.HAUL.DISTANCE_FISHED*(RACEBASE.HAUL.NET_WIDTH/1000)) AS CPUE, \n",
    "RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID, \n",
    "RACEBASE.CATCH.SPECIES_CODE \n",
    "FROM RACE_DATA.V_CRUISES \n",
    "INNER JOIN RACEBASE.HAUL \n",
    "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
    "INNER JOIN RACEBASE.CATCH \n",
    "ON RACEBASE.HAUL.CRUISEJOIN = RACEBASE.CATCH.CRUISEJOIN \n",
    "AND RACEBASE.HAUL.HAULJOIN = RACEBASE.CATCH.HAULJOIN \n",
    "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID = ", survey," \n",
    "AND RACEBASE.HAUL.HAUL_TYPE = 3 \n",
    "AND RACEBASE.HAUL.PERFORMANCE >= 0 \n", 
    "AND RACEBASE.HAUL.STATIONID IS NOT NULL \n", 
    "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n",  
    "AND RACEBASE.CATCH.SPECIES_CODE IN (",noquote(species),")", sep="")

  test3<-paste ("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
    "RACEBASE.HAUL.STRATUM,\n",
    "RACEBASE.LENGTH.LENGTH, \n",
    "RACEBASE.LENGTH.FREQUENCY, \n",
    "RACEBASE.LENGTH.SEX, \n",
    "RACEBASE.HAUL.GEAR_TEMPERATURE AS TEMP, \n",
    "RACEBASE.HAUL.SURFACE_TEMPERATURE AS STEMP, \n",
    "RACEBASE.HAUL.BOTTOM_DEPTH AS DEPTH, \n",
    "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
    "RACEBASE.HAUL.END_LONGITUDE AS LON, \n",
    "RACEBASE.LENGTH.SPECIES_CODE, \n",
    "RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID \n",  
    "FROM RACE_DATA.V_CRUISES \n",
    "INNER JOIN RACEBASE.HAUL \n",
    "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
    "INNER JOIN RACEBASE.LENGTH \n",
    "ON RACEBASE.HAUL.CRUISEJOIN                    = RACEBASE.LENGTH.CRUISEJOIN \n",
    "AND RACEBASE.HAUL.HAULJOIN                     = RACEBASE.LENGTH.HAULJOIN \n",
    "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID = ", survey," \n",
    "AND RACEBASE.HAUL.HAUL_TYPE                    = 3 \n",
    "AND RACEBASE.HAUL.PERFORMANCE                 >= 0 \n",
    "AND RACEBASE.HAUL.STATIONID                   IS NOT NULL \n", 
    "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n", 
    "AND RACEBASE.LENGTH.SPECIES_CODE IN (",noquote(species),")", sep="")

  location      <- data.table(sqlQuery(AFSC,test))
  location_poll <- data.table(sqlQuery(AFSC,test2))
  length        <- data.table(sqlQuery(AFSC,test3))
  #length        <- length[SEX<3]
  odbcClose(AFSC)

## if survey is EBS, exlcude far north regions.
  if(survey==98){
    length        <- length[!is.na(STRATUM)&STRATUM<63]
    location      <- location[!is.na(STRATUM)&STRATUM<63]
    location_poll <- location_poll[!is.na(STRATUM)&STRATUM<63]
  }

  if(survey==52){
    length        <- length[!is.na(STRATUM)&STRATUM<800]
    location      <- location[!is.na(STRATUM)&STRATUM<800]
    location_poll <- location_poll[!is.na(STRATUM)&STRATUM<800]
  }

## for SLope survey exclue 2000 from all plots
  if(survey==78){
      length        <- subset(length,length$YEAR!=2000)
      location      <- subset(location,location$YEAR!=2000)
      location_poll <- subset(location_poll,location_poll$YEAR!=2000)
      }

##Exclude null locations and transform to all positive longitudes
  location      <-location[!is.na(location$LON)]
  location_poll <-location_poll[!is.na(location_poll$LON)]
  length        <-length[!is.na(length$LON)]
     
  location$LON[location$LON<0]           <-  360 + location$LON[location$LON<0]
  location_poll$LON[location_poll$LON<0] <-  360 + location_poll$LON[location_poll$LON<0]
  length$LON[length$LON<0]               <-  360 + length$LON[length$LON<0]
      
## rounding bottom depth to nearest 0.5 meters and temperature to nearest 0.1 degree to simplify calculations
  location$DEPTHR <- round(location$DEPTH)
  location$TEMPR  <- round(location$TEMP,1)
      
  length$TEMPR    <- round(length$TEMP,1)
  length$DEPTHR   <- round(length$DEPTH)
  
  length          <- merge(length,location_poll,by = c("YEAR","LON","LAT","STRATUM","SURVEY_DEFINITION_ID", "SPECIES_CODE"))
  length          <-subset(length,!is.na(length$CPUE))

 
  
  ns<-length(unique(length$SPECIES_CODE))
    
  LENGTH1<-vector("list",length=ns)
    
  for(i in 1:ns){
    cn<-SN$COMMON_NAME[i]
    bins1 <- bins
    x<-subset(length,length$SPECIES_CODE==SN$SPECIES_CODE[i])
    x<-data.table(x)

    if(length(bins)>1){
      bins2<-bins
      bins<-length(bins2)
      }
 
    if(length(bins)==1 & FIG==F){
      x2<-quantile(x$LENGTH,probs=seq(0,1,length=(bins1+1)))
      bins2<-c(0,as.numeric(x2[2:bins1]))
      } 
      
    if(bins==5 & FIG==T){
      x2<-quantile(x$LENGTH,probs=c(0.1,0.3,0.7,0.9,1.0))
      bins2<-c(0,as.numeric(x2))
      }
 
## create length bins  
    for ( k in 2:length(bins2)){
      x$BIN[x$LENGTH<bins2[k] & x$LENGTH >= bins2[(k-1)]] <- bins2[(k-1)]
      }

    label<-array(dim=bins)
    label2<-array(dim=bins)
    lab1<-c(bins2[1:bins]/10,max(length$LENGTH/10))
    lab2<-lab1-1
    lab2[1]<-0

    for(m in 1:(bins-1)){
      label[m]<-paste(as.character(lab1)[m],"-",as.character(lab2)[m+1]," cm",sep="")
      label2[m]<-m
      }
    
    label[bins]<-paste(as.character(lab1)[bins],"-",as.character(lab1)[bins+1]," cm",sep="")
    label2[bins]<-bins

    label<-data.frame(BIN=bins2[1:bins],LABEL=as.factor(label),LABEL2=label2) 
    x<-merge(x,label,by="BIN")
    LENGTH1[[i]]<-x
    }
 
  length<-do.call(rbind,LENGTH1)
  
## output data
  data1 <- vector(mode="list")
  data1$length        <- length
  data1$location      <- location
  data1$location_poll <- location_poll
  data1$SN            <- SN
  return(data1)

}
