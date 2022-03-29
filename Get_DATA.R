## Created 1/26/2017 by Steve Barbeaux 
## This function pulls RACE survey haul and length data for later analysis
## survey 52=AI,98=EBS Shelf, 78=EBS Slope, and 47 = GOA
## bins can either be specified as in bins=c(0,100,200,300,400) or as percentiles based on time series length composition frequency 
## with the number of divisions entered in bins e.g. bins=5 would be 5 percentiles, er... pentiles?
## Example use code below for GOA Pcod and with 5 percentile bins 
#3 afsc_username=""
## afsc_password=""
## data_1<-Get_DATA(username=afsc_username,password=afsc_password,species=21720,survey = 98,bins = 5,FIG=T,yr=1982:2021)

Get_DATA<- function(username=afsc_username,password=afsc_password,species=21720,survey = 98,bins = 5, FIG=T,yr=c(1982:2016)) {


  require(RODBC)
  require(data.table)

  years<-paste(yr,collapse=",")

  if(R.Version()$arch=="i386"){
    AFSC=odbcConnect("AFSC",username,password)
    }
  else {
    AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)
    }

 
  spec<-paste("SELECT RACEBASE.SPECIES.COMMON_NAME, \n",
    "RACEBASE.SPECIES.SPECIES_NAME \n",
    "FROM RACEBASE.SPECIES \n",
    "WHERE RACEBASE.SPECIES.SPECIES_CODE = ", species, sep="")


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
    "AND RACEBASE.CATCH.SPECIES_CODE = ",species, sep="")

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
    "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(paste(years,collapse=",")),") \n", 
    "AND RACEBASE.LENGTH.SPECIES_CODE = ", species, sep="")

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
  
  length          <- merge(length,location_poll,by = c("YEAR","LON","LAT","SURVEY_DEFINITION_ID", "STRATUM","SPECIES_CODE"))
  length          <-subset(length,!is.na(length$CPUE))

  
  if(length(bins)>1){
    bins2<-bins
    bins<-length(bins2)
    }

  if(length(bins)==1 & FIG==F){
    x<-quantile(length$LENGTH,probs=seq(0,1,length=(bins+1)))
    bins2<-c(0,as.numeric(x$BIN[2:bins]))
  } 


  if(bins==5 & FIG==T){
     x<-quantile(length$LENGTH,probs=c(0.1,0.3,0.7,0.9,1.0))
    bins2<-c(0,as.numeric(x))
  } 

  
## create length bins  
  for ( i in 2:length(bins2)){
    length$BIN[length$LENGTH<bins2[i] & length$LENGTH >= bins2[(i-1)]] <- bins2[(i-1)]
    }
 
  label<-array(dim=bins)
  label2<-array(dim=bins)
   

  lab1<-c(bins2[1:bins]/10,max(length$LENGTH/10))
  lab2<-lab1-1
  lab2[1]<-0
  
   for(i in 1:(bins-1)){
        label[i]<-paste(as.character(lab1)[i],"-",as.character(lab2)[i+1]," cm",sep="")
        label2[i]<-i

      }
  
  label[bins]<-paste(as.character(lab1)[bins],"-",as.character(lab1)[bins+1]," cm",sep="")
  label2[bins]<-bins

   label<-data.frame(BIN=bins2[1:bins],LABEL=as.factor(label)) 
   length<-length[!is.na(length$TEMPR)]
   length<-merge(length,label,by="BIN")
  
## output data
  data1 <- vector(mode="list")
  data1$length        <- length
  data1$location      <- location
  data1$location_poll <- location_poll
  data1$SN            <- SN
  return(data1)

}
