## Created 1/26/2017 by Steven Barbeaux
## Function calculates a timeseries of mean bottom temperature from the RACE surveys following Spencer (2006) for all surveys years
## http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2419.2008.00486.x/full
## The Bering Sea is likely the only one that works as intended. May need some work for the other regions. I think that the GOA Stratum surveyed are not consistent across time
## and therefore the mean temps across years are not going to be consistent in this treatment.
## Uses data from the Get_DATA.r function
## plotT=T plots temperatures over time
## The temperature anomaly is based on the full timeseries average bottom temp.
## "WARM" is > 0.5std from the mean, "COLD" is < 0.5std from the mean "MED" is between the two 



Get_TEMP<-function(dataT=data1, plotT=T){

  require(data.table)
  
  location      <- data.table(dataT$location)
  survey        <- unique(dataT$length$SURVEY_DEFINITION_ID)



  SURVEY1  <- c(rep(47,59),rep(52,49),rep(98,18))

## strata ids
  STRATA1 <- c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,
    210,220,221,230,231,232,240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,
    212,213,214,221,222,223,224,311,312,313,314,321,322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,
    594,611,612,613,614,621,622,623,624,711,712,721,722,793,794,811,812,813,814,10,20,31,32,41,42,43,50,61,62,70,81,82,90,
    140,150,160,170)

## strata areas km^2
  AREA1 <- c(8192.574997,13682.65625,6876.381836,12399.20117,7941.368652,7302.438965,10792.57031,5766.484863,15403.96484,9887.424805,
    5260.411179,2199.774414,9947.788055,6714.710144,6546.749786,4247.325684,8152.350098,2278.351074,11104.63574,7735.040283,
    5010.910645,7912.287903,7337.189453,10981.39355,12077.79492,5025.512177,7346.035156,5276.860352,9032.429688,7727.947754,
    4196.598648,6888.172363,2788.070313,10018.25,1528.385864,6659.834412,1622.698395,3208.446533,3043.014069,2127.350769,
    1125.320068,3927.273926,2531.211182,1604.039185,2911.924805,1107.272964,1520.634399,2344.441895,772.6966553,2005.69043,
    1953.303955,1744.927856,1469.337891,1033.513306,1937.304199,3065.696777,3494.051025,1887.523682,1206.404175,3694.263541,
    4064.2204,940.2248052,1711.085075,1183.328831,1252.458884,783.4029729,1561.38569,960.0232771,1735.543661,766.4301818,
    1237.474859,2105.571189,1066.71488,438.3369788,1239.741186,1617.722884,1052.298382,426.0343037,789.2104159,1164.046529,
    751.7920926,477.3601063,714.3037925,1944.355707,1583.079015,155.9293403,1268.052932,2012.546625,1968.562267,2669.984931,
    1908.646009,2260.881404,716.3752055,438.2089785,1760.938457,1861.249777,2079.252159,2574.749486,1585.638665,669.5128599,
    2440.256707,1179.114117,563.8576854,1043.172833,0.49439225,346.4207737,485.4830076,1419.98111,78702.648,41328.676,94983.172,
    8935.522,62875.391,24242.438,21319.879,38989.602,88753.977,6462.794,73353.117,35392.938,20897.016,11542.001,88280.469,25800.449,
    41680.52,20115.686)


  area<-data.table(SURVEY=SURVEY1,STRATUM=STRATA1,AREA=AREA1)

  

  area<-area[SURVEY==survey]
  
  area<-subset(area,area$STRATUM<800)

if(survey==78){area=data.table(SURVEY=78,STRATUM=unique(data$location$STRATUM),AREA=1)}

  loca<-location[!is.na(TEMP)]
  loca<-loca[!is.na(STEMP)]
  loca<-subset(loca,loca$STRATUM<800)

  loca<-merge(loca,area,by="STRATUM",all.x=T)

  loca<-loca[!is.na(AREA)]
  ar1<-loca[,list(AREA=max(AREA)),by='YEAR,STRATUM']
  ar2<-ar1[,list(AREA2=sum(AREA)),by='YEAR']
  ar3<-merge(ar2,ar1,by="YEAR")
  ar3$PROP_AREA<-ar3$AREA/ar3$AREA2
  ar3<-subset(ar3,select=-c(AREA2,AREA))

   ## calculating mean temperature based on Spencer(2009)

    loca<-merge(loca,ar3,by=c("YEAR","STRATUM"))
    t1<-loca[,list(NUM=length(TEMP)),by='YEAR,STRATUM,PROP_AREA']
    t1$W<-t1$PROP_AREA/t1$NUM
    t1<-subset(t1,select=-c(NUM,PROP_AREA))
    t2<-merge(loca,t1,by=c("YEAR","STRATUM"))
    t2$T1<-t2$W*t2$TEMP
    t2$D1<-t2$W*t2$DEPTH
    t3<-t2[,list(MTEMP=sum(T1),MDEPTH=sum(D1)),by='YEAR']
    
    xTEMP<-mean(t3$MTEMP)
    sdTEMP<-sd(t3$MTEMP)
    t3$REGI="MED"
    t3$REGI[t3$MTEMP<=(xTEMP-(0.5*sdTEMP))]="COLD"
    t3$REGI[t3$MTEMP>=(xTEMP+(0.5*sdTEMP))]<-"WARM"

    t3$xtemp<-t3$MTEMP-mean(t3$MTEMP)
    t3$SD<-sdTEMP
    t3$MT<-xTEMP

    t3$TEMP1<-"gray80"
    t3$TEMP1[t3$REGI=="MED"]<-"gray50"
    t3$TEMP1[t3$REGI=="COLD"]<-"gray20"

    t3$TEMP2<-"salmon"
    t3$TEMP2[t3$REGI=="MED"]<-"gray50"
    t3$TEMP2[t3$REGI=="COLD"]<-"light blue"


    if(plotT==T){

      t3$PCH<-18
      t3$PCH[t3$REGI=="MED"]<-16
      t3$PCH[t3$REGI=="COLD"]<-17
      max_t<-max(t3$xtemp)
      MYR<-min(t3$YEAR)-0.5

      plot(t3$xtemp~t3$YEAR,pch=18,cex=2,type="h",lwd=2,xlab="Year",ylab=expression(paste("Temp. anomaly (",degree,"C)")))
      points(t3$xtemp~t3$YEAR,col=t3$TEMP2,pch=t3$PCH,cex=1.5)
      abline(0,0,lty=2,lwd=2)
      text(MYR,max_t,"Warm",pos=4)
      text(MYR,(max_t*0.9),"Medium",pos=4)
      text(MYR,(max_t*0.8),"Cold",pos=4)
      points(rep(MYR,3),c(max_t,(max_t*0.9),(max_t*0.8)),pch=c(18,16,17),col=c("salmon","gray50","light blue"))
      }
    t3
  }
