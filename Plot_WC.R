## getting survey tempertures
Get_td <-
function(data=data_1, plotT=T){

  require(data.table)

  
  location      <- data.table(data$location)
  survey        <- unique(data$length$SURVEY_DEFINITION_ID)


SURVEY1  <- c(rep(47,59),rep(52,49),rep(98,18))

     STRATA1 <- c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,
              210,220,221,230,231,232,240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,
              212,213,214,221,222,223,224,311,312,313,314,321,322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,
              594,611,612,613,614,621,622,623,624,711,712,721,722,793,794,811,812,813,814,10,20,31,32,41,42,43,50,61,62,70,81,82,90,
              140,150,160,170)

     AREA1     <- c(8192.574997,13682.65625,6876.381836,12399.20117,7941.368652,7302.438965,10792.57031,5766.484863,15403.96484,9887.424805,
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
     loca<-location[!is.na(TEMP)]
     loca<-merge(loca,area,by="STRATUM",all.x=T)
  
     ar1<-loca[,list(AREA=max(AREA)),by='YEAR,STRATUM']
     ar2<-ar1[,list(AREA2=sum(AREA)),by='YEAR']
     ar3<-merge(ar2,ar1,by="YEAR")
     ar3$PROP_AREA<-ar3$AREA/ar3$AREA2
     ar3<-subset(ar3,select=-c(AREA2,AREA))

   ## calculating mean temperature based on Spencer(2009)

     loca=merge(loca,ar3,by=c("YEAR","STRATUM"))
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



    if(plotT==T){

     t3$PCH<-18
     t3$PCH[t3$REGI=="MED"]<-16
     t3$PCH[t3$REGI=="COLD"]<-17
     max_t<-max(t3$xtemp)
     MYR<-min(t3$YEAR)-0.5

          plot(t3$xtemp~t3$YEAR,pch=18,cex=2,type="h",lwd=2,xlab="Year",ylab=expression(paste("Temp. anomaly (",degree,"C)")))
          points(t3$xtemp~t3$YEAR,col=t3$TEMP1,pch=t3$PCH,cex=1.5)
          abline(0,0,lty=2,lwd=2)
          text(MYR,max_t,"Warm",pos=4)
          text(MYR,(max_t*0.9),"Medium",pos=4)
          text(MYR,(max_t*0.8),"Cold",pos=4)

          points(rep(MYR,3),c(max_t,(max_t*0.9),(max_t*0.8)),pch=c(18,16,17),col=c("gray80","gray50","gray20"))


        }

    t3
   }


## mean tempertures for surveys
Get_Mean_td <-
function(data=data_1){
  require(data.table)
 
  location      <- data.table(data$location)
  length        <- data.table(data$length)
 length         <- length[!is.na(CPUE)]
  location_poll <- data.table(data$location_poll)
  
  survey        <- unique(length$SURVEY_DEFINITION_ID)
  species       <- unique(length$SPECIES_CODE)
  bins          <- sort(unique(length$BIN))
  cn            <- data$SN$COMMON_NAME
  
SURVEY1  <- c(rep(47,59),rep(52,49),rep(98,18))

     STRATA1 <- c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,
              210,220,221,230,231,232,240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,
              212,213,214,221,222,223,224,311,312,313,314,321,322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,
              594,611,612,613,614,621,622,623,624,711,712,721,722,793,794,811,812,813,814,10,20,31,32,41,42,43,50,61,62,70,81,82,90,
              140,150,160,170)

     AREA1     <- c(8192.574997,13682.65625,6876.381836,12399.20117,7941.368652,7302.438965,10792.57031,5766.484863,15403.96484,9887.424805,
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
     loca<-location[!is.na(location$TEMP)]
     loca<-merge(loca,area,by="STRATUM",all.x=T)
  
     ar1 <- loca[,list(AREA=max(AREA)),by='YEAR,STRATUM']
     ar2 <- ar1[,list(AREA2=sum(AREA)),by='YEAR']
     ar3<-merge(ar2,ar1,by=c("YEAR"))
     ar3$PROP_AREA<-ar3$AREA/ar3$AREA2
     ar3<-subset(ar3,select=-c(AREA2,AREA))

   ## calculating mean temperature based on Spencer(2009)

     loca=merge(loca,ar3,by=c("YEAR","STRATUM"))
     t1<-loca[,list(NUM=length(TEMP)),by='YEAR,STRATUM,PROP_AREA']
     t1$W<-t1$PROP_AREA/t1$NUM
     t1<-subset(t1,select=-c(NUM,PROP_AREA))
     t2<-merge(loca,t1,by=c("YEAR","STRATUM"))
     t2$T1<-t2$W*t2$TEMP
     t3<-t2[,list(MTEMP=sum(T1)),by='YEAR']
    
     xTEMP<-mean(t3$MTEMP)
     sdTEMP<-sd(t3$MTEMP)
     t3$REGI="MED"
     t3$REGI[t3$MTEMP<=(xTEMP-(0.5*sdTEMP))]="COLD"
     t3$REGI[t3$MTEMP>=(xTEMP+(0.5*sdTEMP))]<-"WARM"

     MTEMP<-data.table(YEAR=t3$YEAR,REGI=t3$REGI)
  
  ## merge annual temp data with location and length data
   location<-location[!is.na(TEMPR)]
   length<-length[!is.na(TEMPR)]
  
    location<-merge(location,MTEMP,by=c("YEAR"),all=T)
    length<-merge(length,MTEMP,by=c("YEAR"),all=T)

    location1<-location[,list(NUMBER=length(YEAR)),by='DEPTHR,TEMPR,YEAR,REGI']
    data1<- length[,list(FREQ=sum(FREQUENCY)),by='YEAR,REGI,BIN,DEPTHR,TEMPR,CPUE']
    data2<-length[,list(SUM=sum(FREQUENCY)),by='YEAR,DEPTHR,TEMPR,REGI,CPUE']
    data3<-merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR","REGI","CPUE"))
    data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,REGI,BIN,TEMPR,DEPTHR']
     
    data3$TEMPW<-data3$TEMP*data3$PLOT
    data3$DEPTHW<-data3$DEPTH*data3$PLOT
    data6        <- data3[,list(N=sum(PLOT),SDEPTHW=sum(DEPTHW),STEMPW=sum(TEMPW)),by='YEAR,REGI,BIN']
    data6$DEPTHC<-data6$SDEPTHW/data6$N
    data6$TEMPC<- data6$STEMPW/data6$N

    location1$TEMPC=location1$TEMP
    location1$DEPTHC=location1$DEPTH
    location1$BIN=-1
    data6<-merge(location1,data6,all=T,by=c("YEAR","REGI","BIN","TEMPC","DEPTHC"))

    data6<-data6[order(data6$BIN,data6$YEAR),]

    data6$bin3<-as.factor(data6$BIN)
    data6$YEAR<-as.factor(data6$YEAR)

    data6$DEPTHC<-data6$DEPTHC*-1

    
    
    data7 <- subset(data6,data6$BIN>=0)

    data8<-data.frame(YEAR=data7$YEAR,BIN=data7$BIN,TEMPC=data7$TEMPC,DEPTHC=data7$DEPTHC*-1,CN=rep(cn,length(data7$YEAR)))
    data8

}


## ## boxplots of data depth by warm/medium/average bottom temps for the survey by size bins specified in the get data.

Plot_WC <- function(data=data1, plotT=1, depth=c(-250,0)){
        td4<-Get_td(data=data,plotT=F)
	td2<-Get_Mean_td(data=data)
	td2<-merge(td2,td4,all=T,by="YEAR")
	td2<-subset(td2,!is.na(td2$YEAR))
	td2<-td2[order(td2$REGI),]
    x<-sort(unique(td2$BIN[!is.na(td2$BIN)]))
    n=length(x)
    n2=n*3
	if(plotT==1){ 
		boxplot(DEPTHC*-1~REGI+as.factor(BIN),data=td2, ylim=depth, ylab="Depth (m)", xlab="Length(mm)", main=paste(unique(td2$CN[!is.na(td2$CN)])),col=c("light blue", "gray","salmon"),xaxt="n")
		axis(1,at=seq(2,n2,3),labels=x)
	 	points(c(n2-1,n2-1,n2-1),c(0,-20,-40),pch=15, cex=4,col=c("light blue","gray","salmon"))
     	text(n2-1,-40,"  Warm",pos=4)
                text(n2-1,-20,"  Med",pos=4)
     	text(n2-1,0,"  Cold",pos=4)
        abline(mean(td4$MDEPTH)*-1,0,lty=3,col="gray30")
 }

     if(plotT==2){ boxplot(TEMPC~REGI+as.factor(BIN),data=td2, ylim=c(-2,12), ylab=expression(paste("Temp. (",degree,"C)")), xlab="Length(mm)", main=paste(unique(td2$CN[!is.na(td2$CN)])),col=c("light blue", "gray","salmon"),xaxt="n")
	 	axis(1,at=seq(2,n2,3),labels=x)
	 	points(c(n2-1,n2-1,n2-1),c(12,11,10),pch=15, cex=4,col=c("light blue","gray","salmon"))
     	text(n2-1,10,"  Warm",pos=4)
     	text(n2-1,11,"  Med",pos=4)
                text(n2-1,12,"  Cold",pos=4)
     	abline(mean(td4$MTEMP[td4$REGI=="WARM"]),0,lty=3,col="red")
                abline(mean(td4$MTEMP[td4$REGI=="MED"]),0,lty=3,col="gray")
     	abline(mean(td4$MTEMP[td4$REGI=="COLD"]),0,lty=2,col="blue")
     }

}

## boxplots of data location by warm/medium/average bottom temps for the survey by size bins specified in the get data.

Plot_LL <-function(data=data_1,plotT=1){
   location      <- data.table(data$location)
   length        <- data.table(data$length)
   length<- length[!is.na(CPUE)]
   location_poll <- data$location_poll

   td4<-Get_td(data=data,plotT=F)
  
   survey        <- unique(length$SURVEY_DEFINITION_ID)
   species       <- unique(length$SPECIES_CODE)
   bins          <- sort(unique(length$BIN))
   cn            <- data$SN$COMMON_NAME
   sn            <- data$SN$SPECIES_NAME

  
  location2  <- location[,list(NUMBER=length(YEAR)),by= 'YEAR,LON,LAT']
  data1      <- length[,list(FREQ=sum(FREQUENCY)),by= 'YEAR,BIN,LON,LAT,CPUE']
  data2      <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,LON,LAT,CPUE']
  data3      <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT","CPUE"))
  data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LON,LAT']
  
  data6<- data3[,list(SLONW=sum(LON*PLOT),SLATW=sum(LAT*PLOT),N=sum(PLOT)),by='YEAR,BIN']

  data6$LONC <- data6$SLONW/data6$N
  data6$LATC <- data6$SLATW/data6$N

  location2$LONC <- location2$LON
  location2$LATC <- location2$LAT
  location2$BIN <- -1


  dataL     <- merge(location2,data6,all =T,by=c("YEAR","BIN","LONC","LATC"))
  dataL      <- dataL[order(dataL$BIN,dataL$YEAR),]
  dataL$bin3 <- as.factor(dataL$BIN)
  dataL<-merge(dataL,td4,by=c("YEAR"))
  dataL$YEAR <- as.factor(dataL$YEAR)

    data7  <- subset(dataL,dataL$BIN>=0)
    data7$REGI<-as.factor(data7$REGI)
    data8  <- subset(dataL,dataL$BIN<0)
    
    x<-sort(unique(data7$BIN[!is.na(data7$BIN)]))
    n=length(x)
    n2=n*3

    if(plotT==1){ 
        ml<-max(data7$LONC)
        boxplot(LONC~REGI+as.factor(BIN),data=data7, ylab="Longitude", xlab="Length(mm)", main=paste(cn),col=c("light blue", "gray","salmon"),xaxt="n")
        axis(1,at=seq(2,n2,3),labels=x)
        points(c(n2-1,n2-1,n2-1),c(ml,ml-2,ml-4),pch=15, cex=4,col=c("light blue","gray","salmon"))
        text(n2-1,ml-4,"  Warm",pos=4)
         text(n2-1,ml-2,"  Med",pos=4)
        text(n2-1,ml,"  Cold",pos=4)
        abline(mean(data7$LONC),0,lty=3,col="gray30")
    }

    if(plotT==2){ 
        ml<-max(data7$LATC)
        boxplot(LATC~REGI+as.factor(BIN),data=data7, ylab="Latitude", xlab="Length(mm)", main=paste(cn),col=c("light blue","gray", "salmon"),xaxt="n")
        axis(1,at=seq(2,n2,3),labels=x)
        points(c(n2-1,n2-1,n2-1),c(ml,ml-0.5,ml-1),pch=15, cex=4,col=c("light blue","gray","salmon"))
        text(n2-1,ml-1," Warm",pos=4)        
        text(n2-1,ml-0.5," Med",pos=4)
        text(n2-1,ml," Cold",pos=4)
        abline(mean(data7$LATC),0,lty=3,col="gray30")
    }
}
