## Created by Steven Barbeaux 1/30/2017
## Function to plot RACE survey data centroids of temperature and depth (plotT=1) or location (plotT=2) by sex
## Takes as input data retrieved using the Get_DATA() function, will plot all centroids by year and length bins
## p1 indicates the area of confidence interval ellipse
## Requires the Get_TEMP() function
## Examples: plot_SEX_num(data=data_1,plotT=2,p1=0.9)
##           plot_SEX_num(data=data1,plotT=2,p1=0.9)

plot_SEX_num<- function(data=data1,plotT=1,p1=0.9){
  
  require(data.table)
  require(ggplot2)
  require(grid)
  require(ggmap)



  rgb.palette   <- colorRampPalette(c("#fecc5c","#fd8d3c","#f03b20","#bd0026"),space = "rgb")
  
  weighted.var.se <- function(x, w, na.rm=FALSE)
#  Computes the variance of a weighted mean following Cochran 1977 definition
   {
      if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
      n = length(w)
      xWbar = weighted.mean(x,w,na.rm=na.rm)
      wbar = mean(w)
      out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
      return(out)
  }



  location      <- data.table(data$location)
  length        <- data.table(data$length)
  length        <- length[!is.na(CPUE)]
  location_poll <- data$location_poll
  
  survey        <- unique(length$SURVEY_DEFINITION_ID)
  species       <- unique(length$SPECIES_CODE)
  bins          <- sort(unique(length$BIN))
  cn            <- data$SN$COMMON_NAME
  sn            <- data$SN$SPECIES_NAME

  length        <- length[SEX<3]

  
  loca <- location[!is.na(location$TEMP)& !is.na(location$STRATUM)]
  loca<-subset(loca,loca$STRATUM<800)


  MTEMP <- Get_TEMP(data=data,plotT=F)
  Location <- merge(loca,MTEMP,by=c("YEAR"),all=T)
  

  ## Temperature plot
  if(plotT==1){

    location<-location[!is.na(TEMPR)]
    location<-location[!is.na(STEMP)]
    length<-length[!is.na(TEMPR)]
    length<-length[!is.na(STEMP)]

 	  location1  <- location[,list(NUMBER=length(YEAR)),by= 'YEAR,DEPTHR,TEMPR']
  	data1      <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR,SEX']
  	data2      <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,DEPTHR,TEMPR,SEX']
  	data3      <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR","SEX"))
  	data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR,SEX']
  	data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,BIN,LABEL,SEX']
    data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
    data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

    bins2      <- seq(0,max(log(data3$PLOT)),length=6)



    location1$TEMPC  <- location1$TEMPR
    location1$DEPTHC <- location1$DEPTHR
    location1$BIN    <- -1
    data6 <- merge(location1,data6,all=T,by=c("YEAR","BIN","DEPTHC","TEMPC"))

   	data6$bin3   <- as.factor(data6$BIN)
	  data6$YEAR   <-as.factor(data6$YEAR)
    data6$SEX2 <-as.factor(ifelse(data6$SEX==1,"MALE","FEMALE"))
  	data6$DEPTHC<-data6$DEPTHC*-1
    
    data7<-data6[BIN>=0]
    data8<-data6[BIN<0]

    limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
    limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)

## start GGplot for temperature
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=SEX2,size=SEX2,shape=SEX2,group=LABEL))
      d <- d + ggtitle(paste(cn," (",sn,")",sep=""))
      d <- d + ylim(min(data7$DEPTHC-data7$DEPTHSEM),max(data7$DEPTHC+data7$DEPTHSEM))
      d <- d + xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))
      d <- d + theme(panel.spacing=unit(0,"lines"),panel.background = element_rect(fill = 'white', colour = 'white'), plot.title=element_text(vjust=1,hjust=0))
      d <- d + stat_ellipse(data=data7,aes(x=TEMPC,y=DEPTHC,group=SEX2),level=p1)
      d <- d + geom_point(data=data7,aes(x=TEMPC,y=DEPTHC,color=SEX2,group=LABEL,shape=SEX2,size=SEX2))
      d <- d + geom_pointrange(limDEPTH) 
      d <- d + geom_errorbarh(limTEMP)

      d <- d + scale_colour_manual(name="Sex",values=c(rgb.palette(3))) + scale_size_manual(name="Sex",values=c(0.3,0.3))+scale_shape_manual(name="Sex",values=c(16,17))
      d <- d + xlab(expression("Bottom temp. ("* degree * C *")"))
      d <- d + ylab("Bottom depth (m)")
      d <- d + guides(colour = guide_legend(keyheight=1,override.aes = list(size=0.75)))
      d <- d + facet_wrap(~LABEL,shrink=FALSE)
      print(d)
  } 
  
 ##location plot 
  if(plotT==2){ 
    
        
    location2  <- location[,list(NUMBER=length(YEAR)),by= 'YEAR,LON,LAT']
    data1      <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,LON,LAT,SEX']
    data2      <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,LON,LAT,SEX']
    data3      <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT","SEX"))
    data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,LON,LAT,SEX']
    data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,BIN,LABEL,SEX']
    data6$LONSEM <-sqrt(data6$LONVAR)*1.96
    data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
    location2$LONC <- location2$LON
    location2$LATC <- location2$LAT
    location2$BIN <- -1
   
    dataL   <- merge(location2,data6,all=T,by=c("YEAR","BIN","LONC","LATC"))
    dataL      <- dataL[order(dataL$BIN,dataL$YEAR),]
    dataL$bin3 <- as.factor(dataL$BIN)
    dataL$YEAR <- as.factor(dataL$YEAR)

## Change sex codes to words
    dataL$SEX2 <-as.factor(ifelse(dataL$SEX==1,"MALE","FEMALE"))
    
    B_sea<-map_data("world2","USA:alaska")
    data7<-subset(dataL,dataL$BIN>=0)
    data8<-subset(dataL,dataL$BIN<0)

    p <- ggplot()+ggtitle(paste(cn," (",sn,")",sep=""))
        
## set map coordinates based on survey extent
    if(survey[1]==98)   { p <- p+coord_fixed(ylim=c(51,65),xlim=c(179,205))}
    if(survey[1]==47)   { p <- p+coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
    if(survey[1]==52)   { p <- p+coord_fixed(ylim=c(51,55),xlim=c(170,205))}
    if(survey[1]==78)   { p <- p+coord_fixed(ylim=c(51,63),xlim=c(175,195))}
    if(length(survey)>1){ p <- p+coord_fixed(ylim=c(51,62),xlim=c(170,230))}

    limLAT <- aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM)
    limLON <- aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM)

    ## set the base world to map area   
    base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
         
    map1 <- base_world 
    d    <- map1 + stat_ellipse(data=data7,aes(x=LONC,y=LATC,group=SEX2,color=SEX2,size=SEX2,shape=SEX2),level=p1)
    d    <-    d + geom_point(data=data7,aes(x=LONC,y=LATC,color=SEX2,size=SEX2,SHAPE=SEX2))
    d <- d + geom_pointrange(data=data7,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=SEX2,shape=SEX2,size=SEX2)) 
    d <- d + geom_errorbarh(data=data7,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=SEX2,shape=SEX2,size=SEX2))
    d    <-    d + scale_colour_manual(name="Sex",values=rgb.palette(3) ) + scale_size_manual(name="Sex",values=c(0.3,0.3))+scale_shape_manual(name="Sex",values=c(16,17)) 
    d    <-    d + theme(panel.spacing=unit(0,"lines"),panel.background = element_rect(fill = 'white', colour = 'white'), plot.title=element_text(vjust=1,hjust=0))
        
    if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
     
    d    <-    d + guides(colour = guide_legend(keyheight=1,override.aes = list(size=0.75)))
    d    <-    d + facet_wrap(~LABEL,ncol=2,shrink=TRUE)
    print(d)
        }
}