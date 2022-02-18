plot_YEAR_num<- function(data=data1,plotT=1,REG=TRUE,PATH=TRUE,ELLIP=TRUE,p1=0.9,colx=1){
  require(data.table)
  require(ggplot2)
  require(grid)
  require(ggmap)

  if(colx==2){rgb.palette <- colorRampPalette(c("red","orange","yellow","green","turquoise","blue"),space = "rgb")}
  if(colx==1){rgb.palette <- colorRampPalette(c("#fecc5c","#fd8d3c","#f03b20","#bd0026"),space = "rgb")}
  
  location      <- data$location
  length        <- data$length
  location_poll <- data$location_poll
  
  survey        <- unique(length$SURVEY_DEFINITION_ID)
  species       <- unique(length$SPECIES_CODE)
  bins          <- sort(unique(length$BIN))
  cn            <- data$SN$COMMON_NAME
  sn            <- data$SN$SPECIES_NAME
 
 cex_1<-c(16,15,17,18,21,8,9,24,22,1,5,2,12)
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
  
## temperature by strata plots are not available for the Slope Survey  (78)
  loca <- location[!is.na(location$TEMP)& !is.na(location$STRATUM)]
  loca <- subset(loca,loca$STRATUM<800)

    
  MTEMP <- Get_TEMP(data=data,plotT=F)
  
  location<-location[!is.na(TEMPR)]
  location <- merge(loca,MTEMP,by=c("YEAR"),all=T)
  length   <- length[!is.na(length$TEMPR)]
  length   <- merge(length,MTEMP,by=c("YEAR"),all=T)
  length<-length[!is.na(CPUE)]
  

## Temperature plot
  if(plotT==1){

    if(!REG){ 
      location$T<-1
      location1<-location[,list(NUMBER=sum(T)),by='DEPTHR,TEMPR,YEAR']
      data1<- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by='YEAR,BIN,LABEL,DEPTHR,TEMPR']
      data2<-length[,list(SUM=sum(FREQUENCY)),by='YEAR,DEPTHR,TEMPR']
      data3<-merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR"))
      data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR']
      data6<-data3[,list(TEMPC=weighted.mean(TEMPR,PLOT),DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,BIN,LABEL']
      data6$TEMPSEM <-sqrt(data6$TEMPVAR)*1.96
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      

      location1$TEMPC=location1$TEMP
      location1$DEPTHC=location1$DEPTH
      location1$BIN=-1
      location1$LABEL="0"
      data6<-merge(location1,data6,all=T,by=c("YEAR","BIN","LABEL","TEMPC","DEPTHC"))

      data6<-data6[order(data6$BIN,data6$YEAR),]

      data6$bin3<-as.factor(data6$BIN)
      data6$YEAR<-as.factor(data6$YEAR)

      data6$DEPTHC<-data6$DEPTHC*-1

    
      data7 <- data6[BIN>=0]
      data8 <- data6[BIN<0]
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC))
      
      #d <- d + geom_point(data=location,aes(x=TEMPR,y=-DEPTHR),shape=3,size=0.5,color="gray80")
      d<- d + geom_point(data=data7,aes(x=TEMPC,y=DEPTHC,color=LABEL,shape=LABEL))
      location1 <- location[,list(NUMBER=sum(T)),by= 'DEPTHR,TEMPR']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'BIN,LABEL,DEPTHR,TEMPR']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'DEPTHR,TEMPR']
      data3 <- merge(data1,data2,all=T,by=c("DEPTHR","TEMPR"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'BIN,LABEL,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='BIN,LABEL']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

      location1$TEMPC <- location1$TEMP
      location1$DEPTHC <- location1$DEPTH
      location1$BIN <- -1
      location1$LABEL <- "NA"
      location1<-subset(location1,select=-c(DEPTHR,TEMPR))
      data6 <- merge(location1,data6,all=T, by=c("BIN","LABEL","DEPTHC","TEMPC"))
      data6 <- data6[order(data6$BIN),]
    
      data6$DEPTHC <- data6$DEPTHC*-1
      data9 <- data6[BIN>=0]
      data10 <- data6[BIN<0]
    
     
    limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM,shape=LABEL,color=LABEL)
    limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM,color=LABEL)

    #d<-  d + geom_point(data=data9,aes(x=TEMPC,y=DEPTHC,shape=LABEL),size=1.2)
    d <- d + geom_pointrange(data=data9,limDEPTH) 
    d <- d + geom_errorbarh(data=data9,limTEMP,height=0)
    d <- d + geom_path(data=data9,aes(x=TEMPC,y=DEPTHC,group=1),size=0.4,color="gray20")
    d <- d + scale_colour_manual(name="Size",values=c(rgb.palette(length(unique(data7$LABEL))+1))) + scale_shape_manual(name="Size",values=c(cex_1[1:length(unique(data7$LABEL))]))

    d <- d + ggtitle(paste(cn," (",sn,")",sep=""))
    d <- d + ylim(min(data7$DEPTHC-data7$DEPTHSEM),max(data7$DEPTHC+data7$DEPTHSEM))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))

    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
         
     d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
    d <- d + ylab("Centroid depth (m)")
    d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + theme_minimal(base_size=10)
    print(d)


      }


    if(REG){ 
      location$T<-1
      location1<-location[,list(NUMBER=sum(T)),by='DEPTHR,TEMPR,YEAR,REGI']
      data1<- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by='YEAR,REGI,BIN,LABEL,DEPTHR,TEMPR']
      data2<-length[,list(SUM=sum(FREQUENCY)),by='YEAR,REGI,DEPTHR,TEMPR']
      data3<-merge(data1,data2,all=T,by=c("YEAR","REGI","DEPTHR","TEMPR"))
      data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,REGI,BIN,LABEL,TEMPR,DEPTHR']
      data6<-data3[,list(TEMPC=weighted.mean(TEMPR,PLOT),DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,REGI,BIN,LABEL']
      data6$TEMPSEM <-sqrt(data6$TEMPVAR)*1.96
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      

      location1$TEMPC=location1$TEMP
      location1$DEPTHC=location1$DEPTH
      location1$BIN=-1
      location1$LABEL="0"
      data6<-merge(location1,data6,all=T,by=c("YEAR","REGI","BIN","LABEL","TEMPC","DEPTHC"))

      data6<-data6[order(data6$BIN,data6$YEAR),]

      data6$bin3<-as.factor(data6$BIN)
      data6$YEAR<-as.factor(data6$YEAR)

      data6$DEPTHC<-data6$DEPTHC*-1

    
      data7 <- data6[BIN>=0]
      data8 <- data6[BIN<0]
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,shape=LABEL,color=LABEL))
      d<- d + geom_point()
      #d <- d + scale_colour_manual(name="Size",values=c(rgb.palette(length(unique(data7$LABEL))+1))) + scale_size_manual(name="Size")

      location1 <- location[,list(NUMBER=sum(T)),by= 'REGI,DEPTHR,TEMPR']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'REGI,BIN,LABEL,DEPTHR,TEMPR']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'REGI,DEPTHR,TEMPR']
      data3 <- merge(data1,data2,all=T,by=c("REGI","DEPTHR","TEMPR"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'REGI,BIN,LABEL,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='REGI,BIN,LABEL']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

      location1$TEMPC <- location1$TEMP
      location1$DEPTHC <- location1$DEPTH
      location1$BIN <- -1
      location1$LABEL <- "NA"
      location1<-subset(location1,select=-c(DEPTHR,TEMPR))
      data6 <- merge(location1,data6,all=T, by=c("REGI","BIN","LABEL","DEPTHC","TEMPC"))
      data6 <- data6[order(data6$BIN),]
    
      data6$DEPTHC <- data6$DEPTHC*-1
      data9 <- data6[BIN>=0]
      data10 <- data6[BIN<0]
    
     
     limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM,shape=LABEL,color=LABEL)
    limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM,color=LABEL)

    #d<-  d + geom_point(data=data9,aes(x=TEMPC,y=DEPTHC,shape=LABEL,color=LABEL),size=1.2)
    d <- d + geom_pointrange(data=data9,limDEPTH) 
    d <- d + geom_errorbarh(data=data9,limTEMP,height=0)
    d <- d + scale_colour_manual(name="Size",values=c(rgb.palette(length(unique(data7$LABEL))+1))) + scale_shape_manual(name="Size",values=c(cex_1[1:length(unique(data7$LABEL))]))

    d <- d + geom_path(data=data9,aes(x=TEMPC,y=DEPTHC,group=1),size=0.4,color="gray20")
    d <- d + ggtitle(paste(cn," (",sn,")",sep=""))
    d <- d + ylim(min(data7$DEPTHC-data7$DEPTHSEM),max(data7$DEPTHC+data7$DEPTHSEM))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))

    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
         
     d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
    d <- d + ylab("Centroid depth (m)")
    d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + facet_wrap(~REGI,ncol=3)
    d <- d + theme_minimal(base_size=10)
    print(d)

 
  } 
}
  
 ##location plot 
if(plotT==2) { 

    if(!REG){ 
    
      location$T=1
      location2 <- location[,list(NUMBER=sum(T)),by= 'YEAR,LON,LAT']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,LON,LAT']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,BIN,LABEL']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
  
      location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "0"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("YEAR","BIN","LABEL","LONC","LATC"))
      data7 <- dataL[BIN>=0]
      data7<-data7[order(data7$BIN),]
      data8 <- dataL[BIN<0]
    

      B_sea  <- map_data("world2","USA:alaska")
  

      p <- ggplot() + ggtitle(paste(cn," (",sn,")",sep=""))
    
      if(survey[1]==98){    p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))}
      if(survey[1]==47){    p <- p + coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
      if(survey[1]==52){    p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,205))}
      if(survey[1]==78){    p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))}
      if(length(survey)>1){ p <- p + coord_fixed(ylim=c(51,62),xlim=c(170,230))}

      base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
      map1       <- base_world 
      d          <- map1

     d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=LABEL,shape=LABEL))
     #d <- d + scale_colour_manual(name="Size",values=rgb.palette(length(unique(data7$YEAR))+1))

      location2 <- location[,list(NUMBER=sum(T)),by= 'LON,LAT']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'BIN,LABEL,LON,LAT']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'BIN,LABEL,LON,LAT']
      data6<-data3[,list(LONC=weighted.mean(LON,PLOT),LATC=weighted.mean(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT)),by='BIN,LABEL']
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
      data6$LONSEM <- sqrt(data6$LONVAR)*1.96

     location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "0"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      data6     <- merge(location2,data6,all.x=T,all.y=T,by=c("BIN","LABEL","LONC","LATC"))
      

      data6 <- data6[order(data6$BIN),]
    
      data6$DEPTHC <- data6$DEPTHC*-1
      data9 <- data6[BIN>=0]
      data10 <- data6[BIN<0]
    
     
    #limLAT <- aes(ymax = LATC + LATSEM, ymin=LATC - LATSEM,shape=LABEL,color=LABEL)
    #limLON <- aes(xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=LABEL)

    #d<-  d + geom_point(data=data9,aes(x=TEMPC,y=DEPTHC,shape=LABEL,color=LABEL),size=1.2)
     #d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=factor(REGI),shape=factor(LABEL),size=factor(REGI)))
      d <- d + geom_pointrange(data=data9,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=LABEL,shape=LABEL)) 
      d <- d + geom_errorbarh(data=data9,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=LABEL),height=0)
      d <- d + scale_colour_manual(name="Size",values=c(rgb.palette(length(unique(data7$LABEL))+1))) + scale_shape_manual(name="Size",values=c(cex_1[1:length(unique(data7$LABEL))]))
      
      d <- d + geom_path(data=data9,aes(x=LONC,y=LATC,group=1),size=0.4,color="gray20")
      d <- d + theme(panel.spacing=unit(0,"lines"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.title=element_text(vjust=1,hjust=0))

    if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    d  <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + theme_minimal(base_size=10)
    print(d)

    }

    if(REG){
      location$T=1
      location2 <- location[,list(NUMBER=sum(T)),by= 'YEAR,REGI,LON,LAT']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,REGI,BIN,LABEL,LON,LAT']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,REGI,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","REGI","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,REGI,BIN,LABEL,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,REGI,BIN,LABEL']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
  
      location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "0"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("YEAR","REGI","BIN","LABEL","LONC","LATC"))
      data7 <- dataL[BIN>=0]
      data7<-data7[order(data7$BIN),]
      data8 <- dataL[BIN<0]
    

      B_sea  <- map_data("world2","USA:alaska")
  

      p <- ggplot() + ggtitle(paste(cn," (",sn,")",sep=""))
    
      if(survey[1]==98){    p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))}
      if(survey[1]==47){    p <- p + coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
      if(survey[1]==52){    p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,205))}
      if(survey[1]==78){    p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))}
      if(length(survey)>1){ p <- p + coord_fixed(ylim=c(51,62),xlim=c(170,230))}

      base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
      map1       <- base_world 
      d          <- map1

     d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=LABEL,shape=LABEL))
     #d <- d + scale_colour_manual(name="Size",values=rgb.palette(length(unique(data7$YEAR))+1))

      location2 <- location[,list(NUMBER=sum(T)),by= 'REGI,LON,LAT']
      data1 <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'REGI,BIN,LABEL,LON,LAT']
      data2 <- length[,list(SUM=sum(FREQUENCY)),by = 'REGI,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("REGI","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'REGI,BIN,LABEL,LON,LAT']
      data6<-data3[,list(LONC=weighted.mean(LON,PLOT),LATC=weighted.mean(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT)),by='REGI,BIN,LABEL']
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
      data6$LONSEM <- sqrt(data6$LONVAR)*1.96

     location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "0"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      data6     <- merge(location2,data6,all.x=T,all.y=T,by=c("REGI","BIN","LABEL","LONC","LATC"))
      

      data6 <- data6[order(data6$BIN),]
    
      data6$DEPTHC <- data6$DEPTHC*-1
      data9 <- data6[BIN>=0]
      data10 <- data6[BIN<0]
    
      d <- d + geom_pointrange(data=data9,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=LABEL,shape=LABEL)) 
      d <- d + geom_errorbarh(data=data9,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=LABEL),height=0)
      d <- d + scale_colour_manual(name="Size",values=c(rgb.palette(length(unique(data7$LABEL))+1))) + scale_shape_manual(name="Size",values=c(cex_1[1:length(unique(data7$LABEL))]))
      
      d <- d + geom_path(data=data9,aes(x=LONC,y=LATC,group=1),size=0.4,color="gray20")
      d <- d + theme(panel.spacing=unit(0,"lines"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.title=element_text(vjust=1,hjust=0))

    if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    d  <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d<-d+facet_wrap(~REGI,shrink=T,nrow=3)
    d <- d + theme_minimal(base_size=10)
    print(d)
    } 
  }
}


## example plot 
 #  plot_YEAR_num(data=data1,plotT=2,REG=TRUE,ELLIP=T,p1=0.9)

