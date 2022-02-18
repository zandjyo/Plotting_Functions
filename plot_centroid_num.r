## Function for plotting RACE survey data for any species by temperature and depth (plotT=1) and specified length or by location (plotT=2)and specified length  
## This function takes data pulled using the get_DATA() function.
## REG = T plots data by cold,med, and warm temperture regime, REF=F plots it by year
## PATH = T plots a line between points ordered by year
## ELLIP = T plots a modified Fox and Weisberg (2011) 
## method for calculating the ellipses (ELLIP=TRUE)with a confidence level set at p1.
## colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme
## Confidence intervals are based on 1.96*SE
## Example plots:
## plot_centroid_num(data=data1,REG=T,plotT=1,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)
## plot_centroid_num(data=data_COD,REG=T,plotT=2,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)

plot_centroid_num <- function(data=data1,plotT=1,REG=T,PATH=F,ELLIP=F,p1=0.90,colx=1,thin=F){
  require(data.table)
  require(ggplot2)
  require(grid)
  require(ggmap)

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

  if(colx==2){rgb.palette <- colorRampPalette(c("red","orange","yellow","green","turquoise","blue"),space = "rgb")}
  if(colx==1){rgb.palette <- colorRampPalette(c("#fecc5c","#fd8d3c","#f03b20","#bd0026"),space = "rgb")}
 
  location   <- data$location
  length    <- data$length
  location_poll <- data$location_poll
 
  survey    <- unique(length$SURVEY_DEFINITION_ID)
  species    <- unique(length$SPECIES_CODE)
  bins     <- sort(unique(length$BIN))
  cn      <- data$SN$COMMON_NAME
  sn      <- data$SN$SPECIES_NAME

  loca <- location[!is.na(location$TEMP)& !is.na(location$STRATUM)]
  loca <- subset(loca,loca$STRATUM<800)

    
  MTEMP <- Get_TEMP(data=data,plotT=F)

  Location <- merge(loca,MTEMP,by=c("YEAR"),all=T)
  length<-length[!is.na(length$TEMPR)]
  Length   <- merge(length,MTEMP,by=c("YEAR"),all=T)

    
 ## Temperature plot
  if(plotT==1){

    Location$T<-1
    location1 <- Location[,list(NUMBER=sum(T)),by= 'YEAR,DEPTHR,TEMPR,REGI']
    data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR,REGI']
    data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,DEPTHR,TEMPR,REGI']
    data3 <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR","REGI"))
    data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR,REGI']
    data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,REGI,BIN,LABEL']
    data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
    data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

    location1$TEMPC <- location1$TEMP
    location1$DEPTHC <- location1$DEPTH
    location1$BIN <- -1
    location1$LABEL <- "NA"
    location1<-subset(location1,select=-c(DEPTHR,TEMPR))
    data6 <- merge(location1,data6,all=T, by=c("YEAR","REGI","BIN","LABEL","DEPTHC","TEMPC"))
    data6 <- data6[order(data6$BIN,data6$YEAR),]
    data6$YEAR <- as.factor(data6$YEAR)
    data6$DEPTHC <- data6$DEPTHC*-1
    data7 <- data6[BIN>=0]
    data8 <- data6[BIN<0]
    
    # exclulde the min and max size bins, delete the second-max bins that have fewer samples    
    if(thin==T){ data7 <- data6[BIN>min(bins) & BIN<max(bins)] }
 
   
    if(!REG){

       Location$T<-1
      location1 <- Location[,list(NUMBER=sum(T)),by= 'YEAR,DEPTHR,TEMPR']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,DEPTHR,TEMPR']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,BIN,LABEL']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

      location1$TEMPC <- location1$TEMP
      location1$DEPTHC <- location1$DEPTH
      location1$BIN <- -1
      location1$LABEL <- "NA"
      location1<-subset(location1,select=-c(DEPTHR,TEMPR))
      data6 <- merge(location1,data6,all=T, by=c("YEAR","BIN","LABEL","DEPTHC","TEMPC"))
      data6 <- data6[order(data6$BIN,data6$YEAR),]
      data6$YEAR <- as.factor(data6$YEAR)
      data6$DEPTHC <- data6$DEPTHC*-1
      data7 <- data6[BIN>=0]
      data8 <- data6[BIN<0]
    
    # exclulde the min and max size bins, delete the second-max bins that have fewer samples    
      if(thin==T){ data7 <- data6[BIN>min(bins) & BIN<max(bins)] }
 
    
      limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
      limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=YEAR),size=0.35)

      d <- d + geom_pointrange(limDEPTH,size=0.35) 
      d <- d + geom_errorbarh(limTEMP,size=0.35)
      d <- d + scale_colour_manual(name="Year",values=c(rgb.palette(length(unique(data7$YEAR))+1)))
      }

    if(REG){
      Location$T<-1
      location1 <- Location[,list(NUMBER=sum(T)),by= 'YEAR,DEPTHR,TEMPR,REGI']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR,REGI']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,DEPTHR,TEMPR,REGI']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR","REGI"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR,REGI']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,REGI,BIN,LABEL']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

      location1$TEMPC <- location1$TEMP
      location1$DEPTHC <- location1$DEPTH
      location1$BIN <- -1
      location1$LABEL <- "NA"
      location1<-subset(location1,select=-c(DEPTHR,TEMPR))
      data6 <- merge(location1,data6,all=T, by=c("YEAR","REGI","BIN","LABEL","DEPTHC","TEMPC"))
      data6 <- data6[order(data6$BIN,data6$YEAR),]
      data6$YEAR <- as.factor(data6$YEAR)
      data6$DEPTHC <- data6$DEPTHC*-1
      data7 <- data6[BIN>=0]
      data8 <- data6[BIN<0]
    
    # exclulde the min and max size bins, delete the second-max bins that have fewer samples    
    if(thin==T){ data7 <- data6[BIN>min(bins) & BIN<max(bins)] }
 
    
    limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
    limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
      
    




      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=REGI, shape=REGI,size=REGI))
      d <- d + scale_colour_manual(name="Regime",values=c("gray50","gray80","black"))+scale_shape_manual(name="Regime",values=c(16,17,15))+scale_size_manual(name="Regime",values=c(0.35,0.35,0.35))
      d <- d + geom_pointrange(limDEPTH) 
      d <- d + geom_errorbarh(limTEMP)
    }


    d <- d + ggtitle(paste(cn," (",sn,")",sep=""))
    d <- d + ylim(min(min(data7$DEPTHC)-max(data7$DEPTHSEM)),max(max(data7$DEPTHC)+max(data7$DEPTHSEM)))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))
    #d <- d + ylim(min(-250),max(0))+xlim(min(-10),max(10))

    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
    
    if(PATH) { d <- d + geom_path(aes(group=LABEL,order=YEAR),color="gray90")}
    
    d <- d + geom_point(size=2,shape=16)
    d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
    d <- d + ylab("Centroid depth (m)")
    d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + facet_wrap(~LABEL,shrink=FALSE)
    print(d)
  }
 
 ##location plot 
  else { 

     B_sea <- map_data("world2","USA:alaska")
  
    

    p <- ggplot() + ggtitle(paste(cn," (",sn,")",sep=""))
         
    if(survey[1]==98){  p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))}

    if(survey[1]==47){  p <- p + coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
  
    if(survey[1]==52){  p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,205))}
  
    if(survey[1]==78){  p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))}

  
    base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
    map1    <- base_world 
    d <- map1
    
    
    if(!REG){
      Location$T=1
      location2 <- Location[,list(NUMBER=sum(T)),by= 'YEAR,LON,LAT']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,BIN,LABEL']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
  
      location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "NA"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("YEAR","BIN","LABEL","LONC","LATC"))
      dataL$YEAR <- as.factor(dataL$YEAR)
    
     
      data7 <- dataL[BIN>=0]
      if(thin==T){ data7 <- dataL[BIN > min(bins) & BIN < max(bins)]} 
      data8 <- dataL[BIN<0]

      data7<-data7[order(data7$YEAR,data7$BIN)]
      limLAT <- aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM)
      limLON <- aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM)

   
      d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=YEAR),size=0.35)
      d <- d + geom_pointrange(data=data7,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=YEAR),size=0.3) 
      d <- d + geom_errorbarh(data=data7,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=YEAR),size=0.3)
      d <- d + scale_colour_manual(name="Year",values=rgb.palette(length(unique(data7$YEAR))+1))
      if(PATH){ d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=1),size=0.2,color="gray50")}
      }
    
    if(REG){
      Location$T=1
      location2 <- Location[,list(NUMBER=sum(T)),by= 'YEAR,REGI,LON,LAT']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,REGI,BIN,LABEL,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,REGI,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","REGI","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,REGI,BIN,LABEL,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,REGI,BIN,LABEL']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
  
      location2$LONC <- location2$LON
      location2$LATC <- location2$LAT
      location2$BIN <- -1
      location2$LABEL <- "NA"
      location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("YEAR","REGI","BIN","LABEL","LONC","LATC"))
      dataL$YEAR <- as.factor(dataL$YEAR)
    
      B_sea <- map_data("world2","USA:alaska")
      
      data7 <- dataL[BIN>=0]
      
      if(thin==T){ data7 <- dataL[BIN > min(bins) & BIN < max(bins)]}

      data8 <- dataL[BIN<0]
      data7<-data7[order(data7$REGI,data7$YEAR,data7$BIN)]
      
      limLAT <- aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM)
      limLON <- aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM)

      d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=REGI,shape=factor(REGI),size=factor(REGI)))
      d <- d + geom_pointrange(data=data7,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=REGI,shape=REGI,size=REGI)) 
      d <- d + geom_errorbarh(data=data7,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=REGI,shape=REGI,size=REGI))
      d <- d + scale_colour_manual(name="Regime",values=c("blue","gray80","red"))+scale_size_manual(name="Regime",values=c(0.3,0.3,0.3))+scale_shape_manual(name="Regime",values=c(16,17,15))
      if(PATH){ d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=REGI),size=0.2,color="gray50")}
      
      if (ELLIP){ d <- d + stat_ellipse(data=data7,aes(x=LONC,y=LATC,group=REGI,color=REGI),level=p1)}

      }   

    
    nc<-max(1,round((length(unique(data7$LABEL))/3)))
    d <- d + theme(panel.spacing=unit(0,"lines"),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      plot.title=element_text(vjust=1,hjust=0))

    if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
   
    d  <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + facet_wrap(~LABEL,ncol=nc,shrink=TRUE)
    print(d)

    } 
}