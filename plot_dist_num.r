## Created by Steve Barbeaux 1/30/2017 
## Function for plotting data by temperature (plotT=1) or location (plotT=2)and specified length 
## Uses data from Get_DATA() function and requires the Get_TEMP() function
## Plots individual bins specified when data was retrieved. 
## example plots
## plot_dist_num(data=data1,bin=1,plotT=1)
## plot_dist_num(data=data1,bin=1,plotT=2)


plot_dist_num<- function(data=data_COD,bin=1,plotT=1){
  require(mgcv)
  require(data.table)
  require(ggplot2)
  require(grid)
  
  td1 <- Get_TEMP(data=data, plotT=F)
  location   <- data.table(data$location)
  length    <- data.table(data$length)
  location_poll <- data.table(data$location_poll)
 
  survey    <- unique(length$SURVEY_DEFINITION_ID)
  species    <- unique(length$SPECIES_CODE)
  bins     <- sort(unique(length$BIN))
  cn      <- data$SN$COMMON_NAME
  sn      <- data$SN$SPECIES_NAME

 
  Max_L <-max(length$LENGTH)
  Lbin<-bin
  
  if(length(Lbin)<2){
    Lbin<-c(bins[bin],bins[bin+1])
    if(is.na(Lbin[2])){Lbin[2]=Max_L}
    }
    
  rgb.palette <- colorRampPalette(c("yellow","gold","goldenrod2","brown"),space = "rgb")
  
  length<-length[!is.na(CPUE)]
  length<-length[!is.na(length$TEMPR)]
  length<-length[!is.na(length$STEMP)]
  
## Temperature plot
  if(plotT==1) { 
## rounding depth to 20 m increments for visualization purposes.
    location$DEPTHR <- round(location$DEPTH/2,-1)*2
    length$DEPTHR  <- round(length$DEPTH/2,-1)*2

    location<-location[!is.na(TEMPR)]
    location<-location[!is.na(STEMP)]
    location<-location[!is.na(DEPTHR)]

    location$T<-1
    location1  <- location[,list(NUMBER=sum(T)),by= 'YEAR,DEPTHR,TEMPR']
    data1      <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR']
    data2      <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,DEPTHR,TEMPR']
    data3      <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR"))
    data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR']
    bins2      <- seq(0,max(log(data3$PLOT)),length=6)
  
    for( j in 2:6){
      data3$bin2[log(data3$PLOT)>bins2[j-1]&log(data3$PLOT)<=bins2[j]]<-round(exp(bins2[(j-1)]))
      }
 
    dataT <- data3[data3$BIN>=Lbin[1]&data3$BIN<Lbin[2]]

    dataT <- merge(location1,dataT,all.x=T,by=c("YEAR","DEPTHR","TEMPR"))

    dataT$PLOT[is.na(dataT$PLOT==T)] <- 0
    dataT$bin2[is.na(dataT$bin2==T)] <- 0
    dataT <- dataT[order(dataT$bin2),]

    label2<-array(dim=length(unique(dataT$bin2)))
   
  binsx<-c(0,sort(unique(data3$bin2)))
  lab1<-c(binsx,round(max(data3$PLOT)))
  lab2<-lab1+1
  lab2[1]<-"0"
  label2[1]<-"0"
  label2[2]<-paste(lab1[2],"-",lab1[3],sep="")

   for(i in 4:length(lab1)){
        label2[i-1]<-paste(as.character(lab2)[i-1],"-",as.character(lab1)[i],sep="")
      }

#label2<-as.factor(label2)
#label2<-ordered(label2,levels=unique(label2))

   label2<-data.frame(bin2=binsx,LABEL2=factor(label2))
   label2$ID<-as.numeric(factor(label2$LABEL2)) 
   dataT<-merge(dataT,label2,by="bin2")

    nc<-min(5,trunc(length(unique(data3$YEAR))/4)+1)
    td1$TEMPR=1;td1$DEPTHR=1; td1$bin2=1;td1$LABEL2="0"

    d <- ggplot(dataT,aes(x=TEMPR,y=-DEPTHR,color=LABEL2, shape=LABEL2,size=LABEL2))
    d <- d + geom_rect(data=td1,aes(fill=REGI),xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.2,linetype=0)
    d <- d + geom_point()
    d <- d + scale_fill_manual(name="Regime",values=c("light blue","white","salmon"))
    d <- d + scale_color_manual(name=expression(paste("Number/k",m^2,sep="")),values=c("gray80",rgb.palette(6)[label2$ID[2:nrow(label2)]]),breaks=label2$LABEL2)+scale_shape_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(3,16,16,16,16,16),breaks=label2$LABEL2)+scale_size_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(0.4,rep(0.75,5)),breaks=label2$LABEL2)
    d <- d + ggtitle(paste(cn," (",sn,") ",unique(dataT$LABEL)[2],sep=""))
    d <- d + xlab(expression("Temperature ("* degree * C *")"))
    d <- d + ylab("Depth (m)")
    d<- d+scale_x_continuous(breaks=seq(-1,20,by=1))
    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', color = 'white'),
                   plot.title=element_text(vjust=1,hjust=0),
                   legend.key=element_rect(fill = 'white', color = 'white',linetype=0),
                   legend.key.width=unit(1,"cm"),
                   axis.text=element_text(size=6))
        d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))

    d <- d + facet_wrap(~YEAR,ncol=nc,shrink=FALSE)
    d<-d+theme_minimal(base_size=10)
    print(d)

    #d<-facetAdjust(d)
  
 } 
 
## Location plot 
 if(plotT==2) { 


    B_sea <- map_data("world2","USA:alaska")
    

    p <- ggplot()
         
    if(survey[1]==98){  p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))}

    if(survey[1]==47){  p <- p + coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
  
    if(survey[1]==52){  p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,205))}
  
    if(survey[1]==78){  p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))}



    base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
    map1    <- base_world 
    d <- map1
  
  
  location2  <- location[,list(NUMBER=length(YEAR)),by= 'YEAR,LON,LAT']
  data1      <- length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,BIN,LABEL,LON,LAT']
  data2      <- length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,LON,LAT']
  data3      <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT"))
  data3      <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,LON,LAT']
  bins2      <- seq(min(log(data3$PLOT-1)),max(log(data3$PLOT)),length=6)
 
  for( j in 2:6){
   data3$bin2[log(data3$PLOT)>bins2[j-1]&log(data3$PLOT)<=bins2[j]]<- round(exp(bins2[(j-1)]))
   }
 
  dataL   <- subset(data3,data3$BIN>=Lbin[1]&data3$BIN<Lbin[2])
  dataL   <- merge(location2,dataL,all.x=T,by=c("YEAR","LON","LAT"))

  dataL$PLOT[is.na(dataL$PLOT==T)] <- 0
  dataL$bin2[is.na(dataL$bin2==T)] <- 0
  
  dataL   <- dataL[order(dataL$bin2),]

  binsx<-c(0,sort(unique(data3$bin2)))
  label2<-array(dim=length(binsx))
  lab1<-c(binsx,round(max(data3$PLOT)))
  lab2<-lab1+1
  lab2[1]<-"0"
  label2[1]<-"0"
  label2[2]<-paste(1,"-",lab1[3],sep="")

   for(i in 4:length(lab1)){
        label2[i-1]<-paste(as.character(lab2)[i-1],"-",as.character(lab1)[i],sep="")
      }

   label2<-data.frame(bin2=binsx,LABEL2=factor(label2))
   label2$ID<-as.numeric(factor(label2$LABEL2)) 
   dataL<-merge(dataL,label2,by="bin2")

  
    td1$LON=1;td1$LAT=1; td1$bin2=1;td1$LABEL2="0"
    nc<-min(5,trunc(length(unique(data3$YEAR))/4)+1)
    d <- d + geom_point(data=dataL,aes(x=LON,y=LAT,color=LABEL2,shape=LABEL2,size=LABEL2))
    d <- d + geom_rect(data=td1,aes(fill=REGI),xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.2,linetype=0)
    d <- d + scale_fill_manual(name="Regime",values=c("light blue","white","salmon"))
    d <- d + scale_color_manual(name=expression(paste("Number/k",m^2,sep="")),values=c("gray80",rgb.palette(6)[label2$ID[2:nrow(label2)]]),breaks=label2$LABEL2)+scale_shape_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(3,16,16,16,16,16),breaks=label2$LABEL2)+scale_size_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(0.4,rep(0.6,5)),breaks=label2$LABEL2)
    d <- d + ggtitle(paste(cn," (",sn,") ",unique(dataL$LABEL)[2],sep=""))
    
    if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + facet_wrap(~YEAR,ncol=nc,shrink=FALSE)
    d <- d + theme_minimal(base_size=10)
    print(d)
    #d <- facetAdjust(d)

  }
}
