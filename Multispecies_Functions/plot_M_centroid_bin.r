## Function for plotting RACE survey data centroids for any species by temperature and depth (plotT=1) and 
## specified length or by location (plotT=2)and specified length for all years comnbined  
## This function takes data pulled using the get_DATA() function.
## This function also requires that the Get_TEMP() function is loaded.
## REG=T seperates the centroids by warm, med, and cold years as determiend by the +-0.5 SE from the mean for all surveys examined
## PATH = T plots a line between points ordered by length bin
## colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme
## Example plots:
## plot_M_centroid_bin(data=dataM,REG=T,plotT=1,colx=1,SINGLE=F,SUB=c(1:20))
## plot_M_centroid_bin(data=dataM,REG=T,plotT=2,PATH=T,colx=2)

plot_M_centroid_bin <- function(data=datM,plotT=1,REG=T,colx=1,SINGLE=T,SUB=c(1:20)){
  require(data.table)
  require(ggplot2)
  require(grid)
  require(ggmap)
  require(stringr)

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

  if(colx==2){rgb.palette <- colorRampPalette(c("brown4","red","orange","green","turquoise","blue"),space = "rgb")}
  if(colx==1){rgb.palette <- colorRampPalette(c("#fecc5c","#fd8d3c","#f03b20","#bd0026"),space = "rgb")}
  if(colx==3){rgb.palette <- colorRampPalette(c("gray70","Black"),space = "rgb")}
 
  location   <- data$location
  length    <- data$length
   
  survey    <- unique(length$SURVEY_DEFINITION_ID)
  sn        <- data.table(data$SN)
  sn        <- sn[order(COMMON_NAME),]

  sn<-sn[SUB,]
  length<-length[SPECIES_CODE %in% sn$SPECIES_CODE]
  
  species   <- unique(length$SPECIES_CODE)
  ns<-length(species)
 



  tit<-vector("list",length=length(species))

  for(i in 1:length(species)){
    tit[[i]]<-paste(as.character(sn$COMMON_NAME[i])," (",sn$SPECIES_NAME[i],")",sep="")
  }

  Title<-do.call(paste,c(tit,sep=" and "))
  Title<-str_wrap(Title, width = 80)


  bins<-vector("list",length=length(species))
 
  
  for(i in 1:length(species)){
    bins[[i]]<-unique(length$BIN[length$SPECIES_CODE==species[i]])
  }

  loca <- location[!is.na(location$TEMP)& !is.na(location$STRATUM)]
  loca <- subset(loca,loca$STRATUM<800)

    
  MTEMP <- Get_TEMP(data=data,plotT=F)

  Location <- merge(loca,MTEMP,by=c("YEAR"),all=T)
  length<-length[!is.na(length$TEMPR)]
  Length   <- merge(length,MTEMP,by=c("YEAR"),all=T)

  cex_1<-c(16,15,17,18,21,8,9,24,22,1,5,2,12,25,3,6,7,11,12,13,25) 

  colors2<-c("blue4","red4","blue2","red2","blue","red","dodgerblue","deeppink","deepskyblue","hotpink","lightblue","pink") 
 ## Temperature plot
  if(plotT==1){

     
    if(!REG){
      Location$T<-1
  
      #location1 <- Location[,list(NUMBER=sum(T)),by= 'DEPTHR,TEMPR']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'BIN,LABEL,LABEL2,DEPTHR,TEMPR,SPECIES_CODE']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'DEPTHR,TEMPR,SPECIES_CODE']
      data3 <- merge(data1,data2,all=T,by=c("DEPTHR","TEMPR","SPECIES_CODE"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'SPECIES_CODE,BIN,LABEL,LABEL2,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96
      data6 <- data6[order(data6$SPECIES_CODE,data6$BIN),]
      data6$DEPTHC <- data6$DEPTHC*-1
      data7 <- data6[BIN>=0]
      data7<-merge(data7,sn,by="SPECIES_CODE")
      data8<-vector("list",length=length(species))
    

      for(i in 1:length(species)){
        data8[[i]]<-data7[SPECIES_CODE==species[i]]
        }

      nam1<-paste("A",1:length(species),sep="")
      names(data8)<-nam1
      list2env(data8,envir=.GlobalEnv)
    
      limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
      limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
        
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=COMMON_NAME,shape=COMMON_NAME,size=factor(LABEL2)))
      d <- d + geom_path(data=data7,aes(x=TEMPC,y=DEPTHC,group=COMMON_NAME,color=COMMON_NAME),size=0.5) 
      for( i in 1: length(nam1)){
        dataQ<-get(paste("A",i,sep=""))  
        d <- d + geom_pointrange(data=dataQ,limDEPTH) 
        d <- d + geom_errorbarh(data=dataQ,limTEMP,height=0)
        }

      d <- d + scale_colour_manual(name="Species",values=c(rgb.palette(ns)))+scale_shape_manual(name="Species",values=cex_1[1:ns])
       d <- d + scale_size_manual(name="Size bin",values=seq(0,10,length=length(data7$LABEL2)))
      
      d <- d + ylim(min(data7$DEPTHC-data7$DEPTHSEM),max(data7$DEPTHC+data7$DEPTHSEM))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))
      d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
      #d <- d + ggtitle(paste(Title,sep=""))   
      d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
      d <- d + ylab("Centroid depth (m)")
      d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1),ncol=2))
      d <- d + theme_minimal(base_size=10)
      print(d)
      }

    if(REG){

      Location$T<-1
  
      location1 <- Location[,list(NUMBER=sum(T)),by= 'DEPTHR,TEMPR']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'REGI,BIN,LABEL,LABEL2,DEPTHR,TEMPR,SPECIES_CODE']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'REGI,DEPTHR,TEMPR,SPECIES_CODE']
      data3 <- merge(data1,data2,all=T,by=c("REGI","DEPTHR","TEMPR","SPECIES_CODE"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'REGI,SPECIES_CODE,BIN,LABEL,LABEL2,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='REGI,SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96
      data6 <- data6[order(data6$SPECIES_CODE,data6$BIN),]
      data6$DEPTHC <- data6$DEPTHC*-1
      data7 <- data6[BIN>=0]
      data7<-merge(data7,sn,by="SPECIES_CODE")
      data8<-vector("list",length=length(species))
    
      for(i in 1:length(species)){
        data8[[i]]<-data7[SPECIES_CODE==species[i]]
        }

      nam1<-paste("A",1:length(species),sep="")
      names(data8)<-nam1
      list2env(data8,envir=.GlobalEnv)
    
      limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
      limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
        
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=COMMON_NAME,shape=COMMON_NAME,size=factor(LABEL2)))
      d <- d + geom_path(data=data7,aes(x=TEMPC,y=DEPTHC,group=COMMON_NAME,color=COMMON_NAME),size=0.5) 
      for( i in 1: length(nam1)){
        dataQ<-get(paste("A",i,sep=""))  
        d <- d + geom_pointrange(data=dataQ,limDEPTH) 
        d <- d + geom_errorbarh(data=dataQ,limTEMP,height=0)
        }

      d <- d + scale_colour_manual(name="Species",values=c(rgb.palette(ns)))+scale_shape_manual(name="Species",values=cex_1[1:ns])
      d <- d+scale_size_manual(name="Size bin",values=seq(0,15,length=length(data7$LABEL2)))
      
      d <- d + ylim(min(data7$DEPTHC-data7$DEPTHSEM),max(data7$DEPTHC+data7$DEPTHSEM))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))
      d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
      #d <- d + ggtitle(paste(Title,sep=""))   
      d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
      d <- d + ylab("Centroid depth (m)")
      d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1),ncol=2))
      d <- d + theme_minimal(base_size=10)
      d <- d + facet_wrap(~REGI,ncol=3,shrink=FALSE)
      print(d)
      }
    }
 
 ##location plot 
  if(plotT==2) { 
  
    #if(survey[1]==98){ bering<-get_map(location=c(lon=-170,lat=58),zoom=5,maptype="satellite",color="bw")}

    #Length$LON[Length$LON<180]<-Length$LON[Length$LON<180]
    #Length$LON[Length$LON > 180] <- Length$LON[Length$LON>180]-360
    
    #d<-ggmap(bering)

    B_sea <- map_data("world2","USA:alaska")
    p <- ggplot() 
         
    if(survey[1]==98){  p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))}
    if(survey[1]==47){  p <- p + coord_fixed(ylim=c(50,61.5),xlim=c(185,230))}
    if(survey[1]==52){  p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,205))}
    if(survey[1]==78){  p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))}
    base_world <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
    map1    <- base_world 
    d <- map1


    if(!REG){
      #Location$T=1
      #location2 <- Location[,list(NUMBER=sum(T)),by= 'LON,LAT']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'SPECIES_CODE,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("SPECIES_CODE","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
   
  
      #location2$LONC <- location2$LON
      #location2$LATC <- location2$LAT
      #location2$BIN <- -1
      #location2$LABEL <- "NA"
      #location2$LABEL2 <- "NA"
      #location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      #dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("BIN","LABEL","LABEL2","LONC","LATC"))
      data7 <- data6 #[BIN>=0]
      data7<-data7[order(data7$BIN),]
      
      data7<-merge(data7,sn,by="SPECIES_CODE")
      data8<-vector("list",length=length(species))
    
      for(i in 1:length(species)){
        data8[[i]]<-data7[SPECIES_CODE==species[i]]
        }

      nam1<-paste("A",1:length(species),sep="")
      names(data8)<-nam1
      list2env(data8,envir=.GlobalEnv)
      d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMON_NAME,color=COMMON_NAME),size=0.5) 

      for( i in 1: length(nam1)){
        dataQ<-get(paste("A",i,sep=""))  
        d <- d + geom_pointrange(data=dataQ,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,shape=COMMON_NAME,color=COMMON_NAME,size=factor(LABEL2))) 
        d <- d + geom_errorbarh(data=dataQ,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=COMMON_NAME),height=0)
        }
      
      
      d <- d + scale_colour_manual(name="Species",values=c(rgb.palette(ns)))+scale_size_manual(name="Size bin",values=seq(0.2,5,length=length(data7$LABEL2)))
      d<-d+scale_shape_manual(name="Species",values=cex_1[1:ns])
      }
    
    if(REG){
      #Location$T=1
      #location2 <- Location[,list(NUMBER=sum(T)),by= 'REGI,LON,LAT']
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'REGI,SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'REGI,SPECIES_CODE,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("REGI","SPECIES_CODE","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'REGI,SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='REGI,SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
     
      #location2$LONC <- location2$LON
      #location2$LATC <- location2$LAT
      #location2$BIN <- -1
      #location2$LABEL <- "NA"
      #location2 <-subset(location2,select=-c(LON,LAT,NUMBER))

      #dataL     <- merge(location2,data6,all.x=T,all.y=T,by=c("REGI","BIN","LABEL","LONC","LATC"))
      data7 <- data6 #[BIN>=0]
      data7<-merge(data7,sn,by="SPECIES_CODE")

      if(!SINGLE){ 
        data8<-vector("list",length=length(species))
    
        for(i in 1:length(species)){
          data8[[i]]<-data7[SPECIES_CODE==species[i]]
          }

        nam1<-paste("A",1:length(species),sep="")
        names(data8)<-nam1
        list2env(data8,envir=.GlobalEnv)

         d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMON_NAME,color=COMMON_NAME),size=0.8) 
        for( i in 1: length(nam1)){
          dataQ<-get(paste("A",i,sep=""))  
          d <- d + geom_pointrange(data=dataQ,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,shape=COMMON_NAME,color=COMMON_NAME,size=factor(LABEL2))) 
          d <- d + geom_errorbarh(data=dataQ,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=COMMON_NAME),height=0)
          }

       
        d <- d + scale_colour_manual(name="Species",values=c(rgb.palette(ns)))+scale_size_manual(name="Size bin",values=seq(0,10,length=length(data7$LABEL2)))
        d<- d + scale_shape_manual(name="Species",values=cex_1[1:ns])
        d <- d + facet_wrap(~REGI,ncol=1,shrink=FALSE)
        }
    
    if(SINGLE){

     
      data7$COMMONR<-paste(data7$COMMON_NAME,data7$REGI,sep="-")
      data7<-data7[order(COMMONR,BIN),]
      data7<-data7[REGI!="MED"]

      data8<-vector("list",length=length(species))
    
      for(i in 1:length(species)){
        data8[[i]]<-data7[SPECIES_CODE==species[i]]
        }

      nam1<-paste("A",1:length(species),sep="")
      names(data8)<-nam1
      list2env(data8,envir=.GlobalEnv)

      d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMONR),color="gray80",size=0.4) 
      for( i in 1: length(nam1)){
        dataQ<-get(paste("A",i,sep=""))  
        d <- d + geom_pointrange(data=dataQ,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=REGI,shape=COMMON_NAME,size=factor(LABEL2))) 
        d <- d + geom_errorbarh(data=dataQ,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=REGI),height=0,size=0.5)
        }

      d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMONR,color=REGI),size=0.5) 
      d <- d + scale_shape_manual(name="Species",values=cex_1[1:length(species)])+scale_size_manual(name="Size bin",values=seq(0.2,5,length=length(data7$LABEL2)))
      
      if(colx!=3){d <- d + scale_colour_manual(name="Regime",values=c("blue","red"))}
      if(colx==3){d <- d + scale_colour_manual(name="Regime",values=c("gray70","black"))}
    }  
  }  

    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))

    d <- d + theme_minimal(base_size=10)
    #d <- d + ggtitle(paste(Title,sep="")) 

    d <- d + theme(panel.spacing=unit(0,"lines"),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      plot.title=element_text(vjust=1,hjust=0))

    #if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }

    #if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,230,by=10),labels=seq(170,130,by=-10)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    #if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,200,by=10),labels=c(170,180,170,160)) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    #if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
  
    d  <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1),ncol=2))
    
    print(d)

    } 
}