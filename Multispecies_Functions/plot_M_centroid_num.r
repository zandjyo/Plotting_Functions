## Function for plotting RACE survey data for any species by temperature and depth (plotT=1) and specified length or by location (plotT=2)and specified length  
## This function takes data pulled using the get_DATA() function.
## REG = T plots data by cold,med, and warm temperture regime, REF=F plots it by year
## PATH = T plots a line between points ordered by year
## ELLIP = T plots a modified Fox and Weisberg (2011) 
## method for calculating the ellipses (ELLIP=TRUE)with a confidence level set at p1.
## colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme
## Confidence intervals are based on 1.96*SE
## Example plots:
## plot_M_centroid_num(data=dataM,REG=T,plotT=2,PATH=F,ELLIP=F,p1=0.95,colx=1)

plot_M_centroid_num <- function(data=data1,plotT=1,REG=T,PATH=F,ELLIP=F,p1=0.90,colx=1,SUB=c(1:20)){
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
  if(colx==3){rgb.palette <- colorRampPalette(c("gray90","Black"),space = "rgb")}
 
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

      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,DEPTHR,TEMPR']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,DEPTHR,TEMPR']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","DEPTHR","TEMPR"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMPR,DEPTHR']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96

      data6 <- data6[order(data6$SPECIES_CODE,data6$BIN,data6$YEAR),]
      data6$YEAR <- as.factor(data6$YEAR)
      data6$DEPTHC <- data6$DEPTHC*-1
      data6<-merge(data6,sn,by="SPECIES_CODE")
      data7 <- data6[BIN>=0]
           
      limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
      limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=YEAR,size=LABEL2,shape=COMMON_NAME),size=0.35)

      d <- d + geom_pointrange(limDEPTH,size=0.35) 
      d <- d + geom_errorbarh(limTEMP,size=0.35)
      d <- d + scale_colour_manual(name="Year",values=c(rgb.palette(length(unique(data7$YEAR))+1)))+scale_shape_manual(name="Species",values=cex_1[1:ns])
      d <- d + scale_size_manual(name="Size",values=seq(0.2,5,length=length(unique(data7$LABEL2))))
      d <- d + geom_path(aes(group=LABEL2),size=0.5)


      }

    if(REG){
   

      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,DEPTHR,TEMPR,REGI']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,DEPTHR,TEMPR,REGI']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","DEPTHR","TEMPR","REGI"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMPR,DEPTHR,REGI']
      data6<-data3[,list(DEPTHC=weighted.mean(DEPTHR,PLOT),TEMPC=weighted.mean(TEMPR,PLOT),TEMPVAR=weighted.var.se(TEMPR,PLOT),DEPTHVAR=weighted.var.se(DEPTHR,PLOT)),by='YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
      data6$DEPTHSEM <- sqrt(data6$DEPTHVAR)*1.96
      data6$TEMPSEM <- sqrt(data6$TEMPVAR)*1.96


      data6 <- data6[order(data6$SPECIES_CODE,data6$REGI,data6$BIN,data6$YEAR),]
      data6$YEAR <- as.factor(data6$YEAR)
      data6$DEPTHC <- data6$DEPTHC*-1
      #data6$LABR<-paste(data6$LAB,data6$REGI,sep="-")
      data6<-merge(data6,sn,by="SPECIES_CODE")
      data7 <- data6[BIN>=0]
        
      limDEPTH <- aes(ymax = DEPTHC + DEPTHSEM, ymin=DEPTHC - DEPTHSEM)
      limTEMP <- aes(xmax = TEMPC + TEMPSEM, xmin=TEMPC - TEMPSEM)
      
      d <- ggplot(data7,aes(x=TEMPC,y=DEPTHC,color=REGI, shape=COMMON_NAME))
      d <- d + geom_pointrange(limDEPTH) 
      d <- d + geom_errorbarh(limTEMP)
      d <- d + scale_colour_manual(name="Regime",values=c("blue","gray80","red"))+scale_shape_manual(name="Species",values=cex_1[1:ns])
      d <- d + geom_path(aes(group=REGI),size=0.5)
    }

    d <- d + ylim(min(min(data7$DEPTHC)-max(data7$DEPTHSEM)),max(max(data7$DEPTHC)+max(data7$DEPTHSEM)))+xlim(min(data7$TEMPC-data7$TEMPSEM),max(data7$TEMPC+data7$TEMPSEM))
 
    d <- d + theme(panel.spacing=unit(0,"lines"),
                   panel.background = element_rect(fill = 'white', colour = 'white'),
                   plot.title=element_text(vjust=1,hjust=0))
    
    d <- d + xlab(expression("Centroid temperature ("* degree * C *")"))
    d <- d + ylab("Centroid depth (m)")
    d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
    d <- d + facet_wrap(COMMON_NAME~LABEL2,shrink=FALSE,ncol=length(unique(data7$LABEL2)))
    print(d)
  }
 
 ##location plot 
  else { 

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
      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
      data6<-merge(data6,sn,by="SPECIES_CODE")
   
      data7<-data6[order(data6$YEAR,data6$SPECIES_CODE,data6$BIN)]



      limLAT <- aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM)
      limLON <- aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM)

   
      d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=factor(YEAR),shape=COMMON_NAME))
      d <- d + geom_pointrange(data=data7,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=factor(YEAR))) 
      d <- d + geom_errorbarh(data=data7,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=factor(YEAR)))
      d <- d + scale_colour_manual(name="Year",values=rgb.palette(length(unique(data7$YEAR))+1))+scale_shape_manual(name="Species",values=cex_1[1:ns])
      d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMON_NAME,order=YEAR),size=0.5,color="gray80")
      }
    
    if(REG){

      data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,LON,LAT']
      data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,REGI,LON,LAT']
      data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","REGI","LON","LAT"))
      data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,LON,LAT']
      data6<-data3[,list(LATC=weighted.mean(LAT,PLOT),LONC=weighted.mean(LON,PLOT),LATVAR=weighted.var.se(LAT,PLOT),LONVAR=weighted.var.se(LON,PLOT)),by='YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
      data6$LONSEM <-sqrt(data6$LONVAR)*1.96
      data6$LATSEM <- sqrt(data6$LATVAR)*1.96
      data6<-merge(data6,sn,by="SPECIES_CODE")
      data6$SPeR<-paste(data6$COMMON_NAME,data6$REGI,sep="-")
      data6<-subset(data6,data6$REGI!="MED")

      data7<-data6[order(data6$REGI,data6$COMMON_NAME,data6$YEAR,data6$BIN)]

      #d <- d + geom_point(data=data7,aes(x=LONC,y=LATC,color=REGI,shape=COMMON_NAME,size=REGI))
      d <- d + geom_pointrange(data=data7,aes(y=LATC,x=LONC,ymax = LATC + LATSEM, ymin=LATC - LATSEM,color=REGI,shape=COMMON_NAME)) 
      d <- d + geom_errorbarh(data=data7,aes(x=LONC,y=LATC,xmax = LONC + LONSEM, xmin=LONC - LONSEM,color=REGI))
      d <- d + scale_colour_manual(name="Regime",values=c("blue","red"))+scale_size_manual(name="Regime",values=c(0.3,0.3))+scale_shape_manual(name="Species",values=cex_1[1:ns])
      d <- d + geom_path(data=data7,aes(x=LONC,y=LATC,group=COMMON_NAME),size=0.2,color="gray50")
      
      if (ELLIP){ d <- d + stat_ellipse(data=data7,aes(x=LONC,y=LATC,group=SPeR,color=REGI),level=p1)}

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
     d <- d + facet_wrap(COMMON_NAME~LABEL,shrink=FALSE,ncol=length(unique(data7$LABEL2)))
    print(d)

  } 
}
