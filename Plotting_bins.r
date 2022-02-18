## Function for creating multiple PDFs of length bins as defined in the Get_DATA Function.
## Created by Steven Barbeaux 1/30/2017
## Function for creating multiple PDFs of length bins as defined in the Get_DATA() Function will plot into your working directory
## Each PDF page will hold all years for each of the specified bins in the Get_DATA() function
## example
## Plotting_bins(datas=data1)

Plotting_bins<-function(datas=data1) { 
    length    <- datas$length
    survey    <- unique(length$SURVEY_DEFINITION_ID)
    species    <- unique(length$SPECIES_CODE)
    bins     <- sort(unique(length$BIN))
    cn      <- datas$SN$COMMON_NAME
    name     <- paste(cn,survey,"NUM",sep="_")
  
## Temperature and depth plots
    pdf(paste(name,".pdf",sep=""),width=10,height=7.5)
    for(i in 1:length(bins)) {
        plot_dist_num(data=datas,bin=i,plotT=1 )
    }
    plot_centroid_num(data=datas,REG=F,plotT=1,PATH=T,colx=1,thin=F)
    plot_centroid_bin(data=datas,REG=F,plotT=1,PATH=T,colx=1,thin=F)
    plot_centroid_num(data=datas,REG=T,plotT=1,PATH=T,colx=1,thin=F)
    plot_centroid_bin(data=datas,REG=T,plotT=1,PATH=T,colx=1,thin=F)
    plot_SEX_num(data=datas,plotT=1,p1=0.9)
    dev.off()

## Location plots
    pdf(paste(name,"0.pdf",sep=""),width=10,height=7.5)
    for(i in 1:length(bins)) {
        plot_dist_num(data=datas,bin=i, plotT=2 )
    } 

    plot_centroid_num(data=datas,REG=T,plotT=2,PATH=T,colx=1,thin=F)
    plot_centroid_bin(data=datas,REG=T,plotT=2,PATH=T,colx=1,thin=F)
        plot_centroid_num(data=datas,REG=F,plotT=2,PATH=T,colx=1,thin=F)
    plot_centroid_bin(data=datas,REG=F,plotT=2,PATH=T,colx=1,thin=F)
    plot_SEX_num(data=datas,plotT=2,p1=0.9)

    dev.off()
    print(paste("Plots are in ",getwd(),sep=""))

}
