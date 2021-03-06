<B>WARNING!!! These multispecies functions plots are new and relatively untested...</B>  

To use any of these functions you must start with the Get_MDATA.R  which will pull data from AFSC. You need to supply a password, username, list of species, and bins you wish to use.

<B>Get_MDATA.r</B>  
  This function pulls RACE survey haul and length data for later analysis  
  survey 52=AI,98=EBS Shelf, 78=EBS Slope, and 47 = GOA  
  bins can either be specified as in bins=c(0,100,200,300,400) or as percentiles based on time series length composition frequency   
  with the number of divisions entered in bins e.g. bins=5 would be 5 percentiles  
  Example use code below for EBS Pcod and EBS pollock and with 5 percentile bins   
  afsc_username=""  
  afsc_password=""  
  dataM<-Get_MDATA(afsc_username,afsc_password,species=c("21740","21720"),survey = 98,bins = 5,yr=1982:2021)
 
 
<B>plot_M_centriod_bin.r</B>  
  Function for plotting RACE survey data centroids for any species by temperature and depth (plotT=1) and   
  specified length or by location (plotT=2)and specified length for all years comnbined    
  This function takes data pulled using the get_DATA() function.  
  This function also requires that the Get_TEMP() function is loaded.  
  REG=T seperates the centroids by warm, med, and cold years as determiend by the +-0.5 SE from the mean for all surveys examined  
  PATH = T plots a line between points ordered by length bin  
  colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme  
  Example plots:  
  plot_M_centroid_bin(data=dataM,REG=T,plotT=1,colx=1,SINGLE=F,SUB=c(1:20)) 
  ![image](https://user-images.githubusercontent.com/5395237/154740063-70d16f16-d181-4e7e-a0b0-a8096796bcfa.png)

  plot_M_centroid_bin(data=dataM,REG=T,plotT=2,PATH=T,colx=2)  
 
<B>plot_M_centriod_num.r</B>
  Function for plotting RACE survey data for any species by temperature and depth (plotT=1) and specified length or by location (plotT=2)and specified length    
  This function takes data pulled using the get_DATA() function.  
  REG = T plots data by cold,med, and warm temperture regime, REF=F plots it by year  
  PATH = T plots a line between points ordered by year  
  ELLIP = T plots a modified Fox and Weisberg (2011)   
  method for calculating the ellipses (ELLIP=TRUE)with a confidence level set at p1.  
  colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme   
  Confidence intervals are based on 1.96*SE  
  Example plots:  
  plot_M_centroid_num(data=dataM,REG=T,plotT=1,PATH=F,ELLIP=F,p1=0.95,colx=1)
  ![image](https://user-images.githubusercontent.com/5395237/154740348-230d1c53-780e-4380-b851-c2ce7b93cec5.png)

  plot_M_centroid_num(data=dataM,REG=T,plotT=2,PATH=F,ELLIP=F,p1=0.95,colx=1)
  ![image](https://user-images.githubusercontent.com/5395237/154740305-7bafdc61-4d29-49a7-b54c-eeba8c0730c5.png)

