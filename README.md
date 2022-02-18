# Plotting_Functions
Functions for plotting fish distributions from AFSC survey data, location, depth, etc. with centroids.
Instructions on how to use the functions are documented on the top of each file. 

To use any of these functions you must start with the Get_DATA.R  which will pull data from AFSC. You need to supply a password, username, species, and bins you wish to use.

<B>Get_DATA.r</B>  
  This function pulls RACE survey haul and length data for later analysis  
  survey 52=AI,98=EBS Shelf, 78=EBS Slope, and 47 = GOA  
  bins can either be specified as in bins=c(0,100,200,300,400) or as percentiles based on time series length composition frequency 
  with the number of divisions entered in bins e.g. bins=5 would be 5 percentiles.  
  Example use code below for EBS Pollock and with 5 percentile bins:  
  afsc_username=""  
  afsc_password=""  
  data_1<-Get_DATA(username=afsc_username,password=afsc_password,species=21720,survey = 98,bins = 5,FIG=T,yr=1982:2021)  

You will also need to source the Get_TEMP.r file to set up temperatures for the data for many of the centroids by temperature plots.

<B>Get_TEMP.r</B>  
  Function calculates a timeseries of mean bottom temperature from the RACE surveys following Spencer (2006) for all surveys years
  http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2419.2008.00486.x/full
  The Bering Sea is likely the only one that works as intended. May need some work for the other regions. I think that the GOA Stratum surveyed are not consistent across time
  and therefore the mean temps across years are not going to be consistent in this treatment.  
  Uses data from the Get_DATA.r function  
  plotT=T plots temperatures over time  
  The temperature anomaly is based on the full timeseries average bottom temp.  
  "WARM" is > 0.5std from the mean, "COLD" is < 0.5std from the mean "MED" is between the two   
   Example:  
   Get_TEMP(data=data_1, plotT=T)

<B>plot_centroid_num.r</B>  
 Function for plotting RACE survey data for any species by temperature and depth (plotT=1) and specified length or by location (plotT=2)and specified length  
 This function takes data pulled using the get_DATA() function.  
 REG = T plots data by cold,med, and warm temperture regime, REF=F plots it by year  
 PATH = T plots a line between points ordered by year  
 ELLIP = T plots a modified Fox and Weisberg (2011) 
 method for calculating the ellipses (ELLIP=TRUE)with a confidence level set at p1.  
 colx = 1 is a yellow to brown color scheme friendly to B&W and colorbline colx=2 is a rainbow color scheme  
 Confidence intervals are based on 1.96*SE  
 Example plots:  
 plot_centroid_num(data=data1,REG=T,plotT=1,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)  
 plot_centroid_num(data=data_COD,REG=T,plotT=2,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)  
  
<B>plot_dist_num.r</B>  
   Function for plotting data by temperature (plotT=1) or location (plotT=2)and specified length   
   Uses data from Get_DATA() function and requires the Get_TEMP() function  
   Plots individual bins specified when data was retrieved.   
   example plots:  
   plot_dist_num(data=data_1,bin=1,plotT=1)

<B>plot_SEX_num.r</B>  
  Function to plot RACE survey data centroids of temperature and depth (plotT=1) or location (plotT=2) by sex
  Takes as input data retrieved using the Get_DATA() function, will plot all centroids by year and length bins
  p1 indicates the area of confidence interval ellipse  
  Requires the Get_TEMP() function  
  Examples:   
  plot_SEX_num(data=data_1,plotT=2,p1=0.9)  
  plot_SEX_num(data=data_1,plotT=1,p1=0.9)  
  
 <B>plot_year_num.r</B>  
  Function to plot RACE survey data centroids of temperature and depth (plotT=1) or location (plotT=2) by year
  Takes as input data retrieved using the Get_DATA() function, will plot all centroids by year and length bins
  p1 indicates the area of confidence interval ellipse  
  Example:   
  plot_YEAR_num(data=data_1,plotT=1,REG=TRUE,PATH=TRUE,ELLIP=TRUE,p1=0.9,colx=1)
 
<B>Plotting_bins.r</B>  
  Function for creating multiple PDFs of length bins as defined in the Get_DATA()  
  Function will plot into your working directory  
  Each PDF page will hold all years for each of the specified bins in the Get_DATA() function  
  Example:  
  Plotting_bins(datas=data_1)
