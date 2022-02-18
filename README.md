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
![image](https://user-images.githubusercontent.com/5395237/154738484-0685070f-192d-473b-a768-13cc609cc76c.png)

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
 plot_centroid_num(data=data_1,REG=T,plotT=1,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)  
 ![image](https://user-images.githubusercontent.com/5395237/154738733-af851ac9-69a6-4767-88bf-21ed8450a65d.png)
 
 plot_centroid_num(data=data_1,REG=T,plotT=2,PATH=F,ELLIP=F,p1=0.95,colx=1,thin=F)  
 ![image](https://user-images.githubusercontent.com/5395237/154738818-77aec738-77fe-4d4d-bef4-46ff34ef2d78.png)
 
<B>plot_dist_num.r</B>  
   Function for plotting data by temperature (plotT=1) or location (plotT=2)and specified length   
   Uses data from Get_DATA() function and requires the Get_TEMP() function  
   Plots individual bins specified when data was retrieved.   
   example plots:  
   plot_dist_num(data=data_1,bin=3,plotT=1)
   ![image](https://user-images.githubusercontent.com/5395237/154739164-80d58f06-8d1e-4c8b-a429-b1c2c9e2397b.png)
   plot_dist_num(data=data_1,bin=3,plotT=2)
   ![image](https://user-images.githubusercontent.com/5395237/154739224-4b5cd684-f881-459e-8e74-d15b5b0286fb.png)


<B>plot_SEX_num.r</B>  
  Function to plot RACE survey data centroids of temperature and depth (plotT=1) or location (plotT=2) by sex
  Takes as input data retrieved using the Get_DATA() function, will plot all centroids by year and length bins
  p1 indicates the area of confidence interval ellipse  
  Requires the Get_TEMP() function  
  Examples:   
  plot_SEX_num(data=data_1,plotT=2,p1=0.9)  
  ![image](https://user-images.githubusercontent.com/5395237/154739387-454139ef-3630-4dab-b7a5-73fa3822fcc0.png)
  plot_SEX_num(data=data_1,plotT=1,p1=0.9)  
  ![image](https://user-images.githubusercontent.com/5395237/154739432-8702d0aa-c8d9-4298-8e32-65a3d8a62bfa.png)
  
 <B>plot_year_num.r</B>  
  Function to plot RACE survey data centroids of temperature and depth (plotT=1) or location (plotT=2) by year
  Takes as input data retrieved using the Get_DATA() function, will plot all centroids by year and length bins
  p1 indicates the area of confidence interval ellipse  
  Example:   
  plot_YEAR_num(data=data_1,plotT=1,REG=TRUE,PATH=TRUE,ELLIP=TRUE,p1=0.9,colx=1)
  ![image](https://user-images.githubusercontent.com/5395237/154739518-5a620add-620b-46bd-bf0a-b4cbf0b82719.png)
  plot_YEAR_num(data=data_1,plotT=2,REG=TRUE,PATH=TRUE,ELLIP=TRUE,p1=0.9,colx=1)
  ![image](https://user-images.githubusercontent.com/5395237/154739601-b25a8a02-7910-4278-aa13-b83afd9e86de.png)

 
<B>Plotting_bins.r</B>  
  Function for creating multiple PDFs of length bins as defined in the Get_DATA()  
  Function will plot into your working directory  
  Each PDF page will hold all years for each of the specified bins in the Get_DATA() function  
  Example:  
  Plotting_bins(datas=data_1)
  
  
  
<B><I>Series of plots for doing boxplots by temperature, depth, and location </B></I>  
<B>Plot_WC.r</B>  
<I>Get_td(data=data_1)</I>  
 Function which pulls the temperature data from the data file.  
 <I>Get_Mean_td(data=data_1)</I>
 Function to calculate both mean shelf temperatures and mean temperature for the species at the different bins.  
<I>Plot_WC(data=data_1,plotT=1,depth=c(-250,0))</I>
 Function to plot boxplots of bins by depth and year temperature regine (warm, med, cold). Two possible plots, first is simply the boxplot by depth, the second includs lines in red grey and blue depicting shelf mean for warm, med, and cold years. 
 Depth sets the Y-axis of the plot.  
 Plot 1:  Plot_WC(data=data_1,plotT=1,depth=c(-120,0))  
 ![image](https://user-images.githubusercontent.com/5395237/154748510-a70ae62a-3cde-4bc1-b9de-1a3debdae52e.png)  
 Plot 2: Plot_WC(data=data_1,plotT=2)  
 ![image](https://user-images.githubusercontent.com/5395237/154748479-573af5bc-c411-494d-b659-4aa8936e1c9e.png)  
<I>Plot_LL(data=data_1,plotT=1)</I>  
  Function to plot boxplots of bins by latitude and longlitude by annual temperature regine (warm, med, cold). Two possible plots, first is simply the boxplot by depth, the second includs lines in red grey and blue depicting shelf mean for warm, med, and cold years.  
  Plot_LL(data_1,plotT=1)  
  ![image](https://user-images.githubusercontent.com/5395237/154748949-eb378f2a-d1cc-452f-a4e5-8f0f8edb94bb.png)  
  Plot_LL(data_1,plotT=2)  
  ![image](https://user-images.githubusercontent.com/5395237/154749033-6fc1bac0-57f8-4d30-82dd-e288de6f39ea.png)  


 
 
 <B> Plot of all 'Major' species locations by size classes in the AFSC bottom trawl surveys </B>  
 You can click the species on and off using the legend. Each ball represents a size class by pentile for each species for each survey (1982-2017). Smaller bubbles are smaller size classes. The lines go through the mean of each size class for each species for all survey years and can also be toggled on or off.  
  
 https://chart-studio.plotly.com/~zandjyo/60.embed
 
 
