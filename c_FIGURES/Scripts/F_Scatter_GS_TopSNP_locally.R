#26.6.2023

#Fig. S3 Scatterplot of the imputed genotype dosage at the chromosome 19 top SNP (rs56113850) against the GS for the NMR in UKB.

#These were run locally using RStudio



setwd("[path]")
sessionInfo()
getwd()


#-----------------
#Libraries:
#-----------------

library(plyr)
library(dplyr)
library(ggplot2)

#For plotting two figures in one:
#.libPaths("[path]")
library(cowplot) 
library(gridExtra)
library(grid)


#----------------------------------------------------



#-----------------------------------------------
# Getting Data
#-----------------------------------------------

# Reading in the data

  D<-read.table("./GRS_CPD_plotting.txt", header=T)

	str(D)
#	'data.frame':	343662 obs. of  6 variables:
#	 $ eid       : int  2323946 4890737 2009936 1702642 3629675 5387874 4315548 3676197 4521630 1543457 ...
#	 $ GRS       : num  1.591 1.553 1.457 1.416 -0.372 ...
#	 $ zGRS      : num  0.7 0.632 0.46 0.387 -2.814 ...
#	 $ Sex       : chr  "Male" "Female" "Female" "Female" ...
#	 $ CPD       : int  NA NA NA NA NA NA NA NA 5 NA ...
#	 $ rs56113850: num  NA 2 2 1 0 1 1 2 1 1 ...

#--------------------------------------
# PLOTS
#--------------------------------------

#---------------------
# Scatter plot of GS and Top SNP
#---------------------

  #Only keeping non missing rows as otherwise cor function gives NA
    noNA<-na.omit(D[,c("eid","GRS","rs56113850")])		 
        # > dim(noNA)
        # [1] 343654      3

	#Having a look at the Genetic Score
		#> summary(D$GRS)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
		#-1.7325  0.8967  1.2424  1.2000  1.6244  2.9271 
		#> summary(D$zGRS)
		#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
		#-5.24887 -0.54280  0.07597  0.00000  0.75969  3.09140 
		#> summary(noNA$GRS)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
		#-1.7325  0.8967  1.2424  1.2000  1.6244  2.9271 
  
  
  #Getting the correlation
    cor(noNA$GRS,noNA$rs56113850)
        #> cor(noNA_SC$GRS,noNA_SC$rs56113850)
        #          [,1]
        #[1,] 0.7314881
  
  #Plotting a simple version of the plot
    p7<-ggplot(noNA, aes(x=rs56113850, y=GRS)) + 
      geom_point(size=0.1)+
      geom_smooth(method=lm)+
      theme(legend.position = "none", panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
    str(D)
  
    pdf("./scatter_XtopSNP_YGRS.pdf")
    print(p7) 
    dev.off()

      
  # Adding R2, Beta, n
  
  #model
    m <- lm(GRS ~ rs56113850, D)
    summary(m)
  
      #Call:
      #lm(formula = GRS ~ rs56113850, data = D)
  
      #Residuals:
      #     Min       1Q   Median       3Q      Max
      #-2.72909 -0.21172  0.03263  0.24830  1.76127
  
      #Coefficients:
      #             Estimate Std. Error t value Pr(>|t|)
      #(Intercept) 0.5240354  0.0012560   417.2   <2e-16 ***
      #rs56113850  0.5850959  0.0009304   628.9   <2e-16 ***
      #---
      #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
      #Residual standard error: 0.381 on 343652 degrees of freedom
      #  (8 observations deleted due to missingness)
      #Multiple R-squared:  0.5351,    Adjusted R-squared:  0.5351
      #F-statistic: 3.955e+05 on 1 and 343652 DF,  p-value: < 2.2e-16
  
  
  #Getting the wanted values
    r2 = summary(m)$adj.r.squared
    my.b = summary(m)$coefficients[2,1]
    my.n = nobs(m)
    r2
    my.b
    my.n
      #> r2
      #[1] 0.5350735
      #> my.b
      #[1] 0.5850959
      #> my.n
      #[1] 343654
  
  #PLOT WITH TEXT
    ntemp<-expression(italic(n) == "343654")
    
    p7withtext<-ggplot(noNA, aes(x=rs56113850, y=GRS)) + 
     geom_point(size=0.1)+
     geom_smooth(method=lm)+
     theme(legend.position = "none", panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+
     annotate("text",x=0.25, y=2.8,label=expression(italic(R)^2 == "0.535"))+
     annotate("text",x=0.25, y=2.5,label=expression(italic(beta) == "0.59"))+
     annotate("text",x=0.25, y=2.2,label=ntemp)+
      labs(y=expression("GS"))


    
    save_plot("./Plots/FigS1_Scatter_GS_TopSNP.pdf", p7withtext, 
              ncol = 1, # we're saving a grid plot of 2 columns
              nrow = 1, # and 2 row, 
              base_aspect_ratio = 1.3 #default is 1.1. which works well, when we don't have a legend, each individual subplot should have an aspect ratio of 1.3
    )
        
#Note: used to be fig S1 when I created the script but ended up being Fig S3.
