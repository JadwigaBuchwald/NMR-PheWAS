#Forest plots for the final 71 variables
#5.4.2023
#Jadwiga Buchwald

#These were run locally using RStudio

#----------------------------------------------------
#Drawing forest plots using nightingale's ggforestplot:
#https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html
#----------------------------------------------------

#-----------------
#Libraries:
#-----------------

setwd("C:/LocalData/jadwigab/PhD_FIMM/PhD_Analysis/6_PheWAS/Phewas_PHESANT/Phewas2Stages")

#install.packages("devtools")
library(devtools)

#install.packages("ggforestplot") #Did not work now taht i updated R
#devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

#install.packages("tidyverse")
library(tidyverse)
library(plyr)
library(dplyr)
#?ggforestplot::forestplot

#----------------------------------------------------


#-----------------
#Reading in our results and getting data ready
#-----------------

#-------------------------------------------------------------------------
#Starting with the Binary variables.
#-------------------------------------------------------------------------

#Note with the Binary variables it looked like it would make most sense to show the FDR sig results all in one plot in the main text and then have
#in the supplementary all the variables in two plots so that the scale is more readable and the original Cat3_Title has been kept more intact.

#I'll plot four plots:  
#A) v1 one with all variables in one plot v2 all variables that were FDR sig in one or more groups 
#b) two plots so that the scale is more suited for the given variables & again  v1 all v2 only those with FDR sig in one or more dataset

  B<-read.table("./Final_25_BinaryForest.txt", sep="\t",header=TRUE)
  str(B)
  dim(B) #66 x 9

#Renaming the data variable with a capital letter so that it looks better in the plot.
  names(B)[names(B)=="data"]<-"Data"


#I'll start by plotting Av1 &  Av2

  #Checking our Cat3_Title is sensible
    table(B$Cat3_Title)
    #Early life factors Health and medical history  Lifestyle and environment        Medical information                Medications                 Operations       Psychosocial factors 
    #3                          6                         30                          2                          4                          3                          3 
    #Summary Operations 
    #15

#I'll rename some of the categories so that we have a few less in order to make a prettier plot
#Renaming Summary Operations as Operations so that it is less confusing and we get the Leg artery bypass surgery in the right plot.
#Renaming Medical information as Lifestyle and environment
#Renaming Medications as Health and medical history
  B$Cat3_Title<-recode(B$Cat3_Title, "Summary Operations" = "Operations", "Medical information" = "Lifestyle and environment", "Medications"="Health and medical history")

  table(B$Cat3_Title)
  #Early life factors Health and medical history  Lifestyle and environment                 Operations       Psychosocial factors 
  #3                         10                         32                         18                          3 

  #V1: All variables
    ggforestplot::forestplot(
      df = B,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GRS of the NMR",
      title = "Logistic regression",
      logodds = TRUE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 

  #V2: Only those with a FDR sig result in one of the groups
      #List of variables we want
      keepers <- unique(subset(B,B$NotFDRsig==0)[,c("Phenotype")])
      length(keepers) #16 
      length(unique(B$Phenotype)) #all in all 25 so 25-16=9 that were not FDR sig in any group
      Sigset <- subset(B, B$Phenotype %in% keepers)

          #V2: FDR sig variables
          ggforestplot::forestplot(
            df = Sigset,
            name = Phenotype,
            estimate = beta,
            se = se,
            colour= Data,
            pvalue = NotFDRsig,
            psignif = 0.05,
            xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GRS of the NMR",
            title = "Logistic regression",
            logodds = TRUE
          )+
            ggforce::facet_col(
              facets = ~Cat3_Title,
              scales = "free_y",
              space = "free"
            ) 
      
      
#Then to Bv1 and Bv2
      B<-read.table("./Final_25_BinaryForest.txt", sep="\t",header=TRUE)
      str(B)
      dim(B) #66 x 9
      
      #Renaming the data variable with a capital letter so that it looks better in the plot.
      names(B)[names(B)=="data"]<-"Data"
      
      
#Plotting Medication+operations in one plot and the rest in another

  B$Cat3_Title<-recode(B$Cat3_Title, "Summary Operations" = "Operations", "Medical information" = "Lifestyle and environment")

  One<-subset(B,B$Cat3_Title %in% c("Medications","Operations"))
  Two<-subset(B,!(B$Cat3_Title %in% c("Medications","Operations")))

    ggforestplot::forestplot(
      df = One,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GRS of the NMR",
      title = "Logistic regression (1/2)",
      logodds = TRUE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    
    ggforestplot::forestplot(
      df = Two,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GRS of the NMR",
      title = "Logistic regression (2/2)",
      logodds = TRUE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    

#-------------------------------------------------------------------------------
#   Ordinal variables
#-------------------------------------------------------------------------------    
    
    #Drawing the final ordered-logistic plots
    
    #We had 12 variables, now we should have 8
    #4 were moved to linear (coffee went into binary as well): 2887 Number of cigarettes previously smoked daily, 3456 Number of cigarettes currently smoked daily, 30150 Eosinophill count, 1498 Coffee intake  
    #two had their sign changed:3506 smoking compare to 10 yrs previous & 1249 Past tobacco smoking 
    #Note: I checked the coding for 1239 Current tobacco smoking coding and PHESANT has already corrected it!

    O<-read.table("./Final_8_OrderedLogisticForest.txt", sep="\t",header=TRUE)
    str(O)
    dim(O) #20 x 9
    
    #Renaming the data variable with a capital letter so that it looks better in the plot.
    names(O)[names(O)=="data"]<-"Data"
    
    
    ggforestplot::forestplot(
      df = O,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "OR \n Odd ratio for one increment in phenotype category \n per 1-SD increment in the GRS of the NMR",
      title = "Ordered logistic regression",
      logodds = TRUE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    
    
    
    
    
    
#-------------------------------------------------------------------------
# Linear variables
#-------------------------------------------------------------------------

#NOTE THESE LINES HAVE TO BE UPDATED ONCE I'VE MANAGED TO UPDATE THE SCRIPTS ON FS/PROJECTS    
    L<-read.table("./Final_38_LinearForest.txt", sep="\t",header=TRUE)
    str(L)
    dim(L) #117 x 9
    
    #Renaming the data variable with a capital letter so that it looks better in the plot.
    names(L)[names(L)=="data"]<-"Data"

#Deleting the few extra rows (this step is not needed once the data has been corrected)
    R<-read.table("./Final_38_LinearResults.txt", sep="\t",header=TRUE)
    str(R)
    dim(R) #38 x 30
    
    subset(R, R$Phenotype %in% c("nd1498 - Coffee quantity (excluding decaf)", "nd1498 - Coffee quantity (including decaf)")) [,c("Phenotype","beta_A", "beta_E", "beta_N")]
#    Phenotype     beta_A     beta_E      beta_N
#    27 nd1498 - Coffee quantity (excluding decaf) 0.01335725 0.01474301 0.013847260
#    28 nd1498 - Coffee quantity (including decaf) 0.01061656 0.01354754 0.008206222

    subset(L, L$Phenotype %in% c("nd1498 - Coffee quantity (excluding decaf)", "nd1498 - Coffee quantity (including decaf)")) [,c("Phenotype","beta", "Data")]
#    Phenotype        beta  Data
# keepers:
#    76 nd1498 - Coffee quantity (excluding decaf) 0.013357251   All
#    77 nd1498 - Coffee quantity (excluding decaf) 0.014743008  Ever
#    78 nd1498 - Coffee quantity (excluding decaf) 0.013847260 Never
# Should delete    
#    79 nd1498 - Coffee quantity (excluding decaf) 0.010616561   All
#    80 nd1498 - Coffee quantity (excluding decaf) 0.013547545  Ever
#    81 nd1498 - Coffee quantity (excluding decaf) 0.008206222 Never
    
#    82 nd1498 - Coffee quantity (including decaf) 0.013357251   All
#    83 nd1498 - Coffee quantity (including decaf) 0.014743008  Ever
#    84 nd1498 - Coffee quantity (including decaf) 0.013847260 Never

# keepers:    
#    85 nd1498 - Coffee quantity (including decaf) 0.010616561   All
#    86 nd1498 - Coffee quantity (including decaf) 0.013547545  Ever
#    87 nd1498 - Coffee quantity (including decaf) 0.008206222 Never

  #We delete the faulty rows
    OK <- L[-c(79:84),] 

    #We check:
    subset(OK, OK$Phenotype %in% c("nd1498 - Coffee quantity (excluding decaf)", "nd1498 - Coffee quantity (including decaf)")) [,c("Phenotype","beta", "Data")]
#                                      Phenotype        beta  Data
#    76 nd1498 - Coffee quantity (excluding decaf) 0.013357251   All
#    77 nd1498 - Coffee quantity (excluding decaf) 0.014743008  Ever
#    78 nd1498 - Coffee quantity (excluding decaf) 0.013847260 Never
#    85 nd1498 - Coffee quantity (including decaf) 0.010616561   All
#    86 nd1498 - Coffee quantity (including decaf) 0.013547545  Ever
#    87 nd1498 - Coffee quantity (including decaf) 0.008206222 Never
 
    L<-OK   
             
    #v1 all variables
    ggforestplot::forestplot(
      df = L,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "Beta \n 1-SD increment in phenotype per 1-SD increment in the GRS of the NMR",
      title = "Linear Regression",
      logodds = FALSE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
 
    
    #V2: Only those with a FDR sig result in one of the groups
    #List of variables we want
    keepers <- unique(subset(L,L$NotFDRsig==0)[,c("Phenotype")])
    length(keepers) #33
    length(unique(L$Phenotype)) #all in all 38 so 38-33=5 that were not FDR sig in any group
    Sigset <- subset(L, L$Phenotype %in% keepers)    
    
    
    ggforestplot::forestplot(
      df = Sigset,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "Beta \n 1-SD increment in phenotype per 1-SD increment in the GRS of the NMR",
      title = "Linear Regression",
      logodds = FALSE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    

#The below plots were not 
    
    #Creating two plots: to get a more readable scaling

 
    One<-subset(L,!(L$Cat3_Title %in% c("Blood assays", "Lifestyle and environment")))
    Two<-subset(L,L$Cat3_Title %in% c("Blood assays", "Lifestyle and environment"))
       
    ggforestplot::forestplot(
      df = One,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "Beta \n 1-SD increment in phenotype per 1-SD increment in the GRS of the NMR",
      title = "Linear Regression (1/2)",
      logodds = FALSE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    ggforestplot::forestplot(
      df = Two,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "Beta \n 1-SD increment in phenotype per 1-SD increment in the GRS of the NMR",
      title = "Linear Regression (2/2)",
      logodds = FALSE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    
