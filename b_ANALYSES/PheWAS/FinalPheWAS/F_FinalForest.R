#Forest plots for the final 71 variables
#5.4.2023
#Jadwiga Buchwald

#These were run locally using RStudio
#R version 4.1.1 (2021-08-10) -- "Kick Things"


#----------------------------------------------------
#Drawing forest plots using nightingale's ggforestplot:
#https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html
#----------------------------------------------------

#-----------------
#Libraries:
#-----------------

setwd("[PATH]/Phewas_PHESANT/Phewas2Stages")

#install.packages("devtools")
library(devtools)

#install.packages("ggforestplot") 
#devtools::install_github("NightingaleHealth/ggforestplot") #if the above doesn't work
library(ggforestplot)

#install.packages("tidyverse")
library(tidyverse)
library(plyr)
library(dplyr)
#?ggforestplot::forestplot #checking the help page

#----------------------------------------------------


#-----------------
#Reading in our results and getting data ready
#-----------------

#-------------------------------------------------------------------------
#Starting with the Binary variables.
#-------------------------------------------------------------------------

  B<-read.table("./Final_25_BinaryForest.txt", sep="\t",header=TRUE)
  str(B)
  dim(B) #66 x 9

#Renaming the data variable with a capital letter so that it looks better in the plot.
  names(B)[names(B)=="data"]<-"Data"

#Forming sensible categories for the variables
  #Checking the current categories: Cat3_Title 
    table(B$Cat3_Title)
    #Early life factors Health and medical history  Lifestyle and environment        Medical information                Medications                 Operations       Psychosocial factors 
    #3                          6                         30                          2                          4                          3                          3 
    #Summary Operations 
    #15

#I'll rename some of the categories so that we have a few less in order to make a prettier plot
  #Renaming Summary Operations as Operations so that it is less confusing and we get the Leg artery bypass surgery in the right category.
  #Renaming Medical information as Lifestyle and environment
  #Renaming Medications as Health and medical history
  B$Cat3_Title<-recode(B$Cat3_Title, "Summary Operations" = "Operations", "Medical information" = "Lifestyle and environment", "Medications"="Health and medical history")

  table(B$Cat3_Title)
  #Early life factors Health and medical history  Lifestyle and environment                 Operations       Psychosocial factors 
  #3                         10                         32                         18                          3 


    
  # Plotting only those with a FDR sig result in one of the groups(Ever/Never/All)

      #List of variables we want
      keepers <- unique(subset(B,B$NotFDRsig==0)[,c("Phenotype")])
      length(keepers) #16 
      length(unique(B$Phenotype)) #all in all 25 so 25-16=9 that were not FDR sig in any group
      Sigset <- subset(B, B$Phenotype %in% keepers)

#          # FDR sig variables : Simple plot (all on the same scale which makes the plot difficult to read)
#          ggforestplot::forestplot(
#            df = Sigset,
#            name = Phenotype,
#            estimate = beta,
#            se = se,
#            colour= Data,
#            pvalue = NotFDRsig,
#            psignif = 0.05,
#            xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GRS of the NMR",
#            title = "Logistic regression",
#            logodds = TRUE
#          )+
#            ggforce::facet_col(
#              facets = ~Cat3_Title,
#              scales = "free_y",
#              space = "free"
#            ) 
          
          
          #-->Note we have 16 variables
          
#Fixing the scale
          #Creating two Health and medical history categories to get the scales nicely:
          Sigset$Cat3_Title_scale<-ifelse(Sigset$Phenotype=="20003#1140867092 - Serenace medication","Medical information", Sigset$Cat3_Title)
          table(Sigset$Cat3_Title_scale)
          
		  
#Creating two seperate plots and the legend seperately and then combining them into one
          
        #First part of the plot and title
         Big<-subset(Sigset,!(Sigset$Cat3_Title_scale %in% c("Operations","Medical information")))
          
         p1<- ggforestplot::forestplot(
            df = Big,
            name = Phenotype,
            estimate = beta,
            se = se,
            colour= Data,
            pvalue = NotFDRsig,
            psignif = 0.05,
            xlab = " ",
            title = "Logistic regression",
            logodds = TRUE
          )+
            ggforce::facet_col(
              facets = ~Cat3_Title_scale,
              scales = "free_y",
              space = "free"
            ) + theme(legend.position = "none",axis.title.x=element_blank())
          
          
        #Second part of the plot without the title
         Small<-subset(Sigset,Sigset$Cat3_Title_scale %in% c("Operations","Medical information"))
         p2<- ggforestplot::forestplot(
            df = Small,
            name = Phenotype,
            estimate = beta,
            se = se,
            colour= Data,
            pvalue = NotFDRsig,
            psignif = 0.05,
            xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GS of the NMR",
            #title = "Logistic regression",
            logodds = TRUE
          )+
            ggforce::facet_col(
              facets = ~Cat3_Title_scale,
              scales = "free_y",
              space = "free"
            ) + theme(legend.position = "none")
     
         
         #Legend
         
         #In order to get the legend I need the package cowplot
         #Note, had trouble loading cowplot to R 4.1.1 but I already had it for 4.2.2
          custom_library <- "C:/rlibs/4.2.2"
          #Note i want this folder to be the last option
          .libPaths(c(.libPaths(),custom_library))
          #install.packages("cowplot")
          library(cowplot)
          
          legend <- get_legend(ggforestplot::forestplot(
            df = Small,
            name = Phenotype,
            estimate = beta,
            se = se,
            colour= Data,
            pvalue = NotFDRsig,
            psignif = 0.05,
            xlab = "OR \n Odds ratio for phenotype (95 % CI) \n per 1-SD increment in the GS of the NMR",
            #title = "Logistic regression",
            logodds = TRUE
          )+
            ggforce::facet_col(
              facets = ~Cat3_Title_scale,
              scales = "free_y",
              space = "free"
            ))

          
        #Combining and placing legend at the middle on the right. 
         main<-plot_grid(p1, p2, ncol=1, align='v')
         ggdraw(plot_grid(main,legend, ncol=2, rel_widths=c(1, 0.2)))
                

         ggsave(
           "./Plots/Logistic_ForestPlot_GS_ggsave.pdf"
         ) 

#-------------------------------------------------------------------------------
#   Ordinal variables
#-------------------------------------------------------------------------------    
    
    #Drawing the final ordered-logistic plots
    
    #We had 12 variables, now we should have 8
    #4 were moved to linear (coffee went into binary as well): 2887 Number of cigarettes previously smoked daily, 3456 Number of cigarettes currently smoked daily, 30150 Eosinophill count, 1498 Coffee intake  
    #two had their sign changed:3506 smoking compare to 10 yrs previous & 1249 Past tobacco smoking 
    #Note: I checked the coding for 1239 Current tobacco smoking coding and PHESANT has already corrected it!

    #Reading in the data:     
    O<-read.table("./Final_8_OrderedLogisticForest.txt", sep="\t",header=TRUE)
    str(O)
    dim(O) #20 x 9
    
    #Renaming the data variable with a capital letter so that it looks better in the plot.
    names(O)[names(O)=="data"]<-"Data"
    
    #Plotting the forestplot
    ggforestplot::forestplot(
      df = O,
      name = Phenotype,
      estimate = beta,
      se = se,
      colour= Data,
      pvalue = NotFDRsig,
      psignif = 0.05,
      xlab = "OR \n Odds ratio for one increment in phenotype category \n per 1-SD increment in the GS of the NMR",
      title = "Ordered logistic regression",
      logodds = TRUE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    
    #Saving the plot
    ggsave(
      "./Plots/Ordinal_ForestPlot_GS_ggsave.pdf"
    )  
    
    
    
    
    
#-------------------------------------------------------------------------
# Linear variables
#-------------------------------------------------------------------------

  #Reading in the data
    L<-read.table("./Final_38_LinearForest.txt", sep="\t",header=TRUE)
    str(L)
    dim(L) #117 x 9
        
    #Renaming the data variable with a capital letter so that it looks better in the plot.
    names(L)[names(L)=="data"]<-"Data"

  #Note: When creating the data for the forest plot, I had managed to introduce some false lines as I had 
    #merged a datafile RESULTS with FOREST using varName (1498 for both coffee excluding decaf and including decaf)
  #THUS: Deleting the few extra rows 
    #Checking the values coffee excluding/including decaf should have from the results table:
    R<-read.table("./Final_38_LinearResults.txt", sep="\t",header=TRUE)
    str(R)
    dim(R) #38 x 30
    
    subset(R, R$Phenotype %in% c("nd1498 - Coffee quantity (excluding decaf)", "nd1498 - Coffee quantity (including decaf)")) [,c("Phenotype","beta_A", "beta_E", "beta_N")]
#    Phenotype     beta_A     beta_E      beta_N
#    27 nd1498 - Coffee quantity (excluding decaf) 0.01335725 0.01474301 0.013847260
#    28 nd1498 - Coffee quantity (including decaf) 0.01061656 0.01354754 0.008206222

    #Seeing which rows are to be kept and which deleted from our data for plotting the forest plot
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
             
    
    
    # Only those with a FDR sig result in one of the groups
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
      xlab = "Beta \n 1-SD increment in phenotype \n per 1-SD increment in the GS of the NMR",
      title = "Linear Regression",
      logodds = FALSE
    )+
      ggforce::facet_col(
        facets = ~Cat3_Title,
        scales = "free_y",
        space = "free"
      ) 
    
    ggsave(
      "./Plots/Linear_ForestPlot_GS_ggsave.pdf"
    ) 

