#MRBase and FinnGen top SNP rs56113850 check ups

#2023 Sep 5
#Performed locally using Rstudio, R version 4.2.2 

##########################################
#Getting the association results
  #MRBase 
    #http://app.mrbase.org/ (quick snp lookup rs56113850)
    #Downloaded the results as a csv file. 
  
  #FinnGen
    #https://r9.finngen.fi/ (Search for a variant, gene, or phenotype rs56113850)
    #NOTE: AFTER clicking on p-value to ensure the results are ranked from smallest p-value to the largest
    # --> Downloaded the results as a tsv file
###########################################


setwd("[Path]/6_PheWAS/Phewas_topSNP_MRBase")

#######################################
#libraries

library(ggplot2) #for ggplot
library(dplyr) #for keeping only the first instance when wanting unique values left_join
#install.packages("pastecs")
library(pastecs) #For summary statistics
library(readxl) #For reading in xlsx
#install.packages("openxlsx")
library(openxlsx) #For saving xlsx

#########################################

#-------------------------
  #MRBase
#-------------------------

    #MB<-read.table("mr_base_rs56113850_2023_Sep_5.csv", header = TRUE, stringsAsFactors = FALSE)
    #-->Did not work, so I opened it in excel made sure they are all nicely in columns and saved as xlsx file.

      MB <- read_excel("mr_base_rs56113850_2023_Sep_5.xlsx")
      dim(MB)[1]
      str(MB)
      MB$p<-as.numeric(MB$p)
      MB$beta<-as.numeric(MB$beta)
      MB$eaf<-as.numeric(MB$eaf)
      MB$minuslogp<--(log10(MB$p))
      round(stat.desc(MB[,c("p","beta","eaf")]),3)[c(1,4,5,8,9,13),]
        #p        beta       eaf
        #nbr.val 39105.000   39105.000 38215.000
        #min         0.000     -16.688     0.286
        #max         1.000 1143941.000     0.734
        #median      0.490       0.000     0.565
        #mean        0.490      29.339     0.569
        #std.dev     0.292    5784.810     0.017
        39158-39105 #53
      
      sum(is.na(MB$trait)) #0
      
      #Deleting the rows with missing values for p and beta
      DP <- MB[!is.na(MB$p), ]
      MBC <- MB[complete.cases(MB[, c("p", "beta")]), ] #dim 39105 x 14 from 39158 x14
      MB<-MBC

    #Getting the FDR significant results
      totaltests<-dim(MB)[1]
      MB$bhcritical<-as.numeric(rownames(MB))*0.05/totaltests
      max(which(MB$p <= MB$bhcritical)) #199
      #-->199 reach the 0.05 FDR level of significance. Note, many variables are listed many times.
      cutoff<-MB$bhcritical[199]
      
    #Checking what we would have got with bonferroni correction
      bfthreshold<-0.05/totaltests
      dim(bf<-subset(MB,MB$p <= 1.276878e-06))[1] #105
      #confirming:
      MB$bf<-ifelse(MB$p <= 1.276878e-06,"T","F")
      
      
    #Having a closer look at our FDR results  
      MBfdr<-MB[1:199,]
      length(unique(MBfdr$trait)) #176
      
      # keeping only the first instance of each unique trait
      uniqueMBfdr <- distinct(MBfdr, trait, .keep_all = TRUE) 
      
      #-->Having a look at it and ordering based on trait: we see that most are still there multiple times
      
    #Volcano plot
      
      #Grouping variable: FDR significance and direction of effect
      MB$FDRlessthan5<-ifelse(MB$p<cutoff,1,0)
      MB$FDR_and_direction<-ifelse(MB$FDRlessthan5==1 & MB$beta<0,2,MB$FDRlessthan5)
      table(MB$FDR_and_direction)
      #0     1     2 
      #38906   146    53 
      sum(is.na(MB$FDR_and_direction)) #0
     
      #Only adding labels to bonferroni significant points so that they do not overlap
      MB$Trait<-ifelse(abs(MB$beta)>0.01 & MB$p<0.05/totaltests,as.character(MB$trait),"")
      head(MB)
      ggplot(MB,aes(beta,minuslogp,label=Trait)) +
        geom_point(aes(colour=factor(FDR_and_direction)),size=2) + scale_color_manual(values=c("#999999", "#009E73", "#e79f00"), labels=c("FDR>5%", "FDR<5%, Beta>0", "FDR<5%, Beta<0")) + xlim(-0.05,0.065) + #plotting data
        geom_hline(yintercept = -log10(0.05/totaltests),linetype="dashed")+
        geom_text(check_overlap = TRUE) +
        labs(x="Beta (EA=C, OA=T)", y="-log10(p)", col="")
      
      ggsave(
        "./Plots/VolcanoPlot_MRBase_ggsave_199fdr_of39105.pdf"
      ) 
      
   
    
    #Looks like one can get some hints of where each of the association result is from, from here: https://gwas.mrcieu.ac.uk/datasets/ using the GWAS id (id column in the file downloaded from mrbase.org, here the data table MB).
    #For example, chronotype is listed twice within the FDR set (rank 170 and 199)
        #170: the first is from ukb and chronotype is clearly coded as listed in ukb showcase (i.e. 1 = Definitely a morning person ... 4 = Definitely an evening person) 
            #https://gwas.mrcieu.ac.uk/datasets/ukb-b-4956/ (order shown in PHESANT log at bottom)
            #https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100342
        #199: The second is by Samuel Jones and looks like he has at least in some of his papers been studying being a morning person and has flipped the variable coding (https://www.nature.com/articles/s41467-018-08259-7, table 1)
            #https://gwas.mrcieu.ac.uk/datasets/ieu-b-4862/

  
      
    #Creating excel of the fdr results.      
     #?write.xlsx
      
      # Save the data frame as an Excel file
      write.xlsx(MBfdr, file="MRBase_199Fdrsig_results.xlsx")
      
      # Ordering by trait and svaing the data frame as an Excel file
      MBfdr_trait<-MBfdr[order(MBfdr$trait),]
      write.xlsx(MBfdr_trait, file="MRBase_199Fdrsig_results_orderedbytrait.xlsx")    
          
#--------------------------          
  #FinnGen
#--------------------------
      
    FG<-read.table(file = 'FinnGen_19_40847202_T_C_phenotype_associations_2023Sep5.tsv',sep = '\t', header = TRUE, stringsAsFactors = FALSE)
      #->Error: "Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  : 
                #line 424 did not have 11 elements""
      #Couln't find what was wrong  
      lines <- readLines("FinnGen_19_40847202_T_C_phenotype_associations_2023Sep5.tsv")
      print(lines[424])
      print(lines[1])

    #TAKING A DIFFERENT STRATEGY

    #Checking the total number of association results:
      #Amount of results visible in the webbrowser
      20*113+12 # 2272

      #Amount of rows in the tsv file: 2273, note first row is the header

      #In the about section: 
      #"These results are from 2,269 binary endpoints and 3 quantitative endpoints (HEIGHT_IRN, WEIGHT_IRN, BMI_IRN) from freeze 9 (April 2022), consisting of 377,277 individuals. "
      2269+3 #2272
      

    #-->Thus, as I can calculate the FDR BH critical values myself knowing the n outcomes I'll only read a subset of the data in. 
    #I.e. I won't struggle to figure out what is wrong with the lines that R is giving error messages on as they are not needed.

      
    #Decided only to read in a subset (smallest p-values) as I'm just trying to get hold of the FDR BH sig results (and as the data is ordered by the p-values it's enough to take a slice of the data)
      #This was the largest subset I could read in without problems. Probably the apostrophe on line 40 is a problem and changing Chron's to Chrohn\'s didn't help. 
      data_subset <- read.table(text = lines[1:39], header = TRUE, sep = "\t", stringsAsFactors = FALSE,row.names = NULL)
      dim(data_subset) #38 11
      
      #Seeing if we can manage with this
      DataBH<-data_subset
      
      totaltests<-2272
      DataBH$bhcritical<-as.numeric(rownames(DataBH))*0.05/totaltests
      max(which(DataBH$pval <= DataBH$bhcritical)) #2
      #-->so our subset of 38 was plenty as only 2 reach the 0.05 FDR level of significance.
      cutoffFG<-DataBH$bhcritical[2]
      
    #Saving the fdr significant results  
      FGfdr<-DataBH[1:2,]

      # Save the data frame as an Excel file
      write.xlsx(FGfdr, file="FinnGen_2Fdrsig_results.xlsx")