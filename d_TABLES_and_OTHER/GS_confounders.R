######################################################################################################################

#29.8.2023
#Checking if GS is associated with age, i.e. if it might be a confounder in our PheWAS analyses.
#-->was not associated:

#----------------------------------------------------------------------
#Using R-4.1.1 interactively -opening R in bash
#----------------------------------------------------------------------

#cd /[PATH]/PHESANT/TRAIT
#RDir="[PATH]"
#${RDir}R-4.1.1/bin/R

#-----------------------------------
#Rscript dependencies -the libraries needed
#-----------------------------------

sessionInfo()
getwd()
#libraries
	library(dplyr)
	library(tidyr)
	library(plyr)
.libPaths("/[PATH]/ukbb/jadwiga/Rlib4.1.1/")
#install.packages("pastecs")
	library(pastecs)


#Reading in the data

	#A) Getting the smoking data
		D = read.csv("./zGRS10SNPs_SmokingVariables_343662_confounders_forDA.csv", as.is=T, header=T)
		dim(D)
		#[1] 343662     56
		str(D)
		#keeping wanted variables:
			W<-D[,c("eid","sex_31","Status_20116","EvNevExp","EvNev","WantToStop_3496","age_21022","AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897","AgeStartedSmoking_incurrent_3436","CPD_3456")]



	#B) Getting the GRS data
		G<-read.csv("/[PATH]/GRS9_topconfig/topconfigalleleA_GRS_topSNPCiealleleB.txt",as.is=T, header=T,sep="\t")
		dim(G)
		#[1] 343662     14
			#Keeping the wanted variables
				WG<-G[,c("userId","GRS","zGRS")]
				names(WG)<-c("eid","GRS","zGRS")	

	#C) Merging them together to form our Descptive Table

		DT<-join(WG,W,by="eid",type="left")


#Modelling 
		model_GS <- lm(GRS ~ age_21022, data=DT)
		summary(model_GS)

		model_zGS <- lm(zGRS ~ age_21022, data=DT)
		summary(model_zGS)

			> summary(model_GS)

			Call:
			lm(formula = GRS ~ age_21022, data = DT)

			Residuals:
 			    Min       1Q   Median       3Q      Max
			-2.93242 -0.30342  0.04249  0.42446  1.72706

			Coefficients:
		             		Estimate Std. Error t value Pr(>|t|)
			(Intercept) 1.1992064  0.0068506 175.052   <2e-16 ***
			age_21022   0.0000136  0.0001193   0.114    0.909
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			Residual standard error: 0.5587 on 343660 degrees of freedom
			Multiple R-squared:  3.783e-08, Adjusted R-squared:  -2.872e-06
			F-statistic: 0.013 on 1 and 343660 DF,  p-value: 0.9092

			>
			> model_zGS <- lm(zGRS ~ age_21022, data=DT)
			> summary(model_zGS)

			Call:
			lm(formula = zGRS ~ age_21022, data = DT)

			Residuals:
			    Min      1Q  Median      3Q     Max
			-5.2487 -0.5431  0.0760  0.7597  3.0912

			Coefficients:
			              Estimate Std. Error t value Pr(>|t|)
			(Intercept) -1.384e-03  1.226e-02  -0.113    0.910
			age_21022    2.434e-05  2.135e-04   0.114    0.909

			Residual standard error: 1 on 343660 degrees of freedom
			Multiple R-squared:  3.783e-08, Adjusted R-squared:  -2.872e-06
			F-statistic: 0.013 on 1 and 343660 DF,  p-value: 0.9092

######################################################################################################################
#1.9.2023 Doing the same for sex
		
	#modelling
		model_zGS <- lm(zGRS ~ sex_31, data=D)
		summary(model_zGS)
			Call:
			lm(formula = zGRS ~ sex_31, data = D)

			Residuals:
			    Min      1Q  Median      3Q     Max
			-5.2524 -0.5463  0.0778  0.7632  3.0879

			Coefficients:
			             Estimate Std. Error t value Pr(>|t|)
			(Intercept)  0.003552   0.002328   1.526   0.1270
			sex_31      -0.007672   0.003421  -2.243   0.0249 *
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			Residual standard error: 1 on 343660 degrees of freedom
			Multiple R-squared:  1.464e-05, Adjusted R-squared:  1.173e-05
			F-statistic:  5.03 on 1 and 343660 DF,  p-value: 0.02492

	
	#-->Based on this, it looks like men have ever so slightly lower values of GS than women and that the difference is stat. signif.


#####################################################################################################################
