#14.8.2022
#Sensitivity analyses of cessation

# 0) Plot histogram and boxplots of GS for current vs former: 
		# Sanity check: As positive association between GS and Cessation, former smokers should have bigger GS values --> checked
		# Mann & Whitney U test to test for differeces between former and current -->yes, statistically sig.

# 1) Reason stopped smoking comparison across slow, medium and fast metabolizers (Ever smokers)
		# Checking for statistically significant differences between slow and fast

# 2) Cessation (Ever smokers)
		#a) Cessation PheWAS model 
		#b) the same with CPD added to the model
		#c) (b)-step again once deleted those who stopped due to illness or doctor's advice
		#d) Cessation PHEWAS model with all the 4 reasons in the model 
		#e) (d)- step again with CPD in the model too

# 3) Unsuccessfull stop smoking attempts among Former smokers
		#0) distribution to determine whether we should use negative binomial or poisson
 		#a) Negative binomial regression: Number of unsuccessful stop-smoking attempts explained by the GS and the 4 reasons (+age, sex, 10 PCs) 
		#b) (a)-step with CPD in the model too



#Note why stopped/reduced coding: 
#1	Illness or ill health
#2	Doctor's advice
#3	Health precaution
#4	Financial reasons
#-7	None of the above
#-1	Do not know
#-3	Prefer not to answer
#    If code -7 was selected, then no additional choices were allowed.
#    If code -1 was selected, then no additional choices were allowed.
#    If code -3 was selected, then no additional choices were allowed.

#Note, for variables StoppedReason_Illness_6157 etc 1=Yes, 0= (none of the above, or listed some other reason(s)), NA=did not answer or said "Do not know" or "Prefer not to answer"


#----------------------------------------------------------------------
#ANALYSES
#----------------------------------------------------------------------

#DOing this interactively

#-------------
# Bash
#-------------
	#cd /[path]
	#RDir="/[path]"
	#${RDir}R-4.1.1/bin/R

#-----------------------------------
#Rscript
#-----------------------------------

	sessionInfo()
	getwd()

#-----------------
#Libraries:
#-----------------
	library(dplyr)
	library(tidyr)
	library(plyr)
	library(ggplot2)


#-----------------
# Getting data:
#-----------------

	#Reading in our final data set that is ready to be analysed
	D = read.csv("/[path]/PHESANT/TRAIT/zGRS10SNPs_SmokingVariables_343662_confounders_forDA.csv", as.is=T, header=T)
	str(D)
	summary(D)


#---------------------------------------------------------
# 0) Current vs Former NMR GS Distributions
#---------------------------------------------------------

#Formatting sex so it is easy to use in the plots
	D$Sex <- factor(D$sex_31, 
		levels = c(0,1), 
		labels = c("Female", "Male")) 

#Taking our Ever subset
	E<-subset(D,D$EvNev==1)

#Formatting Current-Former status so it is easy to use in the plots

	#> table(E$Status_20116)
	#
	#   -3     1     2
	#   28 81179 29141

	E$CurrentFormer<-ifelse(E$Status_20116==-3,NA,E$Status_20116)
	table(E$CurrentFormer)
	#    1     2
	# 81179 29141

	E$CurrentFormer<-ifelse(E$Status_20116==1,"Former",E$CurrentFormer)
	table(E$CurrentFormer)
	# 2 Former
	# 29141  81179

	E$CurrentFormer<-ifelse(E$Status_20116==2,"Current",E$CurrentFormer)
	table(E$CurrentFormer)
	# Current  Former
	#  29141   81179


#HISTOGRAMS: USING FACETING TO GET PANELS BY SEX
	E_complete=subset(E, !is.na(E$CurrentFormer))
	pdf("./Plots/zGRS_CurrentvsFormer__bysex.pdf")
		ggplot(E_complete, aes(x=zGRS, fill=CurrentFormer, colour=CurrentFormer,na.rm = TRUE)) +
		  geom_histogram(position="identity", alpha=0.5,na.rm = TRUE) + 
		facet_grid(Sex ~ .)
	dev.off()

#BOXPLOTS: 

	b <- ggplot(E_complete, aes(x=CurrentFormer, y=zGRS, fill=Sex)) + 
    		geom_boxplot()
	pdf("./Plots/boxplotzGRS_CurrentFormer_bysex.pdf")
		print(b)
	dev.off()

#Mann and whitney u-test to see if there is a statistically significant difference between the distributions

	test <- wilcox.test(E_complete$zGRS ~ E_complete$CurrentFormer)
		#        Wilcoxon rank sum test with continuity correction
		#
		#data:  E_complete$zGRS by E_complete$CurrentFormer
		#W = 1140290804, p-value < 2.2e-16
		#alternative hypothesis: true location shift is not equal to 0
	#-->There is a statistically significant difference between the GS distributions of current and former smokers.
	#Based on the boxplots, looks like former smokers do have slightly higher values than current smokers.

#--------------------------------------------------------
# 1) Reason stopped smoking comparison across slow, medium and fast metabolizers
#--------------------------------------------------------

#---------------------------------------------------------------------
#This I had done earlier:
#---------------------------------------------------------------------
#Creating the reason stopped variables 
#Then finalizing the why stopped and why reduced variables
#1	Illness or ill health
#2	Doctor's advice
#3	Health precaution
#4	Financial reasons
#-7	None of the above
#-1	Do not know
#-3	Prefer not to answer

	#First, I'll create a StoppedHealthAndOrFinancialReasons_6157=Yes/No/NA like the one above with reduced
		table(F$WhyStopped_6157_0_0)
		#   -7    -3    -1     1     2     3     4
		#16779    60   647  9937  3512 47769  5730

		#None of the above(-7)=No=0
		F$StoppedHealthAndOrFinancialReasons_6157<-ifelse(F$WhyStopped_6157_0_0==-7,0,F$WhyStopped_6157_0_0)
		F$StoppedHealthAndOrFinancialReasons_6157<-ifelse(F$StoppedHealthAndOrFinancialReasons_6157<0,NA,F$StoppedHealthAndOrFinancialReasons_6157)
		F$StoppedHealthAndOrFinancialReasons_6157<-ifelse(F$StoppedHealthAndOrFinancialReasons_6157>0,1,F$StoppedHealthAndOrFinancialReasons_6157)
		table(F$StoppedHealthAndOrFinancialReasons_6157)
		#    0     1
		#16779 66948
		#9937 +3512+ 47769+  5730 #=66948

	#Then creating one reson at a time Yes/No
		#First replacing missing values so we don't loose most of our data when we go through F$WhyStopped_6157_0_1 to F$WhyStopped_6157_0_3
			F[, 40:43][is.na(F[, 40:43])] <- -3

#> table(F$WhyStopped_6157_0_0)
#    -7     -3     -1      1      2      3      4
# 16779 259288    647   9937   3512  47769   5730

#> table(F$WhyStopped_6157_0_1)
#    -3      2      3      4
#324316   2458   3613  13275

#> table(F$WhyStopped_6157_0_2)
#    -3      3      4
#341007   1246   1409

#> table(F$WhyStopped_6157_0_3)
#    -3      4
#343266    396

		#Due to Ilness: we see from the above tables that we want to end up with 9937 with ones
		F$StoppedReason_Illness_6157<-ifelse(F$WhyStopped_6157_0_0==-7,0,F$WhyStopped_6157_0_0)
		F$StoppedReason_Illness_6157<-ifelse(F$StoppedReason_Illness_6157<0,NA,F$StoppedReason_Illness_6157)
		F$StoppedReason_Illness_6157<-ifelse(F$StoppedReason_Illness_6157>1,0,F$StoppedReason_Illness_6157)
		
		table(F$StoppedReason_Illness_6157)
		#0     1
		#73790  9937

		#Due to Doctor's advice: We want to end up with 3512+2458=5970 cases
		F$StoppedReason_Doctor_6157<-ifelse(F$WhyStopped_6157_0_0==-7,0,F$WhyStopped_6157_0_0)
		F$StoppedReason_Doctor_6157<-ifelse(F$StoppedReason_Doctor_6157<0,NA,F$StoppedReason_Doctor_6157)
		F$StoppedReason_Doctor_6157<-ifelse(F$StoppedReason_Doctor_6157==2,1,0)
		table(F$StoppedReason_Doctor_6157)
		F$StoppedReason_Doctor_6157<-ifelse(F$WhyStopped_6157_0_1==2,1,F$StoppedReason_Doctor_6157)
		table(F$StoppedReason_Doctor_6157)
		#    0     1
		#77757  5970

		#Due to Health precaution: should have 47769+3613+1246= 52628
		F$StoppedReason_Health_6157<-ifelse(F$WhyStopped_6157_0_0==-7,0,F$WhyStopped_6157_0_0)
		F$StoppedReason_Health_6157<-ifelse(F$StoppedReason_Health_6157<0,NA,F$StoppedReason_Health_6157)
		F$StoppedReason_Health_6157<-ifelse(F$StoppedReason_Health_6157==3,1,0)
		table(F$StoppedReason_Health_6157)
		F$StoppedReason_Health_6157<-ifelse(F$WhyStopped_6157_0_1==3|F$WhyStopped_6157_0_2==3,1,F$StoppedReason_Health_6157)
		table(F$StoppedReason_Health_6157)
		#     0     1
		# 31099 52628

		#Due to Financial reasons: should have 5730+13275+1409+396=20810
		F$StoppedReason_Financial_6157<-ifelse(F$WhyStopped_6157_0_0==-7,0,F$WhyStopped_6157_0_0)
		F$StoppedReason_Financial_6157<-ifelse(F$StoppedReason_Financial_6157<0,NA,F$StoppedReason_Financial_6157)
		F$StoppedReason_Financial_6157<-ifelse(F$StoppedReason_Financial_6157==4,1,0)
		table(F$StoppedReason_Financial_6157)
		F$StoppedReason_Financial_6157<-ifelse(F$WhyStopped_6157_0_1==4|F$WhyStopped_6157_0_2==4|F$WhyStopped_6157_0_3==4,1,F$StoppedReason_Financial_6157)
		table(F$StoppedReason_Financial_6157)
		#    0     1
		#62917 20810
#Saving this for data-analysis (descriptives and secondary analysis)
write.table(F,"./zGRS10SNPs_SmokingVariables_343662_confounders_forDA.csv",sep=",",quote=FALSE,row.names=FALSE, col.names=TRUE)

#---------------------------------------------------------------------


#--------------------------------------------------------
#We divide the data into three equal sized groups
#--------------------------------------------------------

	tert<-quantile(E$zGRS,probs = seq(0, 1, 1/3))
		# > tert
	       # 0%  33.33333%  66.66667%       100%
		# -5.2488708 -0.2652350  0.5154701  2.9270147


	E$MetabolizerGroup<-with(E, 
               cut(zGRS, 
                   tert, 
                   include.lowest = T, 
                   labels = c("Slow", "Medium", "Fast")))

	tapply(E$zGRS, E$MetabolizerGroup, summary)

			#$Slow
		 	#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
			#-5.2489 -1.4773 -1.0082 -1.1484 -0.5652 -0.2653

			#$Medium
			#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
			#-0.26517 -0.12453  0.07369  0.09696  0.31098  0.51525

			#$Fast
			#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
			# 0.5156  0.7585  0.9457  1.0308  1.2028  2.9270

	#> dim(E)
	#[1] 110348     59

	#table(E$MetabolizerGroup)
	#  Slow Medium   Fast
	# 36783  36782  36783

	#table(E$CurrentFormer)
	# Current  Former
  	# 29141   81179


#----------------------------------------------------------
#Note: The reason stopped smoking has only been asked from a subset:

		Data-Field 6157
		Description:	Why stopped smoking

		"ACE touchscreen question "Why did you stop smoking? (You can select more than one answer)"

		The following checks were performed:

    			If code -7 was selected, then no additional choices were allowed.
    			If code -1 was selected, then no additional choices were allowed.
    			If code -3 was selected, then no additional choices were allowed.

		Field 6157 was collected from participants who indicated that in the past they smoked tobacco on most or all days, 
		as defined by their answers to Field 1249 and that during the time they smoked they stopped for more than 6 months, as defined by their answers to Field 1249" (should probably have 2907 here)


		# Data-Field 2907 Ever stopped smoking for +6 months
		#ACE touchscreen question "In the time that you smoked, did you ever stop for more than 6 months?"
		#Field 2907 was collected from participants who indicated that in the past they smoked tobacco on most or all days, as defined by their answers to Field 1249 

#Note: same explanation of who was asked the question applies to the variable: 2926 Number of unsuccessful stop-smoking attempts


#----------------------------------------------------------

#--------------------------------------------------------
#We see if there are differences in the reasons listed between the slow and fast metabolizers
#--------------------------------------------------------
#table(E$MetabolizerGroup)
	#  Slow Medium   Fast
	# 36783  36782  36783

num_obs<-c(36783, 36782, 36783)

#ILLNESS
	#frequency table
	freq_cesI<-table(E$StoppedReason_Illness_6157,E$MetabolizerGroup)
		#    Slow Medium  Fast
		#  0 24075  24800 24915
		#  1  3319   3221  3397


	cesI<-c(3319, 3221, 3397)
	prop.test(cesI,num_obs)
	
	
	       # 3-sample test for equality of proportions without continuity
	       # correction

		# data:  cesI out of num_obs
		# X-squared = 5.1553, df = 2, p-value = 0.07595
		# alternative hypothesis: two.sided
		# sample estimates:
		#    prop 1     prop 2     prop 3
		# 0.09023190 0.08757001 0.09235245
		
		#->No statistically significant differences

	#Just lowest versus highest
	prop.test(cesI[c(1,3)],num_obs[c(1,3)])

	       # 2-sample test for equality of proportions with continuity correction

		# data:  cesI[c(1, 3)] out of num_obs[c(1, 3)]
		# X-squared = 0.97151, df = 1, p-value = 0.3243
		# alternative hypothesis: two.sided
		# 95 percent confidence interval:
		#  -0.006310339  0.002069249
		# sample estimates:
		#    prop 1     prop 2
		# 0.09023190 0.09235245

		#->No statistically significant differences


#DOCTOR
	freq_cesD<-table(E$StoppedReason_Doctor_6157,E$MetabolizerGroup)
		#        Slow Medium  Fast
		#  	0 25414  26053 26290
		#  	1  1980   1968  2022
	
	cesD<-c(1980, 1968, 2022)
	prop.test(cesD,num_obs)
	
	       # 3-sample test for equality of proportions without continuity
 	       #correction

		# data:  cesD out of num_obs
		# X-squared = 0.85299, df = 2, p-value = 0.6528
		# alternative hypothesis: two.sided
		# sample estimates:
		#    prop 1     prop 2     prop 3
		# 0.05382921 0.05350443 0.05497105

		#->No statistically significant differences


	#Just lowest versus highest
	prop.test(cesD[c(1,3)],num_obs[c(1,3)])

	       # 2-sample test for equality of proportions with continuity correction

		# data:  cesD[c(1, 3)] out of num_obs[c(1, 3)]
		# X-squared = 0.4442, df = 1, p-value = 0.5051
		# alternative hypothesis: two.sided
		# 95 percent confidence interval:
		# -0.004446889  0.002163225
		# sample estimates:
		#    prop 1     prop 2
		# 0.05382921 0.05497105
	
		#->No statistically significant differences



#HEALTH PRECAUTION
	freq_cesH<-table(E$StoppedReason_Health_6157,E$MetabolizerGroup)
		# 	Slow Medium  Fast
		#  0 10025  10416 10658
		#  1 17369  17605 17654
	
	cesH<-c(17369, 17605, 17654)
	prop.test(cesH,num_obs)
	
		#             	3-sample test for equality of proportions without continuity
		#        	correction

		# data:  cesH out of num_obs
		# X-squared = 5.0675, df = 2, p-value = 0.07936
		# alternative hypothesis: two.sided
		# sample estimates:
		#   prop 1    prop 2    prop 3
		# 0.4722018 0.4786309 0.4799500

	#Just lowest versus highest
	prop.test(cesH[c(1,3)],num_obs[c(1,3)])

		#        2-sample test for equality of proportions with continuity correction

		# data:  cesH[c(1, 3)] out of num_obs[c(1, 3)]
		# X-squared = 4.3956, df = 1, p-value = 0.03603
		# alternative hypothesis: two.sided
		# 95 percent confidence interval:
		#  -0.0149930295 -0.0005032596
		# sample estimates:
		#    prop 1    prop 2
		# 0.4722018 0.4799500

		#->Statistically significant (at the 0.05 level) differences. 



#FINANCIAL
	freq_cesF<-table(E$StoppedReason_Financial_6157,E$MetabolizerGroup)
	#            Slow Medium  Fast
	#	  0 20640  21042 21235
	#	  1  6754   6979  7077
	
	cesF<-c(6754, 6979, 7077)
	prop.test(cesF,num_obs)
	
		#	3-sample test for equality of proportions without continuity
        	#	correction

		# data:  cesF out of num_obs
		# X-squared = 9.7483, df = 2, p-value = 0.007642
		# alternative hypothesis: two.sided
		# sample estimates:
		#   prop 1    prop 2    prop 3
		# 0.1836174 0.1897395 0.1923987

	#Just lowest versus highest
	prop.test(cesF[c(1,3)],num_obs[c(1,3)])
		#        2-sample test for equality of proportions with continuity correction

		# data:  cesF[c(1, 3)] out of num_obs[c(1, 3)]
		# X-squared = 9.2322, df = 1, p-value = 0.002378
		# alternative hypothesis: two.sided
		# 95 percent confidence interval:
		#  -0.014454876 -0.003107585
		# sample estimates:
		#    prop 1    prop 2
		# 0.1836174 0.1923987
#----------------------------------------------------------




#---------------------------------------------

#2) Cessation PheWAS model with CPD added to the model, and again once deleted those who stopped due to illness or doctor's advice.

#---------------------------------------------

	#Creating our Cessation variable (coding former=1 and current=0, never and prefer not to answer as NA)
	E$Cessation <- ifelse(E$Status_20116<1,NA,E$Status_20116)
	E$Cessation <- ifelse(E$Cessation==1,1,0)
	dim(E)

	summary(mc<-glm(Cessation~sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009+zGRS,data=E,family=binomial(link="logit")  ))
#Coefficients:
              Estimate Std. Error z value Pr(>|z|)
(Intercept) -2.3683193  0.0753259 -31.441  < 2e-16 ***
sex_31      -0.0049312  0.0140872  -0.350 0.726305
age_21022    0.0595155  0.0008870  67.094  < 2e-16 ***
PC1_22009   -0.0022636  0.0045758  -0.495 0.620816
PC2_22009   -0.0008395  0.0047552  -0.177 0.859869
PC3_22009    0.0124464  0.0045860   2.714 0.006647 **
PC4_22009   -0.0053774  0.0034217  -1.572 0.116051
PC5_22009   -0.0146124  0.0014679  -9.955  < 2e-16 ***
PC6_22009    0.0026977  0.0043728   0.617 0.537284
PC7_22009   -0.0027591  0.0039100  -0.706 0.480401
PC8_22009   -0.0041673  0.0038904  -1.071 0.284083
PC9_22009   -0.0055845  0.0016002  -3.490 0.000483 ***
PC10_22009  -0.0072452  0.0034171  -2.120 0.033983 *
zGRS         0.0719700  0.0069115  10.413  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	> exp(0.0719700)
	[1] 1.074623


#Getting OR
	est <- cbind(Estimate = coef(mc), confint(mc))
	cbind(exp(est), pvalue = summary(mc)$coefficients[,4])


              Estimate      2.5 %    97.5 %        pvalue
(Intercept) 0.09363797 0.08078161 0.1085298 5.579120e-217
sex_31      0.99508099 0.96797625 1.0229324  7.263047e-01
age_21022   1.06132223 1.05947967 1.0631701  0.000000e+00
PC1_22009   0.99773892 0.98883134 1.0067281  6.208156e-01
PC2_22009   0.99916087 0.98989150 1.0085162  8.598687e-01
PC3_22009   1.01252420 1.00346433 1.0216666  6.647369e-03
PC4_22009   0.99463700 0.98798681 1.0013279  1.160513e-01
PC5_22009   0.98549381 0.98266310 0.9883336  2.398910e-23
PC6_22009   1.00270132 0.99414467 1.0113323  5.372836e-01
PC7_22009   0.99724466 0.98962992 1.0049150  4.804009e-01
PC8_22009   0.99584135 0.98827792 1.0034648  2.840834e-01
PC9_22009   0.99443103 0.99131144 0.9975492  4.830990e-04
PC10_22009  0.99278094 0.98615432 0.9994527  3.398270e-02
zGRS        1.07462306 1.06015785 1.0892732  2.161406e-25



#---------------------------------------------
# Including CPD to the model
#---------------------------------------------

#To include cpd I need to combined CPDcurrent and CPDformer as they've been asked from current and former respectively
	summary(E$CPD_3456)
	E$CPD<-ifelse(E$CPD_3456==-10,0.5,E$CPD_3456)
	E$CPD<-ifelse(E$CPD<0,NA,E$CPD)


	summary(E$CPD_previously_2887) #-10=less than one a day, -1=Do not know
	E$CPDprevious<-ifelse(E$CPD_previously_2887==-10,0.5,E$CPD_previously_2887)
	E$CPDprevious<-ifelse(E$CPDprevious==-1,NA,E$CPDprevious)
	summary(E$CPDprevious)

	summary(E$CPD)

	E$CPDcombined<-ifelse(is.na(E$CPDprevious),E$CPD,E$CPDprevious)

	summary(mc2<-glm(Cessation~sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009+zGRS+CPDcombined,data=E,family=binomial(link="logit")  ))
Coefficients:
              Estimate Std. Error z value Pr(>|z|)
(Intercept) -2.9615404  0.0799723 -37.032  < 2e-16 ***
sex_31      -0.0707968  0.0150552  -4.702 2.57e-06 ***
age_21022    0.0591007  0.0009276  63.711  < 2e-16 ***
PC1_22009   -0.0006470  0.0048245  -0.134  0.89332
PC2_22009   -0.0020161  0.0050129  -0.402  0.68755
PC3_22009    0.0157778  0.0048398   3.260  0.00111 **
PC4_22009   -0.0059652  0.0036068  -1.654  0.09815 .
PC5_22009   -0.0178390  0.0015457 -11.541  < 2e-16 ***
PC6_22009    0.0048687  0.0046073   1.057  0.29063
PC7_22009   -0.0018766  0.0041230  -0.455  0.64899
PC8_22009   -0.0047369  0.0041110  -1.152  0.24921
PC9_22009   -0.0079630  0.0016935  -4.702 2.58e-06 ***
PC10_22009  -0.0105889  0.0036062  -2.936  0.00332 **
zGRS         0.0492000  0.0072847   6.754 1.44e-11 ***
CPDcombined  0.0422642  0.0009240  45.738  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	exp(0.0492000) #1.05043

#Getting OR
	est <- cbind(Estimate = coef(mc2), confint(mc2))
	cbind(exp(est), pvalue = summary(mc2)$coefficients[,4])

Variable     OR      95CI_lower  95CI_upper   pvalue
(Intercept) 0.05173916 0.0442294 0.06051432 3.490681e-300
sex_31      0.93165114 0.9045571 0.95954718  2.570339e-06
age_21022   1.06088203 1.0589561 1.06281385  0.000000e+00
PC1_22009   0.99935320 0.9899486 1.00884872  8.933160e-01
PC2_22009   0.99798598 0.9882281 1.00783908  6.875537e-01
PC3_22009   1.01590294 1.0063123 1.02558619  1.114040e-03
PC4_22009   0.99405254 0.9870479 1.00110261  9.815411e-02
PC5_22009   0.98231914 0.9793479 0.98529998  8.196749e-31
PC6_22009   1.00488061 0.9958476 1.01399642  2.906267e-01
PC7_22009   0.99812513 0.9900902 1.00622205  6.489897e-01
PC8_22009   0.99527428 0.9872882 1.00332718  2.492125e-01
PC9_22009   0.99206862 0.9887749 0.99536101  2.576382e-06
PC10_22009  0.98946698 0.9824982 0.99648552  3.321152e-03
zGRS        1.05043043 1.0355321 1.06552872  1.438948e-11
CPDcombined 1.04317003 1.0412875 1.04506609  0.000000e+00




#Ever data once those who stopped due to illness or doctor's advice were excluded:
	E_exclID<-subset(E, !(E$StoppedReason_Illness_6157==1) & !(E$StoppedReason_Doctor_6157==1)) # dim(E_exclID) 70278    60

	summary(mc3<-glm(Cessation~sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009+zGRS+CPDcombined,data=E_exclID,family=binomial(link="logit")  ))
Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -1.047979   0.221117  -4.739 2.14e-06 ***
sex_31      -0.379971   0.042913  -8.855  < 2e-16 ***
age_21022    0.059601   0.002536  23.505  < 2e-16 ***
PC1_22009   -0.010896   0.013723  -0.794 0.427207
PC2_22009    0.001289   0.014235   0.091 0.927859
PC3_22009    0.006717   0.013851   0.485 0.627734
PC4_22009    0.009037   0.010194   0.887 0.375320
PC5_22009   -0.015583   0.004412  -3.532 0.000413 ***
PC6_22009    0.004353   0.013181   0.330 0.741192
PC7_22009    0.016206   0.011624   1.394 0.163244
PC8_22009    0.006038   0.011644   0.519 0.604078
PC9_22009    0.006322   0.004653   1.359 0.174265
PC10_22009   0.002718   0.010303   0.264 0.791916
zGRS         0.012794   0.020870   0.613 0.539855
CPDcombined  0.062420   0.002998  20.822  < 2e-16 ***


	> exp(0.012794)
	[1] 1.012876

#Getting OR
	est <- cbind(Estimate = coef(mc3), confint(mc3))
	cbind(exp(est), pvalue = summary(mc3)$coefficients[,4])

Variable     OR      95CI_lower  95CI_upper   pvalue
(Intercept) 0.3506456 0.2274797 0.5412356  2.142611e-06
sex_31      0.6838812 0.6286395 0.7438148  8.405331e-19
age_21022   1.0614124 1.0561501 1.0667009 3.612351e-122
PC1_22009   0.9891630 0.9629255 1.0161454  4.272067e-01
PC2_22009   1.0012896 0.9737330 1.0296120  9.278588e-01
PC3_22009   1.0067393 0.9797763 1.0344462  6.277337e-01
PC4_22009   1.0090782 0.9890802 1.0294027  3.753204e-01
PC5_22009   0.9845381 0.9760801 0.9931084  4.126047e-04
PC6_22009   1.0043629 0.9787537 1.0306552  7.411918e-01
PC7_22009   1.0163384 0.9934186 1.0397302  1.632436e-01
PC8_22009   1.0060561 0.9833826 1.0293067  6.040785e-01
PC9_22009   1.0063418 0.9971127 1.0154696  1.742653e-01
PC10_22009  1.0027219 0.9826861 1.0231877  7.919161e-01
zGRS        1.0128764 0.9721230 1.0550009  5.398553e-01
CPDcombined 1.0644096 1.0582132 1.0707209  2.712445e-96

	

	



#------------------------------------------------------------------
# 2 d-e) So among those who had managed to quit for at least 6 months, modelling the likelihood of still being a former smoker: 
#------------------------------------------------------------------


summary(m_d<-glm(Cessation~sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009+StoppedReason_Illness_6157 +StoppedReason_Doctor_6157 +StoppedReason_Health_6157 + StoppedReason_Financial_6157+zGRS,data=E,family=binomial(link="logit")  ))


Deviance Residuals:
    Min       1Q   Median       3Q      Max
-2.9473   0.2178   0.2513   0.3037   0.6424

Coefficients:
                              Estimate Std. Error z value Pr(>|z|)
(Intercept)                  -0.084395   0.192345  -0.439  0.66083
sex_31                       -0.237707   0.036899  -6.442 1.18e-10 ***
age_21022                     0.059443   0.002222  26.750  < 2e-16 ***
PC1_22009                     0.001107   0.011795   0.094  0.92525
PC2_22009                     0.002889   0.012249   0.236  0.81354
PC3_22009                     0.004754   0.011859   0.401  0.68848
PC4_22009                     0.006140   0.008786   0.699  0.48462
PC5_22009                    -0.011515   0.003786  -3.041  0.00236 **
PC6_22009                     0.001462   0.011292   0.129  0.89699
PC7_22009                     0.015219   0.009992   1.523  0.12774
PC8_22009                     0.001039   0.009966   0.104  0.91696
PC9_22009                     0.005264   0.004019   1.310  0.19020
PC10_22009                    0.002497   0.008813   0.283  0.77693
StoppedReason_Illness_6157   -0.034565   0.057025  -0.606  0.54442
StoppedReason_Doctor_6157    -0.489466   0.062555  -7.825 5.09e-15 ***
StoppedReason_Health_6157     0.055221   0.038069   1.451  0.14690
StoppedReason_Financial_6157  0.186555   0.043941   4.246 2.18e-05 ***
zGRS                          0.039862   0.017954   2.220  0.02641 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 27366  on 83703  degrees of freedom
Residual deviance: 26562  on 83686  degrees of freedom
  (26644 observations deleted due to missingness)
AIC: 26598

Number of Fisher Scoring iterations: 6


#Getting OR
	est <- cbind(Estimate = coef(m_d), confint(m_d))
	cbind(exp(est), pvalue = summary(m_d)$coefficients[,4])

Variable     OR      95CI_lower  95CI_upper   pvalue
(Intercept)                  0.9190685 0.6308219 1.3408016  6.608304e-01
sex_31                       0.7884334 0.7333229 0.8474573  1.177975e-10
age_21022                    1.0612454 1.0566297 1.0658747 1.247775e-157
PC1_22009                    1.0011072 0.9782423 1.0245333  9.252496e-01
PC2_22009                    1.0028934 0.9790964 1.0272575  8.135394e-01
PC3_22009                    1.0047657 0.9816810 1.0283924  6.884760e-01
PC4_22009                    1.0061593 0.9889553 1.0236086  4.846201e-01
PC5_22009                    0.9885515 0.9812569 0.9959285  2.355269e-03
PC6_22009                    1.0014629 0.9795469 1.0238783  8.969903e-01
PC7_22009                    1.0153355 0.9956236 1.0353949  1.277356e-01
PC8_22009                    1.0010397 0.9816950 1.0208061  9.169555e-01
PC9_22009                    1.0052782 0.9973205 1.0131573  1.901980e-01
PC10_22009                   1.0025001 0.9853402 1.0199756  7.769288e-01
StoppedReason_Illness_6157   0.9660256 0.8647928 1.0814636  5.444211e-01
StoppedReason_Doctor_6157    0.6129534 0.5429859 0.6939245  5.094011e-15
StoppedReason_Health_6157    1.0567745 0.9806037 1.1384360  1.469044e-01
StoppedReason_Financial_6157 1.2050903 1.1063263 1.3143163  2.180210e-05
zGRS                         1.0406666 1.0045627 1.0778140  2.640858e-02


> dim(E)
[1] 110348     63
> dim(E)[1]
[1] 110348
> dim(E)[1]-26644
[1] 83704

#-------------------------------------
#Including CPD
#-------------------------------------

#To include cpd I need to combined CPDcurrent and CPDformer as they've been asked from current and former respectively
	summary(E$CPD_3456)
	E$CPD<-ifelse(E$CPD_3456==-10,0.5,E$CPD_3456)
	E$CPD<-ifelse(E$CPD<0,NA,E$CPD)


	summary(E$CPD_previously_2887) #-10=less than one a day, -1=Do not know
	E$CPDprevious<-ifelse(E$CPD_previously_2887==-10,0.5,E$CPD_previously_2887)
	E$CPDprevious<-ifelse(E$CPDprevious==-1,NA,E$CPDprevious)
	summary(E$CPDprevious)

	summary(E$CPD)

	E$CPDcombined<-ifelse(is.na(E$CPDprevious),E$CPD,E$CPDprevious)


summary(m_e<-glm(Cessation~sex_31+age_21022+CPDcombined+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009+StoppedReason_Illness_6157 +StoppedReason_Doctor_6157 +StoppedReason_Health_6157 + StoppedReason_Financial_6157+zGRS,data=E,family=binomial(link="logit")  ))

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)
(Intercept)                  -0.7944089  0.2022191  -3.928 8.55e-05 ***
sex_31                       -0.3573780  0.0387688  -9.218  < 2e-16 ***
age_21022                     0.0565564  0.0022934  24.661  < 2e-16 ***
CPDcombined                   0.0624467  0.0026564  23.508  < 2e-16 ***
PC1_22009                     0.0003135  0.0123675   0.025  0.97978
PC2_22009                    -0.0019141  0.0128374  -0.149  0.88147
PC3_22009                     0.0120971  0.0124653   0.970  0.33182
PC4_22009                     0.0062522  0.0092161   0.678  0.49752
PC5_22009                    -0.0157604  0.0039576  -3.982 6.83e-05 ***
PC6_22009                     0.0025612  0.0118506   0.216  0.82889
PC7_22009                     0.0133503  0.0104980   1.272  0.20348
PC8_22009                     0.0014888  0.0104861   0.142  0.88710
PC9_22009                     0.0043777  0.0041885   1.045  0.29594
PC10_22009                    0.0002412  0.0092427   0.026  0.97918
StoppedReason_Illness_6157   -0.1825260  0.0596599  -3.059  0.00222 **
StoppedReason_Doctor_6157    -0.6646138  0.0659498 -10.078  < 2e-16 ***
StoppedReason_Health_6157     0.0798537  0.0400547   1.994  0.04619 *
StoppedReason_Financial_6157  0.0373870  0.0458729   0.815  0.41506
zGRS                          0.0093306  0.0187654   0.497  0.61903
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 25326  on 79227  degrees of freedom
Residual deviance: 23866  on 79209  degrees of freedom
  (31120 observations deleted due to missingness)
AIC: 23904

Number of Fisher Scoring iterations: 7

>
>
> getOption("na.action")
[1] "na.omit"
> dim(E)
[1] 110348     61
>
> 110348 -31120
[1] 79228

#--> men are less likely to be former smokers, age is positively associated with being a former smoker, as is cigarettes smoked per day, 
# Illness and doctor's advice have clearly not been effective enough reasons as listing them makes it less likely to be a former smoker,
# the reasons that " have come from within" (health or financial reasons) were not statistically significant
#zGS has a positive association which is attenuated once CPD is added to the model.


#Getting OR
	est <- cbind(Estimate = coef(m_e), confint(m_e))
	cbind(exp(est), pvalue = summary(m_e)$coefficients[,4])

Variable     OR      95CI_lower  95CI_upper   pvalue
(Intercept)                  0.4518483 0.3041752 0.6720362  8.549310e-05
sex_31                       0.6995080 0.6482608 0.7546681  3.021958e-20
age_21022                    1.0581863 1.0534387 1.0629523 2.818551e-134
CPDcombined                  1.0644377 1.0589430 1.0700267 3.371377e-122
PC1_22009                    1.0003135 0.9763696 1.0248708  9.797778e-01
PC2_22009                    0.9980877 0.9732822 1.0235136  8.814721e-01
PC3_22009                    1.0121706 0.9877415 1.0372053  3.318161e-01
PC4_22009                    1.0062718 0.9882302 1.0245844  4.975218e-01
PC5_22009                    0.9843631 0.9767713 0.9920430  6.825658e-05
PC6_22009                    1.0025644 0.9795504 1.0261284  8.288940e-01
PC7_22009                    1.0134398 0.9927791 1.0344858  2.034817e-01
PC8_22009                    1.0014899 0.9811370 1.0223068  8.870991e-01
PC9_22009                    1.0043873 0.9961001 1.0125918  2.959396e-01
PC10_22009                   1.0002413 0.9822932 1.0185357  9.791778e-01
StoppedReason_Illness_6157   0.8331630 0.7420512 0.9376097  2.217498e-03
StoppedReason_Doctor_6157    0.5144722 0.4527541 0.5863664  6.942303e-24
StoppedReason_Health_6157    1.0831286 1.0011293 1.1713483  4.619381e-02
StoppedReason_Financial_6157 1.0380947 0.9494171 1.1364845  4.150647e-01
zGRS                         1.0093743 0.9727977 1.0470564  6.190316e-01









#
#-----------------------------------------------------
# 3) Unsuccessfull stop smoking attempts
	#distribution to determine whether we should use negative binomial or poisson
#-----------------------------------------------------

#Checking 2926 Number of unsuccessful stop-smoking attempts

		# Ever (N = 84434)
			> dim(E)
			[1] 110348     61
			summary(E$NumberofUnsuccessfulStopAttempts_2926)
			#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
			# -3.000   0.000   1.000   2.527   3.000 200.000   25914

		# Former (N = 81103)
			> dim(Former)
			[1] 81179    57
			summary(Former$NumberofUnsuccessfulStopAttempts_2926)
			#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
			# -3.000   0.000   1.000   2.516   3.000 200.000      76

		#Current ( N = 3303)
			> dim(Current)
			[1] 29141    57

			summary(Current$NumberofUnsuccessfulStopAttempts_2926)
			#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
			# -3.000   0.000   2.000   2.814   4.000 200.000   25838




		#Let's check the stop attempts is properly coded:
		#Value type integer
		#-1	Do not know
		#-3	Prefer not to answer

		#summary(D$NumberofUnsuccessfulStopAttempts_2926)
   		#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
  		#-3.00    0.00    1.00    2.53    3.00  200.00  259228
	

		D$NumberofUnsuccessfulStopAttempts_2926<-ifelse(D$NumberofUnsuccessfulStopAttempts_2926<0,NA,D$NumberofUnsuccessfulStopAttempts_2926)
		D$NumberofUnsuccessfulStopAttempts_2926<-as.numeric(D$NumberofUnsuccessfulStopAttempts_2926)

	#summary(D$NumberofUnsuccessfulStopAttempts_2926)

	#We take our Ever subset
		E<-subset(D,D$EvNev==1)


	#Creating our Former/Current subsets: Note, reason stopped has only bee asked from the former smokers.
		#Smoking status 20116:
		#-3	Prefer not to answer
		#0	Never
		#1	Previous
		#2	Current

		table(D$Status_20116)
		#    -3      0      1      2
		#  1188 187078 120786  34610


	Former<-subset(E,E$Status_20116==1)
	Current<-subset(E,E$Status_20116==2)

	#DISTRIBUTION
	summary(Former$NumberofUnsuccessfulStopAttempts_2926)
	# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
	#  0.000   0.000   2.000   2.851   3.000 200.000    7071

	#POISSON REGRESSION OR NEGATIVE BINOMIAL?
	#To assess which reason for stopping was the most important I want to have them in the same model with y=unsuccessfull stop-attempts.
	#Based on the variance being a lot bigger than the mean it looks like negative binomial would be better.

	var(Former$NumberofUnsuccessfulStopAttempts_2926,na.rm=TRUE)
		#> var(Former$NumberofUnsuccessfulStopAttempts_2926,na.rm=TRUE)
		#[1] 50.38089


	#N for non missing number of unsuccessfullstopattempts among former smokers
		dim(subset(Former,!(is.na(Former$NumberofUnsuccessfulStopAttempts_2926)))) # 74108    56


	#Distribution Once we remove those who stopped due to illness or doctor's advice:
		dim(Former) # 81179    56
		test<-subset(Former, !(Former$StoppedReason_Illness_6157==1)) #dim(test) 70968    56
		testwanted<-subset(test, !(test$StoppedReason_Doctor_6157==1)) #67646    56
		Former_exclID<-subset(Former, !(Former$StoppedReason_Illness_6157==1) & !(Former$StoppedReason_Doctor_6157==1)) # dim(Former_exclID) 67646    56

	summary(Former_exclID$NumberofUnsuccessfulStopAttempts_2926)
	#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
	#  0.000   0.000   2.000   2.777   3.000 200.000    5752

#------------------------------------------------------------

#3) Negative binomial regression: Number of unsuccessful stop-smoking attempts explained by the reasons, GS, and CPD (+age, sex, 10 PCs) 

#------------------------------------------------------------
	library("MASS")
	summary(m1 <- glm.nb(NumberofUnsuccessfulStopAttempts_2926 ~ zGRS +StoppedReason_Illness_6157 +StoppedReason_Doctor_6157 +StoppedReason_Health_6157 + StoppedReason_Financial_6157+ sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009, data = Former))
	#Coefficients:
                               Estimate Std. Error z value Pr(>|z|)
(Intercept)                   1.1233742  0.0551935  20.353  < 2e-16 ***
zGRS                          0.0117397  0.0048696   2.411 0.015917 *
StoppedReason_Illness_6157    0.2984778  0.0157107  18.998  < 2e-16 ***
StoppedReason_Doctor_6157     0.0710434  0.0194368   3.655 0.000257 ***
StoppedReason_Health_6157     0.3301750  0.0104104  31.716  < 2e-16 ***
StoppedReason_Financial_6157  0.1385504  0.0111218  12.458  < 2e-16 ***
sex_31                        0.3183497  0.0098462  32.332  < 2e-16 ***
age_21022                    -0.0102101  0.0006542 -15.606  < 2e-16 ***
PC1_22009                    -0.0024257  0.0031725  -0.765 0.444498
PC2_22009                     0.0043765  0.0032888   1.331 0.183282
PC3_22009                     0.0035074  0.0031804   1.103 0.270104
PC4_22009                     0.0012774  0.0023668   0.540 0.589381
PC5_22009                     0.0057087  0.0010294   5.546 2.93e-08 ***
PC6_22009                     0.0045652  0.0030244   1.509 0.131189
PC7_22009                    -0.0021326  0.0027011  -0.790 0.429785
PC8_22009                     0.0004876  0.0026893   0.181 0.856116
PC9_22009                    -0.0013003  0.0010955  -1.187 0.235215
PC10_22009                   -0.0023331  0.0023515  -0.992 0.321129
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(0.7394) family taken to be 1)

    Null deviance: 81032  on 73592  degrees of freedom
Residual deviance: 78265  on 73575  degrees of freedom
  (7586 observations deleted due to missingness)
AIC: 319495

Number of Fisher Scoring iterations: 1


              Theta:  0.73944
          Std. Err.:  0.00519

 2 x log-likelihood:  -319457.04000

#-------------------------
#Interpretation
#-------------------------
	#https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
	#In terms of regression coefficient
		#The variable zGRS has a coefficient of 0.0117397, which is statistically significant. 
		#This means that for each one-unit increase in zGRS (one sd increase in GRS), the expected log count of the NumberofUnsuccessfulStopAttempts increases by 0.0117397.
#-------------------------
	#Or in terms of IRR
#-------------------------
		exp(0.0117397) #  1.011809
		est <- cbind(Estimate = coef(m1), confint(m1))
		exp(est)
		cbind(exp(est), pvalue = summary(m1)$coefficients[,4])

> cbind(exp(est), pvalue = summary(m1)$coefficients[,4])
                              Estimate     2.5%   97.5%        pvalue
(Intercept)                  3.0752131 2.7589793 3.428062  4.333811e-92
zGRS                         1.0118089 1.0021978 1.021501  1.591723e-02
StoppedReason_Illness_6157   1.3478055 1.3071325 1.389916  1.759524e-80
StoppedReason_Doctor_6157    1.0736278 1.0338100 1.115223  2.570901e-04
StoppedReason_Health_6157    1.3912115 1.3630271 1.419955 9.420253e-221
StoppedReason_Financial_6157 1.1486076 1.1238660 1.173948  1.272275e-35
sex_31                       1.3748569 1.3485516 1.401668 2.454947e-229
age_21022                    0.9898418 0.9885634 0.991120  6.574700e-55
PC1_22009                    0.9975772 0.9913633 1.003831  4.444981e-01
PC2_22009                    1.0043861 0.9979865 1.010826  1.832822e-01
PC3_22009                    1.0035136 0.9972429 1.009824  2.701042e-01
PC4_22009                    1.0012783 0.9966002 1.005976  5.893809e-01
PC5_22009                    1.0057250 1.0037013 1.007754  2.930514e-08
PC6_22009                    1.0045756 0.9987213 1.010465  1.311892e-01
PC7_22009                    0.9978696 0.9926368 1.003128  4.297853e-01
PC8_22009                    1.0004877 0.9952892 1.005715  8.561158e-01
PC9_22009                    0.9987005 0.9965428 1.000856  2.352146e-01
PC10_22009                   0.9976697 0.9930847 1.002276  3.211289e-01

		The percent change in the incident rate of NumberofUnsuccessfulStopAttempts is a 1% increase for every unit increase in zGRS (for every sd in GRS).
#-------------------------
#Adding cpd to model
#-------------------------

summary(Former$CPD_previously_2887) #-10=less than one a day, -1=Do not know
Former$CPDprevious<-ifelse(Former$CPD_previously_2887==-10,0.5,Former$CPD_previously_2887)
Former$CPDprevious<-ifelse(Former$CPDprevious==-1,NA,Former$CPDprevious)


summary(Former$CPDprevious)
summary(m2 <- glm.nb(NumberofUnsuccessfulStopAttempts_2926 ~ zGRS +StoppedReason_Illness_6157 +StoppedReason_Doctor_6157 +StoppedReason_Health_6157 + StoppedReason_Financial_6157+CPDprevious+ sex_31+age_21022+PC1_22009+PC2_22009+PC3_22009+PC4_22009+PC5_22009+PC6_22009+PC7_22009+PC8_22009+PC9_22009+PC10_22009, data = Former))

Deviance Residuals:
    Min       1Q   Median       3Q      Max
-2.3847  -1.3345  -0.3748   0.1209  11.5059

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)
(Intercept)                   0.9678231  0.0558788  17.320  < 2e-16 ***
zGRS                          0.0058590  0.0049350   1.187   0.2351
StoppedReason_Illness_6157    0.2584993  0.0159710  16.186  < 2e-16 ***
StoppedReason_Doctor_6157     0.0391891  0.0198591   1.973   0.0485 *
StoppedReason_Health_6157     0.3502458  0.0105645  33.153  < 2e-16 ***
StoppedReason_Financial_6157  0.0957497  0.0112414   8.518  < 2e-16 ***
CPDprevious                   0.0154542  0.0004742  32.594  < 2e-16 ***
sex_31                        0.2778287  0.0101294  27.428  < 2e-16 ***
age_21022                    -0.0114626  0.0006583 -17.413  < 2e-16 ***
PC1_22009                     0.0010940  0.0032084   0.341   0.7331
PC2_22009                     0.0060520  0.0033271   1.819   0.0689 .
PC3_22009                     0.0044933  0.0032216   1.395   0.1631
PC4_22009                     0.0007676  0.0023942   0.321   0.7485
PC5_22009                     0.0049214  0.0010406   4.729 2.25e-06 ***
PC6_22009                     0.0037596  0.0030581   1.229   0.2189
PC7_22009                    -0.0031515  0.0027279  -1.155   0.2480
PC8_22009                     0.0006511  0.0027202   0.239   0.8108
PC9_22009                    -0.0019467  0.0011084  -1.756   0.0790 .
PC10_22009                   -0.0009528  0.0023831  -0.400   0.6893
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(0.7694) family taken to be 1)

    Null deviance: 78132  on 69874  degrees of freedom
Residual deviance: 74360  on 69856  degrees of freedom
  (11304 observations deleted due to missingness)
AIC: 304055

Number of Fisher Scoring iterations: 1


              Theta:  0.76938
          Std. Err.:  0.00556

 2 x log-likelihood:  -304015.13100

#-------------------------
#Getting IRR
#-------------------------
est <- cbind(Estimate = coef(m2), confint(m2))
cbind(exp(est), pvalue = summary(m2)$coefficients[,4])

                              Estimate     2.5%    97.5%        pvalue
(Intercept)                  2.6322081 2.3585009 2.9379761  3.321145e-67
zGRS                         1.0058762 0.9961891 1.0156463  2.351336e-01
StoppedReason_Illness_6157   1.2949852 1.2552516 1.3361412  6.379221e-59
StoppedReason_Doctor_6157    1.0399671 1.0005625 1.0811632  4.845510e-02
StoppedReason_Health_6157    1.4194164 1.3902503 1.4491709 5.086353e-241
StoppedReason_Financial_6157 1.1004835 1.0765104 1.1250401  1.629571e-17
CPDprevious                  1.0155743 1.0145453 1.0166081 5.067673e-233
sex_31                       1.3202601 1.2943367 1.3466991 1.271864e-165
age_21022                    0.9886029 0.9873194 0.9898862  6.564032e-68
PC1_22009                    1.0010946 0.9947633 1.0074673  7.331172e-01
PC2_22009                    1.0060703 0.9995829 1.0125992  6.890964e-02
PC3_22009                    1.0045034 0.9981468 1.0109005  1.631018e-01
PC4_22009                    1.0007679 0.9960376 1.0055187  7.485101e-01
PC5_22009                    1.0049335 1.0028895 1.0069832  2.251233e-06
PC6_22009                    1.0037667 0.9978574 1.0097112  2.189320e-01
PC7_22009                    0.9968535 0.9915738 1.0021590  2.479820e-01
PC8_22009                    1.0006513 0.9953863 1.0059459  8.108376e-01
PC9_22009                    0.9980552 0.9958766 1.0002319  7.902619e-02
PC10_22009                   0.9990477 0.9943943 1.0037236  6.892988e-01




