#2023 March 16th
#Jadwiga Buchwald

#------------------------------------------------------------------------------
#I) Plotting the continuous variables that had been analysed using linear regression. 
#II) Also plotting the ones that I might wnat to modify/ analyse using a different model.
#------------------------------------------------------------------------------


#----------------------------------------------------------------------
#ANALYSES
#----------------------------------------------------------------------

#Doing this interactively

#cd /[path]
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R


#-----------------------------------
#Rscript
#-----------------------------------


#---------------------------------
#Not for Github:

DataDir="/[path]/"

#----------------------------------

sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(ggplot2)
	library(tidyr)

#----------------------------------------------------------------------------------------
#READING IN THE DATA 


#a) Reading in the phenotype data

	P = read.csv("./IntermediateData/ukb_sorted_n343662_61variables.csv")
	dim(P)

#> dim(P)
#[1] 343662    415


#b) Reading the confounder data

	#we read in the confounder file
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=T, header=T)
		names(C)
		str(C)
		dim(C)
#> names(C)
# [1] "eid"         "x31_0_0"     "x21022_0_0"  "x22009_0_1"  "x22009_0_2"
# [6] "x22009_0_3"  "x22009_0_4"  "x22009_0_5"  "x22009_0_6"  "x22009_0_7"
#[11] "x22009_0_8"  "x22009_0_9"  "x22009_0_10"

#> dim(C)
#[1] 343662     13



#--------------------------------------
# PLOTS
#--------------------------------------

########################################################################
#I) Plotting the variables that we already had in the linear regression
########################################################################

#The 33 variables that we already had in the linear regression (and want to keep)
#varName                                                       description
#30610                                              Alkaline phosphatase
#30620                                          Alanine aminotransferase
#1807                                             Father's age at death
#30700                                                        Creatinine
#20154 Forced expiratory volume in 1-second (FEV1), predicted percentage
#30530                                                   Sodium in urine
#23462                                                           Glycine
#23451               Omega-3 Fatty Acids to Total Fatty Acids percentage
#30210                                            Eosinophill percentage
#23459                  Omega-6 Fatty Acids to Omega-3 Fatty Acids ratio
#2897                                               Age stopped smoking
#20256               Forced expiratory volume in 1-second (FEV1) Z-score
#20150         Forced expiratory volume in 1-second (FEV1), Best measure
#30750                                      Glycated haemoglobin (HbA1c)
#1488                                                        Tea intake
#20258                                           FEV1/ FVC ratio Z-score
#25874             Volume of grey matter in Supracalcarine Cortex (left)
#30880                                                             Urate
#30710                                                C-reactive protein
#30630                                                  Apolipoprotein A
#23443                                            Degree of Unsaturation
#3063                       Forced expiratory volume in 1-second (FEV1)
#23449                                                     Linoleic Acid
#23109                                          Impedance of arm (right)
#26678                                    Volume of VA (left hemisphere)
#30680                                                           Calcium
#48                                               Waist circumference
#30510                                   Creatinine (enzymatic) in urine
#23007                      pp150 Nter antigen for Human Cytomegalovirus
#20151                         Forced vital capacity (FVC), Best measure
#20257                               Forced vital capacity (FVC) Z-score
#53062                                       Forced vital capacity (FVC)
#26536			Volume-ratio of BrainSegVol-to-eTIV (whole brain)

L<- P[,c("eid",
	"x30610_0_0","x30620_0_0","x1807_0_0","x30700_0_0","x20154_0_0","x30530_0_0","x23462_0_0","x23451_0_0","x30210_0_0","x23459_0_0","x2897_0_0","x20256_0_0","x20150_0_0","x30750_0_0","x1488_0_0","x20258_0_0",
	"x25874_2_0","x30880_0_0","x30710_0_0","x30630_0_0","x23443_0_0","x3063_0_0","x23449_0_0","x23109_0_0","x26678_2_0","x30680_0_0","x48_0_0","x30510_0_0","x23007_0_0","x20151_0_0","x20257_0_0","x3062_0_0", "x26536_2_0")]
dim(L) #34
head(L[,1:10])

#------------------------------------
##Then going through all variables: negative values to missing

#Let's have a look at the minimum values each variable gets and that the summary stats look ok:
	str(L)
	summary(L[,2:34])

	#The following variables had negative values:  x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking ,  
	x20256_0_0 FEV1 z-version, x1488_0_0.1 Tea intake, x20258_0_0 FEV1 z-version, x20257_0_0 FEV1 z-version   


	#Quickly checking the coding for the above variables containing negative values (doing this in ukbiobank showcase on their webpage: https://www.ukbiobank.ac.uk/)

	#-->We make negative value smissing for: x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking
	#-->BUT we keep them as they are for: x20256_0_0 FEV1 z-version,  x20258_0_0 FEV1 z-version, x20257_0_0 FEV1 z-version  


	#Then for x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking
	#We assign missing values to all negative values:
	
for (i in c("x1807_0_0", "x2897_0_0")) { 
	print(i)
	print("before")
	print(summary(L[,which(names(L)==i)]))
	L[,which(names(L)==i)]<-ifelse(L[,which(names(L)==i)]<0,NA,L[,which(names(L)==i)]) 
	print("after")
	print(summary(L[,which(names(L)==i)]))
}


#------------------------------------

#Merging data 

	#Merging the two files
		D<-join(C,L,by="eid",type="left")

#Ensuring Sex is in the format needed for plotting
	D$Sex <- factor(D$x31_0_0, 
	levels = c(0,1), 
	labels = c("Female", "Male")) 

#Saving this data
	write.table(D,"./IntermediateData/Linear33_forhistos.csv",sep=",",quote=T,row.names=F, col.names=T)
#note: I removed this once plots were ready so it wouldn't take up space unnecessarily.

#------------------------------------------------------

#Reading in the data
	D = read.csv("./IntermediateData/Linear33_forhistos.csv")
	dim(D)

#> dim(D)
#[1] 343662     47

#Note: 1 eid 2-13 confounders 47 Sex for plotting & 14-46 33 linear regression outcomes



#Doing one plot first: 
	#Bimodal one: 23109 CONTINUOUS Impedance of arm (right) Anthropometry

	#Density plot of zGRS
	p<-ggplot(D, aes(x=x23109_0_0, fill=Sex, colour=Sex)) +
	  geom_histogram(position="identity", alpha=0.5) +
	 labs(x = "23109 Impedance of arm (right)")
	pdf("./Histograms/23109_ImpedanceofArm_bySex_histo.pdf")
	print(p)
	dev.off()

	#--> Just as I thought. Distributed by sex. (Had already looked at the distributions on showcase)


#Then all of them:
	phenotypes <- names(D[,14:46]) 
for (i in phenotypes) { 
	print(i)
	p<-ggplot(D, aes(x=D[,which(names(D)==i)], fill=Sex, colour=Sex)) +
	  geom_histogram(position="identity", alpha=0.5) +
	 #labs(x = names(i))
	pdf(paste("./Histograms/",i,"_bySex_histo.pdf",sep=""))
	print(p)
	dev.off()
}

#-->Distributed by sex: 
#30880 Urate -men have greater values
#30530 -men have greater values
#30700 Creatinine (same as above, not so clear though)
#30630 Apolipoprotein A -Females have greater values
#30510 Creatinine (enzymatic) in urine	Urine assays -me have greater values
#26678 Volume of VA (left hemisphere) Men have greater values
#25874 Volume of grey matter in Supracalcarine Cortex (left)	Same as above
#23451 Omega-3 Fatty Acids to Total Fatty Acids percentage -Women have greater values
#23449 Linoleic Acid	Blood assays -Women have greater values
#23443 Degree of Unsaturation -Women have greater values
#23109 Impedance of arm (right) -Women have greater values
#20151 Forced vital capacity (FVC), Best measure -Men have greater values
#20150 Forced expiratory volume in 1-second (FEV1) -Same as above
#3063 Forced expiratory volume in 1-second (FEV1) -Same as above
#3062 Forced vital capacity (FVC) -Same as above
#48 Waist circumference -same as above

#-->Outliers:
#30750 Glycated haemoglobin (HbA1c) (few larger values)
#30710 C-reactive protein (same as above)
#30700 Creatinine (same as above)
#30620 Alanine aminotransferase (same as above)
#30610 Alkaline phosphatase (same as above)
#30210 Eosinophill percentage	Blood assays (same as above)
#3063 Forced expiratory volume in 1-second (FEV1) (Same as above)
#1488 Tea intake (same as above)
#-->Based on showcase looks like they are just outliers.

#----------------------

########################################################################
#II) Also plotting the ones that I might wnat to modify/ analyse using a different model.
########################################################################

#The ones we might want to make continuous:
#30150   CONTINUOUS  Eosinophill count Blood assays x30150_0_0
#2887    INTEGER Number of cigarettes previously smoked daily    Lifestyle and environment x2887_0_0
#3456    INTEGER Number of cigarettes currently smoked daily (current cigarette smokers)    Lifestyle and environment x3456_0_0
#1498    INTEGER Coffee intake    Lifestyle and environment x1498_0_0

#Coding for CPD
#-10	Less than one a day
#-1	Do not know

#Plotting coffee and tea: I'd like to break them into binary variables: Yes-No and Quantity variables > 0 cups
#1488    INTEGER   Tea intake Lifestyle and environment x1488_0_0
#1498    INTEGER   Coffee intake    Lifestyle and environment x1498_0_0

#Coding for both:
#-10	Less than one
#-1	Do not know
#-3	Prefer not to answer



#Note, there are lots of tea questions in the ukbb data but no other tea question had a statistically significant association with the GRS! 
#So perhaps, GRS only associated with caffeine 

#We read these variables in:
	M<- P[,c("eid",
		"x30150_0_0","x2887_0_0","x3456_0_0","x1498_0_0","x1488_0_0")]
	dim(M) #6

#Merging data 

	#Merging the two files
		D<-join(C,M,by="eid",type="left")

#Ensuring Sex is in the format needed for plotting
	D$Sex <- factor(D$x31_0_0, 
	levels = c(0,1), 
	labels = c("Female", "Male")) 

##Then going through all variables: negative values to missing
#Let's have a look at the minimum values each variable gets and that the summary stats look ok:
	str(M)
	summary(M[,2:6])

#-->We make negative value smissing for: x2887_0_0 CPD previous,  x3456_0_0 CPD current
#We assign missing values to all negative values:
	
for (i in c("x2887_0_0","x3456_0_0")) { 
	print(i)
	print("before")
	print(summary(M[,which(names(M)==i)]))
	M[,which(names(M)==i)]<-ifelse(M[,which(names(M)==i)]<0,NA,M[,which(names(M)==i)]) 
	print("after")
	print(summary(M[,which(names(M)==i)]))
}

#We make -10 -->0.5 cups 
	D$coffee=ifelse(D$x1498_0_0==-10,0.5,D$x1498_0_0)
	D$tea=ifelse(D$x1488_0_0==-10,0.5,D$x1488_0_0)

#We make other minus values missing
	D$coffee=ifelse(D$coffee<0,NA,D$coffee)
	D$tea=ifelse(D$tea<0,NA,D$tea)

#We make binary versions
	D$BinaryCoffee_x1498_0_0=ifelse(D$coffee>0,1,D$coffee)
	D$BinaryTea_x1488_0_0=ifelse(D$tea>0,1,D$tea)

#We make the quantity versions (drop the zero values)
	D$QuantityCoffee_x1498_0_0=ifelse(D$coffee>0,D$coffee,NA)
	D$QuantityTea_x1488_0_0=ifelse(D$tea>0,D$tea,NA)



#Plotting
#Then all of them:
	phenotypes <- c("x30150_0_0","x2887_0_0","x3456_0_0","QuantityCoffee_x1498_0_0","QuantityTea_x1488_0_0")

for (i in phenotypes) { 
	print(i)
	p<-ggplot(D, aes(x=D[,which(names(D)==i)], fill=Sex, colour=Sex)) +
	  geom_histogram(position="identity", alpha=0.5) +
	 #labs(x = names(i))
	pdf(paste("./Histograms/",i,"_bySex_histo.pdf",sep=""))
	print(p)
	dev.off()
}

#-->Sex differences
#2887    INTEGER Number of cigarettes previously smoked daily -Men have greater values
#3456    INTEGER Number of cigarettes currently smoked daily (current cigarette smokers) -Men have greater values

#-->Outliers:
#30150   CONTINUOUS  Eosinophill count Blood assays x30150_0_0
#1498    INTEGER Coffee intake 
#1488    INTEGER   Tea intake

#table of the binary variables
table(D$BinaryCoffee_x1498_0_0,D$BinaryTea_x1488_0_0)

         0      1
  0   7432  63671
  1  43189 228396

43189+228396 #271585 (coffee)

63671+228396 #292067 (tea)

#-->Most people drink both tea and coffee. More plain tea drinkers than plain coffee drinkers. Smallest group is the no coffee nor tea group.
#(More people drink tea than coffee - makes sense as people from the UK)




