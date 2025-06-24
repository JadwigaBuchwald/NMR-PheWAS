#Supplementary table
#Jadwiga Buchwald

#--------------------------------------
# b: MODELS
#--------------------------------------
#Reading in our data
	D = read.csv("./zGRS10SNPs_sexage_SmokingVariables_343662.csv", as.is=T, header=T)
	str(D)


#Reading in our confounders so that we get the 10 PCs for our model
	C = read.csv("[path]/Phenotypes/DataWrangling/Extracting_ALL/ukb_n343662_phesant_confounders.csv")
	names(C)
	str(C)

	#Only keeping the 10 PCs and eid as I already have sex and age
		C<-C[,c(1, 4:13)]

#Merging D and C
	D <- join(D,C, by = "eid", type ="left")
	str(D)

#Making the Sex variable for the plots
	D$Sex <- factor(D$sex_31, 
		levels = c(0,1), 
		labels = c("Female", "Male")) 

#Recoding the negative CPD values as missing (note there are no 0 values)
	D$CPD<-ifelse(D$CPD_3456>0, D$CPD_3456,NA)  

#Keeping only the data without missing values for CPD
		A<-subset(D,!(is.na(D$CPD)))
		dim(A) #23682    32




#Dividing the data in to slower and faster halves:

GRSlow<-subset(A, A$zGRS<0); dim(GRSlow) #10871    33
GRShigh<-subset(A, A$zGRS>=0); dim(GRShigh) # 12811    33


#Comparing the regression coefficient

	#adjusting and normalizing CPD first
		fit2 <- lm(CPD ~ Sex+ age_21022 + x22009_0_1+ x22009_0_2 + x22009_0_3 + x22009_0_4 + x22009_0_5 + x22009_0_6 + x22009_0_7 + x22009_0_8 + x22009_0_9+ x22009_0_10, data=A)
	
	#Taking the adjusted CPD
		resCPD<-residuals(fit2)

		summary(resCPD)
		sd(resCPD)
		#> summary(resCPD)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
		#-17.534  -5.873  -0.352   0.000   4.516 122.174
		#> sd(resCPD)
		#[1] 8.245184


	#Inverse rank transformation
		#Getting function ready:
		#https://yuxuanstat.com/posts/2020/06/rank-based-inverse-normal-transformation/
		#find INT-transformed residuals
			irnt <- function(pheno) {
				set.seed(1234)
				numPhenos = length(which(!is.na(pheno)))
				quantilePheno = (rank(pheno, na.last="keep", ties.method="random")-0.5)/numPhenos
				phenoIRNT = qnorm(quantilePheno)	
				return(phenoIRNT);
			}

	#normalizing using the above function
		rtn_resCPD = irnt(resCPD)

	#PHESANT style modelling
		fit1<-lm(rtn_resCPD ~ zGRS, data=A)
		summary(fit1)

	#Then for low and high subsets seperately
		A$rtn_resCPD = irnt(resCPD)
		GRSlow<-subset(A, A$zGRS<0); dim(GRSlow) #10871    33
		GRShigh<-subset(A, A$zGRS>=0); dim(GRShigh) # 12811    33


	#LOW
		fitLOW<-lm(rtn_resCPD ~ zGRS, data=GRSlow)
		summary(fitLOW)


	#HIGH
		fitHIGH<-lm(rtn_resCPD ~ zGRS, data=GRShigh)
		summary(fitHIGH)






