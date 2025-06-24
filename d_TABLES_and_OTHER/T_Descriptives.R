#Creating a descriptives table of the UKBB data

#binary variables:
#	sex
#	ever-never-experimenter
#	current-former
#	wants to quit

#continuous
#	age
#	GRS
#	zGRS
#	former:age started smoking
#	current: age started smoking
#	former: cpd
#	current: cpd
#	former:number of unsuccessfull quit attempts
#	former: age stopped smoking
#	current: wants to quit


#----------------------------------------------------------------------
#Using R-4.1.1 interactively -opening R in bash
#----------------------------------------------------------------------

#cd [path]
#RDir="/apps/statistics2/"
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
	.libPaths("[path]")
	#install.packages("pastecs")
		library(pastecs)

#-----------------------------------
#functions
#-----------------------------------

### Calculating non missing values for a variable. Way to use: nobs(data$variable) 

	nobs= function(variable){ #variable=data$variable
	N=sum(!is.na(variable))
	return(N)
	}

### Function for getting the wanted descriptives for the binary/Categorical variables

	BinaryDescriptives= function(data,list,wanted){ #data, list of variables, list of the names of categories(cases) we wnat the descriptives for

		OUT<-data.frame()
		List<-which(names(data) %in% list)

		for (i in List) {
		
			#Getting all valid answers
			print(names(data)[i])
			Nobs<-nobs(data[,i])
			print(paste("N = ",Nobs,sep=""))

			#Getting the number and percentage of cases
			Table<-table(data[,i])	
			set<- which(names(Table) %in% wanted)

				for (k in set) {

					print(names(Table)[k])
					variable<-paste(names(data)[i],"_",names(Table)[k],sep="")
					n<-Table[k]
					percentage <- round(100*n/Nobs,1)
					cases<-paste(n," (",percentage,")",sep="")
					print(cases)

					latest<-data.frame(variable,Nobs, cases)
					OUT<-rbind(OUT,latest)
	
				}
		}

		names(OUT)<-c("Variable","N","Cases")
		return(OUT)

		}



#-------------------------------------
#The wanted binary and continuous variables for our descriptives table
#-------------------------------------
#binary variables:
#	sex
#	current-former (Status_20116)
#	ever-never-experimenter
#	current: wants to quit

#continuous
#(We also need sex because we want the descriptives by sex)
#	age
#	GRS
#	zGRS
#	former:age started smoking
#	current: age started smoking
#	former: cpd
#	current: cpd
#	former:number of unsuccessfull quit attempts
#	former: age stopped smoking


#-->I'll form one dataset with all of the needed binary and continuous variables and then for obtaining the descriptives I'll just take the needed set of variables


#-------------------------------------------
#Reading data in and getting it into the wanted format
#-------------------------------------------

	#A) Getting the smoking data
		D = read.csv("./zGRS10SNPs_SmokingVariables_343662_confounders_forDA.csv", as.is=T, header=T)
		dim(D)
		#[1] 343662     56
		str(D)
			#keeping wanted variables:
				W<-D[,c("eid","sex_31","Status_20116","EvNevExp","EvNev","WantToStop_3496","age_21022","AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897","AgeStartedSmoking_incurrent_3436","CPD_3456")]



	#B) Getting the GRS data
		G<-read.csv("[path]/GRS/WhiteBritish/GRS9_topconfig/topconfigalleleA_GRS_topSNPCiealleleB.txt",as.is=T, header=T,sep="\t")
		dim(G)
		#[1] 343662     14
			#Keeping the wanted variables
				WG<-G[,c("userId","GRS","zGRS")]
				names(WG)<-c("eid","GRS","zGRS")	

	#C) Merging them together to form our Descptive Table

		DT<-join(WG,W,by="eid",type="left")

	#D) Making sure our variables are in the wanted format

	    #a) current-former:

		#----------------------------------------------------------------
		#2 20116*		LOGISTIC-BINARY	Current/Former

		#1) Among Ever smokers, 
		#2) Recoding needed: "1="Former"| 2="Current"| -3=NA | 0=NA"
			#20116:
			#coding meaning
			#-3 Prefer not to answer
			#0 Never
			#1 Previous
			#2 Current		
		#-----------------------------------------------------------------
		#Recoding
			table(DT$Status_20116,DT$EvNev)
			DT$CurrentFormer<-ifelse(DT$Status_20116>=1 & DT$EvNev==1,DT$Status_20116,NA)
			table(DT$CurrentFormer)

		#Labelling
			DT$CurrentFormer <- factor(DT$CurrentFormer, 
				levels = c(1,2), 
				labels = c("Former", "Current")) 
			table(DT$CurrentFormer)

		#We delete the old version, i.e. column Status_20116
			DT<-DT[,-which(names(DT)=="Status_20116")]

	     #b) Labelling Sex
			#Making the Sex variable for the plots
				DT$Sex <- factor(DT$sex_31, 
				levels = c(0,1), 
				labels = c("Female", "Male")) 



	     #c) Have to make sure negative values are coded as missing where appropriate 

		#i)First creating our Full data and formatting EvNev and EvNevExp
			#Full data
				Full<-DT[,c("eid","age_21022","GRS","zGRS","sex_31","EvNevExp","EvNev","Sex")]
			#Formatting EvNev
				table(Full$EvNev)
				Full$EvNev<-ifelse(Full$EvNev<0,NA,Full$EvNev)
				table(Full$EvNev)
				Full$EvNev <- factor(Full$EvNev, 
					levels = c(0,1), 
					labels = c("Never", "Ever")) 
				table(Full$EvNev)

			#Formatting EvNevExp
				table(Full$EvNevExp)
				Full$EvNevExp<-ifelse(Full$EvNevExp==-3,NA,Full$EvNevExp)
				table(Full$EvNevExp)
				Full$EvNevExp <- factor(Full$EvNevExp, 
					levels = c(-1,0,1), 
					labels = c("Experimenter","Never", "Ever")) 
				table(Full$EvNevExp)

		#ii) Then creating our Ever data and formatting the smoking variables needed for the descriptives table
		#    Need to get rid of -3 Prefer not to answer and -10=Less than one a day
		#    Need to format the variable wants to stop smoking

			#Ever data 
				Ever<-subset(DT,DT$EvNev==1)
				#> dim(Ever)
				#[1] 110348     15

			#Checking which variables have negative values to be dealt with	
				summary(Ever)
				#Four variables:
				# WantToStop_3496,   AgeStartedSmoking_informer_2867,CPD_previously_2887,NumberofUnsuccessfulStopAttempts_2926, AgeStoppedSmoking_2897,AgeStartedSmoking_incurrent_3436,CPD_3456

			#We assign missing values to all negative values:
	
				for (i in c("WantToStop_3496", "AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926", "AgeStoppedSmoking_2897","AgeStartedSmoking_incurrent_3436","CPD_3456")) { 
					print(i)
					print("before")
					print(summary(Ever[,which(names(Ever)==i)]))
					Ever[,which(names(Ever)==i)]<-ifelse(Ever[,which(names(Ever)==i)]<0,NA,Ever[,which(names(Ever)==i)]) 
					print("after")
					print(summary(Ever[,which(names(Ever)==i)]))
				}

			#Then getting the want to stop smoking variable in order
			
			#-----------------------------------------------------------------
			#Data-Field 3496
			#Description:	Wants to stop smoking
			#1	Yes, definitely
			#2	Yes, probably
			#3	No, probably not
			#4	No, definitely not
			#-3	Prefer not to answer
			#-----------------------------------------------------------------

				table(Ever$WantToStop_3496)
				Ever$WantToStop<-ifelse(Ever$WantToStop_3496<=2,"Yes","No")
				table(Ever$WantToStop)


#----------------------------------------------------------------
#Getting the wanted descriptives for our full data  (Age,GRS,zGRS,EvNevExp & everything by sex aswell)
#----------------------------------------------------------------

#Continuous variables(Age,GRS,zGRS):

	#All
		round(stat.desc(Full[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),]
 		t(round(stat.desc(Full[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),])
		#          nbr.val    min    max median  mean std.dev
		# age_21022  343662 39.000 72.000 58.000 56.88   7.991
		# GRS        343662 -1.733  2.927  1.242  1.20   0.559
		# zGRS       343662 -5.249  3.091  0.076  0.00   1.000

		#Saving this into a data.frame Data Continuous All (three decimals)
		DCA3<-as.data.frame(t(round(stat.desc(Full[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),]))

		#Saving this into a data.frame Data Continuous All (one decimal)
		DCA1<-as.data.frame(t(round(stat.desc(Full[,c("age_21022","GRS","zGRS")]),1)[c(1,4,5,8,9,13),]))


	#By Sex
		#Female
			t(round(stat.desc(subset(Full,Full$Sex=="Female")[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),])
			#          nbr.val    min    max median   mean std.dev
			# age_21022  184565 40.000 71.000 58.000 56.667   7.899
			# GRS        184565 -1.733  2.927  1.245  1.202   0.557
			# zGRS       184565 -5.249  3.091  0.081  0.004   0.997

			# Three decimal places:
			DCF3<-t(round(stat.desc(subset(Full,Full$Sex=="Female")[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),])
			colnames(DCF3)<-paste("F",colnames(DCF3),sep="")
			
			# One decimal place:
			t(round(stat.desc(subset(Full,Full$Sex=="Female")[,c("age_21022","GRS","zGRS")]),1)[c(1,4,5,8,9,13),])
			DCF1<-t(round(stat.desc(subset(Full,Full$Sex=="Female")[,c("age_21022","GRS","zGRS")]),1)[c(1,4,5,8,9,13),])
			colnames(DCF1)<-paste("F",colnames(DCF1),sep="")


		#Male
			t(round(stat.desc(subset(Full,Full$Sex=="Male")[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),])
			#          nbr.val    min    max median   mean std.dev
			# age_21022  159097 39.000 72.000 59.000 57.128   8.089
			# GRS        159097 -1.731  2.835  1.241  1.198   0.561
			# zGRS       159097 -5.245  2.927  0.074 -0.004   1.003

			DCM3<-t(round(stat.desc(subset(Full,Full$Sex=="Male")[,c("age_21022","GRS","zGRS")]),3)[c(1,4,5,8,9,13),])
			colnames(DCM3)<-paste("M",colnames(DCM3),sep="")

			DCM1<-t(round(stat.desc(subset(Full,Full$Sex=="Male")[,c("age_21022","GRS","zGRS")]),1)[c(1,4,5,8,9,13),])
			colnames(DCM1)<-paste("M",colnames(DCM1),sep="")

	#Merging all together and saving: Descriptives Continuous
		DC3<-cbind(DCA3,DCF3,DCM3)
		DC1<-cbind(DCA1,DCF1,DCM1)
		
		write.table(DC1,"./DescriptivesContinuous_1dec.csv",sep=",",quote=F,row.names=T, col.names=T)
		write.table(DC1,"./DescriptivesContinuous_1dec.txt",sep="\t",quote=F,row.names=T) 

#Binary/Categorical variables (sex,EvNevExp):
	list<-which(names(Full) %in% c("Sex","EvNevExp"))

	for (i in list) {
		print(names(Full)[i])
		N<-nobs(Full[,i])
		print(paste("N = ",N,sep=""))
		print(table(Full[,i]))
		print(table(Full[,i])/N)
	}

	#	[1] "EvNevExp"
	#	[1] "N = 342510"

	#	Experimenter        Never         Ever
	#       96272       135890       110348

	#	Experimenter        Never         Ever
	#	   0.2810779    0.3967475    0.3221745
	#	[1] "Sex"
	#	[1] "N = 343662"

	#	Female   Male
	#	184565 159097

	#	Female      Male
	#	0.5370538 0.4629462

    #And then by sex:

	addmargins( table(Full$Sex,Full$EvNevExp),c(1,2)) 

	#         Experimenter  Never   Ever    Sum
	#  Female        52700  81037  50217 183954
	#  Male          43572  54853  60131 158556
	#  Sum           96272 135890 110348 342510


	prop.table(table(Full$Sex,Full$EvNevExp), margin=1)
	#    Experimenter     Never      Ever
	#  Female    0.2864847 0.4405286 0.2729867
	#  Male    0.2748051 0.3459535 0.3792414


	#Saving the descriptives data

#Using the function to get our descriptives
	#Wanted variables
		list<-c("EvNevExp")
	#Wanted categories
		wanted<-c("Never","Experimenter","Ever")

	#Getting the descriptives for our variables and categories 

		#Saving and merging (All Binary Data)

		#ALL
			AB<-BinaryDescriptives(Full,"EvNevExp",c("Never","Experimenter","Ever"))

		#Female
			FB<-BinaryDescriptives(subset(Full,Full$Sex=="Female"),list,wanted)
			colnames(FB)<-paste("F",colnames(FB),sep="")
		#Male
			MB<-BinaryDescriptives(subset(Full,Full$Sex=="Male"),list,wanted)
			colnames(MB)<-paste("M",colnames(MB),sep="")
	
		#Merging, but leaving out the variable column from the two latter
		B<-cbind(AB,FB[,-which(colnames(FB)=="FVariable")],MB[,-which(colnames(MB)=="MVariable")])

		write.table(B,"./DescriptivesAll_Binary.txt",sep="\t",quote=F,row.names=F) 

#----------------------------------------------------------------
#Getting the wanted descriptives for Ever data  
#----------------------------------------------------------------

#Binary (Former-Current & Wants to quit) 


#Using the function to get our descriptives
	#Wanted variables
		list<-c("CurrentFormer","WantToStop")
	#Wanted categories
		wanted<-c("Former","Current","Yes")

	#Getting the descriptives for our variables and categories 

		#Saving and merging (Ever Binary Data)

		#ALL
			EBA<-BinaryDescriptives(Ever,list,wanted)
		#Female
			EBF<-BinaryDescriptives(subset(Ever,Ever$Sex=="Female"),list,wanted)
			colnames(EBF)<-paste("F",colnames(EBF),sep="")
		#Male
			EBM<-BinaryDescriptives(subset(Ever,Ever$Sex=="Male"),list,wanted)
			colnames(EBM)<-paste("M",colnames(EBM),sep="")
	
		#Merging, but leaving out the variable column from the two latter
		EB<-cbind(EBA,EBF[,-which(colnames(EBF)=="FVariable")],EBM[,-which(colnames(EBM)=="MVariable")])

		write.table(EB,"./DescriptivesEver_Binary.txt",sep="\t",quote=F,row.names=F) 

#----------------------------------------------------------------
#Descriptives for Former (Age started,cpd, number of unsuccessful quit attempts, age stopped)
#----------------------------------------------------------------
	#Getting our Former subset
		Former<-subset(Ever,Ever$CurrentFormer=="Former")

	#Printing out the descriptives for the wanted phenotypes
		t(round(stat.desc(Former[,c("AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),3)[c(1,4,5,8,9,13),])

	                                      			#nbr.val min max median   mean std.dev
			# AgeStartedSmoking_informer_2867         80788   5  63     17 17.198   3.561
			# CPD_previously_2887                     76865   1 140     20 19.300  10.486
			# NumberofUnsuccessfulStopAttempts_2926   74108   0 200      2  2.851   7.098
			# AgeStoppedSmoking_2897                  80859   9  69     39 39.511  11.561

	#Saving this into a data.frame Descriptives Former All (one decimal)
		DFA<-as.data.frame(t(round(stat.desc(Former[,c("AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),]))

	#By Sex
		#Female
						
			#One decimal place:
				DFF<-t(round(stat.desc(subset(Former,Former$Sex=="Female")[,c("AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),])
				colnames(DFF)<-paste("F",colnames(DFF),sep="")


		#Male
			#One decimal place:
				DFM<-t(round(stat.desc(subset(Former,Former$Sex=="Male")[,c("AgeStartedSmoking_informer_2867","CPD_previously_2887","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),])
				colnames(DFM)<-paste("M",colnames(DFM),sep="")

	#Merging all together and saving: Descriptives Former
		DF<-cbind(DFA,DFF,DFM)
		
		write.table(DF,"./DescriptivesFormer_1dec.txt",sep="\t",quote=F,row.names=T) 
		#(By hand added "Variable" to the first column)

#----------------------------------------------------------------
#Descriptives for Current (Age started,cpd, unsuccessfull stop attempts, age stopped smoking on most days)
#----------------------------------------------------------------
#Getting our Current subset
		Current<-subset(Ever,Ever$CurrentFormer=="Current")

	#Printing out the descriptives for the wanted phenotypes
		t(round(stat.desc(Current[,c("AgeStartedSmoking_incurrent_3436","CPD_3456", "NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),3)[c(1,4,5,8,9,13),])

	                                      			#nbr.val min max median   mean std.dev
				# AgeStartedSmoking_incurrent_3436   25522   5  69     16 17.735   5.764
				# CPD_3456                           23682   1 140     15 15.749   8.427
				# NumberofUnsuccessfulStopAttempts_2926    2527   0 200      3  4.057  10.100
				# AgeStoppedSmoking_2897                   3218  12  70     48 47.253  11.595

	#Saving this into a data.frame Descriptives Current All (one decimal)
		DCA<-as.data.frame(t(round(stat.desc(Current[,c("AgeStartedSmoking_incurrent_3436","CPD_3456","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),]))

	#By Sex
		#Female
						
			#One decimal place:
				DCF<-t(round(stat.desc(subset(Current,Current$Sex=="Female")[,c("AgeStartedSmoking_incurrent_3436","CPD_3456","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),])
				colnames(DCF)<-paste("F",colnames(DCF),sep="")


		#Male
			#One decimal place:
				DCM<-t(round(stat.desc(subset(Current,Current$Sex=="Male")[,c("AgeStartedSmoking_incurrent_3436","CPD_3456","NumberofUnsuccessfulStopAttempts_2926","AgeStoppedSmoking_2897")]),1)[c(1,4,5,8,9,13),])
				colnames(DCM)<-paste("M",colnames(DCM),sep="")

	#Merging all together and saving: Descriptives Former
		DC<-cbind(DCA,DCF,DCM)
		
		write.table(DC,"./DescriptivesCurrent_1dec_withUnsuccessfulStopAttempts.txt",sep="\t",quote=F,row.names=T) 
		#(By hand added "Variable" to the first column)



#--------------------------------------------------------------------
#Formatting the descriptives for the continuous variables slightly differently
#--------------------------------------------------------------------

#Reading in data
	All<-read.csv("./DescriptivesContinuous_1dec.txt",as.is=T, header=T,sep="\t")
	Current<-read.csv("./DescriptivesCurrent_1dec.txt",as.is=T, header=T,sep="\t")
	Former<-read.csv("./DescriptivesFormer_1dec.txt",as.is=T, header=T,sep="\t")


#Creating Formatted version

	#ALL
	
	AllFormatted <- data.frame(
		phenotype<-rownames(All)
	)
 	
	AllFormatted$mean_sd<-paste(All$mean, " (",All$std.dev,")",sep="")
	AllFormatted$med_range<-paste(All$median, " [",All$min,"-",All$max,"]",sep="")

	AllFormatted$Fmean_sd<-paste(All$Fmean, " (",All$Fstd.dev,")",sep="")
	AllFormatted$Fmed_range<-paste(All$Fmedian, " [",All$Fmin,"-",All$Fmax,"]",sep="")

	AllFormatted$Mmean_sd<-paste(All$Mmean, " (",All$Mstd.dev,")",sep="")
	AllFormatted$Mmed_range<-paste(All$Mmedian, " [",All$Mmin,"-",All$Mmax,"]",sep="")

	write.table(AllFormatted,"./DescriptivesAllFormatted_1dec.txt",sep="\t",quote=F,row.names=T) 



	#CURRENT
	
	CurrentFormatted <- data.frame(
		phenotype<-Current$Variable
	)

	CurrentFormatted$mean_sd<-paste(Current$mean, " (",Current$std.dev,")",sep="")
	CurrentFormatted$med_range<-paste(Current$median, " [",Current$min,"-",Current$max,"]",sep="")

	CurrentFormatted$Fmean_sd<-paste(Current$Fmean, " (",Current$Fstd.dev,")",sep="")
	CurrentFormatted$Fmed_range<-paste(Current$Fmedian, " [",Current$Fmin,"-",Current$Fmax,"]",sep="")

	CurrentFormatted$Mmean_sd<-paste(Current$Mmean, " (",Current$Mstd.dev,")",sep="")
	CurrentFormatted$Mmed_range<-paste(Current$Mmedian, " [",Current$Mmin,"-",Current$Mmax,"]",sep="")

	write.table(CurrentFormatted,"./DescriptivesCurrentFormatted_1dec.txt",sep="\t",quote=F,row.names=F) 



	#FORMER
	
	FormerFormatted <- data.frame(
		phenotype<-Former$Variable
	)

	FormerFormatted$mean_sd<-paste(Former$mean, " (",Former$std.dev,")",sep="")
	FormerFormatted$med_range<-paste(Former$median, " [",Former$min,"-",Former$max,"]",sep="")

	FormerFormatted$Fmean_sd<-paste(Former$Fmean, " (",Former$Fstd.dev,")",sep="")
	FormerFormatted$Fmed_range<-paste(Former$Fmedian, " [",Former$Fmin,"-",Former$Fmax,"]",sep="")

	FormerFormatted$Mmean_sd<-paste(Former$Mmean, " (",Former$Mstd.dev,")",sep="")
	FormerFormatted$Mmed_range<-paste(Former$Mmedian, " [",Former$Mmin,"-",Former$Mmax,"]",sep="")

	write.table(FormerFormatted,"./DescriptivesFormerFormatted_1dec.txt",sep="\t",quote=F,row.names=F) 




#####################################################################################################################
#Not included in the descriptives table:
#Tea and coffee (from: [path]/PHESANT/PHEWAS_2Stage/FDR_final/Scripts/D1_Binary.R)

	
	table(N$x1488der_TeaDrinker)			
		#     0      1
		# 50676 292337
	
	table(N$d1498_CoffeeDrinker_excldecaf)
	#     0      1
	# 71266 218326
#> 292337/343662
#[1] 0.8506527
#> 218326/343662
#[1] 0.6352928

