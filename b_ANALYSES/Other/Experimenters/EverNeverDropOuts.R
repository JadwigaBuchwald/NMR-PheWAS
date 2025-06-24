####################################################################################
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
	D = read.csv("./zGRS10SNPs_SmokingVariables_343662_confounders_forDA.csv", as.is=T, header=T)

	table(D$EvNev)
		#> table(D$EvNev)
		#    -3      0      1
		# 97424 135890 110348


	D$EverNever <- D$EvNev
	D$EverNever <- ifelse(D$EverNever==-3,NA,D$EverNever)
		table(D$EverNever)
			#     0      1
			#135890 110348

	D$EverNever <- ifelse(D$EverNever==1,"Ever","Never")
		table(D$EverNever)
			#  Ever  Never
			#110348 135890


#Then with Ever-Never-Experimenters

	table(D$EvNevExp)
		#    -3     -1      0      1
		#  1152  96272 135890 110348

	D$EverNeverExperimenters<-D$EvNevExp
	D$EverNeverExperimenters <- ifelse(D$EverNeverExperimenters==-3,NA,D$EverNeverExperimenters)
	D$EverNeverExperimenters <- ifelse(D$EverNeverExperimenters==-1,"Experimenters",D$EverNeverExperimenters)
	D$EverNeverExperimenters <- ifelse(D$EverNeverExperimenters==0,"Never",D$EverNeverExperimenters)
	D$EverNeverExperimenters <- ifelse(D$EverNeverExperimenters==1,"Ever",D$EverNeverExperimenters)

	table(D$EverNeverExperimenters)
		#         Ever Experimenters         Never
		#       110348         96272        135890



	#1.9.2023
	#Checking if na's, experimenters or ever differ at all from never with respect to the GS

	#Never vs Ever
		> table(D$EverNever)

		#  Ever  Never
		#110348 135890


		wilcox.test(GRS ~ EverNever, data=D) 
        		#Wilcoxon rank sum test with continuity correction

			#data:  zGRS by EverNever
			#W = 7484378710, p-value = 0.4511
			#alternative hypothesis: true location shift is not equal to 0


	#Never vs Experimenter
		D$NevervsExperimenter<-D$EverNeverExperimenters
		D$NevervsExperimenter<-ifelse(D$NevervsExperimenter=="Ever",NA,D$NevervsExperimenter)
		table(D$NevervsExperimenter)

			#Experimenters         Never
        		#96272        135890
	
		wilcox.test(GRS ~ NevervsExperimenter, data=D) 
		
        		#Wilcoxon rank sum test with continuity correction

			#data:  GRS by NevervsExperimenter
			#W = 6594133581, p-value = 0.000876
			#alternative hypothesis: true location shift is not equal to 0

	#Never vs NA
		D$NevervsNA<-D$EverNeverExperimenters
		D$NevervsNA<-ifelse(is.na(D$NevervsNA)==TRUE,"Missing",D$NevervsNA)
		D$NevervsNA<-ifelse(D$NevervsNA=="Experimenters",NA,D$NevervsNA)
		D$NevervsNA<-ifelse(D$NevervsNA=="Ever",NA,D$NevervsNA)
		table(D$NevervsNA)

		#Missing   Never
		#   1152  135890
		wilcox.test(GRS ~ NevervsNA, data=D)


        
        	#Wilcoxon rank sum test with continuity correction

		#data:  GRS by NevervsNA
		#W = 80508246, p-value = 0.09448
		#alternative hypothesis: true location shift is not equal to 0

#Most of those that dropped from our Ever-Never variable were Experimenters:

96272/(96272+1152)*100 #=98.82%

#Having a look at the distributions in more detail:

#A) Getting the smoking data
		


	#B) Getting the GRS data
		G<-read.csv("/fs/projects/ukbb/jadwiga/GRS/WhiteBritish/GRS9_topconfig/topconfigalleleA_GRS_topSNPCiealleleB.txt",as.is=T, header=T,sep="\t")
		dim(G)
		#[1] 343662     14
			#Keeping the wanted variables
				WG<-G[,c("userId","GRS")]
				names(WG)<-c("eid","GRS")	

	#C) Merging them together to form our Descptive Table

		DT<-join(WG,D,by="eid",type="left")

	#AGE & GRS

	#Ever
	t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Ever")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])

	#Never
	t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Never")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])

	#Experimenters
	t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Experimenters")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])

	#NA
	t(round(stat.desc(subset(DT,is.na(DT$EverNeverExperimenters)==T)[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])

 	#Ever
	 t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Ever")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])
		#          nbr.val    min    max median   mean std.dev
		# age_21022  110348 39.000 72.000 60.000 57.913   7.749
		# GRS        110348 	-1.733  	2.835  	1.241  	1.196   	0.561

	# Never
	t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Never")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])
		#          nbr.val    min    max median   mean std.dev
		# age_21022  135890 39.000 70.000 57.000 56.262   8.038
		# GRS        135890 	-1.721  	2.746  	1.241  	1.198   	0.559

	# Experimenters
	t(round(stat.desc(subset(DT,DT$EverNeverExperimenters=="Experimenters")[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])
		#          nbr.val    min    max median   mean std.dev
		# age_21022   96272 40.000 72.000 58.000 56.532   8.079
		# GRS         96272	 -1.731	  2.927	  1.249 	 1.207  	 0.555

	# NA
	t(round(stat.desc(subset(DT,is.na(DT$EverNeverExperimenters)==T)[,c("age_21022","GRS")]),3)[c(1,4,5,8,9,13),])
		#          nbr.val    min    max median   mean std.dev
		# age_21022    1152 40.000 70.000 62.000 60.043   7.339
		# GRS          1152	 -0.707	  2.489	  1.273	  1.223	   0.557
