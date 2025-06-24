#19.1.22
#Creating the GRS


#Doing this interactively

#cd /[path]
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R

#-----------------------------------------------------------------------
#Rscript
#-----------------------------------------------------------------------

sessionInfo()
getwd()

#libraries 
	library(dplyr)
	library(ggplot2)



#Note, Dosages have now been calculated for alleleA which is the NEA so the betaJ vector will have to be multiplied by -1 so that we get the effects for alleleA


Dosages="GRStopconfig_10SNPs_WB20210809_dosage.txt" #From 3_JoinDosage.sh
WeightsCHR19="/[path]topconfig_JointBetas.txt" #From FINEMAP results: 6_topconfig_JointBetas.sh
WeightsCHR4<-"/[path]chr4SNP_details.txt" #Results from previous META GWAS. Table S5 in: Buchwald, J., Chenoweth, M.J., Palviainen, T. et al. Genome-wide association meta-analysis of nicotine metabolism and cigarette consumption measures in smokers of European descent. Mol Psychiatry 26, 2212–2223 (2021). https://doi.org/10.1038/s41380-020-0702-z
Stats="GRS10SNPs.stats" #From 5_Combining_GRS10SNPs_stats.sh

#Reading in the datasets
	#Dosages (alleleA)
		D<-read.table(paste(Dosages,sep=""), as.is=T, header=T)
		print("dim(D)")
		print(dim(D))
	#Summary statistics
		S<-read.table(paste(Stats,sep=""), as.is=T, header=T)
	#Weights
		#joint betas from finemap: Note betaJ for alleleB=EA
		W19<-read.table(paste(WeightsCHR19,sep=""), header=T,sep=",")
		
		#Checking weight for chr4:
			W4<-read.table(paste(WeightsCHR4,sep=""), as.is=T, header=T,sep="\t")
			W4[,c("CHR","RSID","Major_Minor","b_minor")]
			#  CHR       RSID Major_Minor b_minor
			#1   4 rs36103218         T/C   0.175
			S[1,c("rsid","chromosome","alleleA","alleleB")]
			#        rsid chromosome alleleA alleleB
			#1 rs36103218          4       C       T

			#-->so weight for alleleB=EA is -0.175 (this way is comparable with the chr19 betaJs)

#Joining all the SNPs and weights into one data frame
	W4$weight_alleleB<--0.175
	Weight4<-W4[,c("RSID","weight_alleleB")]

	W19$weight_alleleB<-W19$betaJ
	Weight19<-W19[,c("SNP","weight_alleleB")]
	names(Weight19)<-c("RSID","weight_alleleB")

	W<-rbind(Weight4,Weight19)


#We check the SNPs are in the same order in the weights and dosage datasets (W & D)
	print("dim(W)")
	print(dim(W))

	print("dim(D)")
	print(dim(D))

	names(D[2:11])
	W$RSID
	names(D[2:11])==W$RSID #TRUE for all

#Checking both weight and dosages are in numeric format
	str(W$weight_alleleB)
	str(D[,2:11])

#We flip the weights so they are for alleleA which is the allele the dosages have been counted for
	W$weight_alleleA <- -W$weight_alleleB

#Then we create the GRS
	D$GRS<-as.matrix(D[,2:11]) %*% W$weight_alleleA

#	> summary(D$GRS)
#	       V1
#	 Min.   :-1.7325
#	 1st Qu.: 0.8967
#	 Median : 1.2424
#	 Mean   : 1.2000
#	 3rd Qu.: 1.6244
#	 Max.   : 2.9271

	mean(D$GRS)
#	> mean(D$GRS)
#	[1] 1.19998

	sd(D$GRS)
#	> sd(D$GRS)
#	[1] 0.5586965

#Creating the standardized GRS
	D$zGRS <- (D$GRS - mean(D$GRS)) / sd(D$GRS)
	summary(D$zGRS)
#	> summary(D$zGRS)
#       	V1
# 	Min.   :-5.24887
#	 1st Qu.:-0.54280
#	 Median : 0.07597
#	 Mean   : 0.00000
#	 3rd Qu.: 0.75969
#	 Max.   : 3.09140



#We save the GRS data
	final<-D[,c("userId","GRS")]
	write.table(final,"GRS10SNPs_topconfig.txt", quote = FALSE, row.names=FALSE, sep="\t") 

#We save the file as a csv file (in PHESANT format)
write.table(final, "GRS10SNPs_topconfig.csv", sep=',', col.names=TRUE, row.names=FALSE, quote=TRUE, na="");


#We save the standardized data
#We save the GRS data
final<-D[,c("userId","zGRS")]
write.table(final,"zGRS10SNPs_topconfig.txt", quote = FALSE, row.names=FALSE, sep="\t") 

#We save the file as a csv file (in PHESANT format)
write.table(final, "zGRS10SNPs_topconfig.csv", sep=',', col.names=TRUE, row.names=FALSE, quote=TRUE, na="");

#We save both in the same file:
write.table(D,"GRS_and_zGRS_10SNPs_topconfig.txt", quote = FALSE, row.names=FALSE, sep="\t") 



################


#Sanity check: checking correlation with top SNP
	topSNP<-"rs56113850_Cdosage_proxyforNMR.txt"
	T<-read.table(paste(topSNP,sep=""), as.is=T, header=T)
	head(T)
	dim(T)
	T<-T[,2:3]
	names(T)<-c("userId", "rs56113850")

	SC<-left_join(D,T,by="userId")
	noNA_SC<-na.omit(SC)
	cor(noNA_SC$GRS,noNA_SC$rs56113850)
#	> cor(noNA_SC$GRS,noNA_SC$rs56113850)
#	          [,1]
#	[1,] 0.7314881

#We save the data
	write.table(SC,"topconfigalleleA_GRS_topSNPCiealleleB.txt", quote = FALSE, row.names=FALSE, sep="\t") 


#--------------------------------------
# PLOTS
#--------------------------------------

#Scatter plot of top SNP and GRS
pdf("./Plots/scatter_GRS_topSNP.pdf")
	print(qplot(GRS, rs56113850, data = SC))
	dev.off()

#Density plot of GRS
p<-ggplot(SC, aes(x=GRS)) +
  geom_histogram(position="identity", alpha=0.5)
pdf("./Plots/histGRS.pdf")
print(p)
dev.off()

#Density plot of zGRS
p<-ggplot(SC, aes(x=zGRS)) +
  geom_histogram(position="identity", alpha=0.5)
pdf("./Plots/histzGRS.pdf")
print(p)
dev.off()




