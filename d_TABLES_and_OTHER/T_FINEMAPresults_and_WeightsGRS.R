#10.5.2023
#A) Creating a table with the finemap results (SupplementaryTable_FINEMAPresults) -ordered by SNP PROB
#B) Creating a table with the 10 GRS SNPS (Table_GRSWeights10SNPs) -ordered by chromosome and position
#Note: EA based on finemap and meta GWAS is alleleB. So I'll take these weights for alleleB and name alleleB as the EA. 
#However, as you can see in my GRS script, the UKB dosage data has been calculated for alleleA. 
#Therefore, for calculating the GRS I actually just multiplied the weights by -1 and used the dosage data (counted for alleleA) as such.

#Doing this ineractively in R


#cd [path]
#RDir="/apps/statistics2/"
#${RDir}R-4.1.1/bin/R

#-----------------------------------------------------------------------
#Rscript
#-----------------------------------------------------------------------

sessionInfo()
getwd()

library(dplyr)

#Directories
	DirFINEMAP<-[path]


#-----------------------------------------------------------------------
#A) Creating a table with the finemap results (SupplementaryTable_FINEMAPresults)
#-----------------------------------------------------------------------

#Reading in the almost ready data (created earlier 21.2.22, see bottom of this document)
	
	#Supplementary Table for FINEMAP results
		STF<-read.table(paste("./","topconfig9SNPs_YF.txt",sep=""),header=T)
	#Only keeping wanted columns
		W<-STF[,c("RSID","position","EA","NEA","YF_Minor","YF_maf","YF_betaJ","YF_seJ","YF_pJ")]

	#Getting the SNP PROB (posterior probability of being a causal SNP)
		PROB<- read.table(paste(DirFINEMAP,"topconfig9SNPs.snp",sep=""),header=T)
		W2<-PROB[,c("rsid","prob")]
		names(W2)<-c("RSID","prob")

	#Merging to get All Wanted Columns
		AWC<-left_join(W2,W,by="RSID")	

	#Wanted Order of Columns
		WOC<-AWC[,c("RSID","position","EA","NEA","YF_Minor","YF_maf","prob", "YF_betaJ","YF_seJ","YF_pJ")]

	#Saving this (lots of decimals)
		write.table(WOC,"SupplementaryTableFINEMAPresults_alldecimals.txt",quote=F,row.names=F) 

	#Making a version where EA and NEA are in one variable: EA/NEA
		WOC$EANEA<-paste(WOC$EA,"/",WOC$NEA,sep="") 
		WOCD<-WOC[,c("RSID","position","EANEA","YF_Minor","YF_maf","prob", "YF_betaJ","YF_seJ","YF_pJ")]

	#Getting rid of unnecessary decimals: 4 decimal places for SNP PROB and two decimal places for the other values
		WOCD[,c("YF_maf", "YF_betaJ","YF_seJ")]<-round(WOCD[,c("YF_maf", "YF_betaJ","YF_seJ")],digits=2)
		WOCD[,c("prob")]<-round(WOCD[,c("prob")],digits=4)
		WOCD[,c("YF_pJ")]<-signif(WOCD[,c("YF_pJ")],digits=3)
		write.table(WOCD,"SupplementaryTableFINEMAPresults_twoto4decimalplaces.txt",quote=F,row.names=F) 


#-----------------------------------------------------------------------
#B) Creating a table with the 10 GRS SNPS (ordered by chromosome and position)
#-----------------------------------------------------------------------
#1 RSID
#2 chr
#3 position
#4 Weight (BetaJ from YF Finemap for the chr 19 SNPs and Mtea beta for the chr 4 SNP)
#5 EA
#6 NEA
#7 MINOR
#8 MAF_UKB
#9 MAF_YF
#10 MAF_META
#If time then Location and NCBI dbANP functional annotation? 

#------------------------------------
#First getting from UKB
#------------------------------------
#1 RSID
#2 chr
#3 position
#5 EA
#6 NEA
#7 MINOR
#8 MAF_UKB

	UKB<-read.table(paste("./","GRS10SNPs.stats",sep=""),header=T)
	U<-UKB[,c("rsid","chromosome","position","alleleA","alleleB","minor_allele","minor_allele_frequency")]

#-----------------------------------------------------------------------
#As this has all the 10 SNPs I'll use this as the base and merge the FY weights(chr 19) and mafs and META weight(chr4) and mafs to this
#-----------------------------------------------------------------------

#So as the weights have been calculated for alleleB:
	U$EANEA<-paste(U$alleleB,U$alleleA,sep="/")
	U<-U[,c("rsid","chromosome","position","EANEA","minor_allele","minor_allele_frequency")]
	names(U)<-c("RSID","chr","position","EANEA","MINOR","MAF_UKB")



#------------------------------------
#Then from YF
#------------------------------------
#1 RSID
#(2 chr)
#(3 position)
#(5 EA)
#(6 NEA)
#(7 MINOR)
#8 MAF_YF
#9 WEIGHT for all chromosome 19 SNPs

#chr19 SNPs: we read in the dataset created in A
	FY19<-read.table(paste("./","SupplementaryTableFINEMAPresults_alldecimals.txt",sep=""),header=T)
	FY19<-FY19[order(FY19$position),]
	FY19<-FY19[,c("RSID","YF_maf","YF_betaJ")]
	names(FY19)<-c("RSID","MAF_YF","WEIGHT")


#------------------------------------
#Then from meta
#------------------------------------
#1 RSID
#(2 chr)
#(3 position)
#(7 MINOR)
#8 MAF_META
#9 WEIGHT for chromosome 4 SNP

	WeightsCHR4<-"../../../Genotypes/GRS_2021/chr4SNP_details.txt"

	W4<-read.table(paste(WeightsCHR4,sep=""), as.is=T, header=T,sep="\t")
	W4[,c("RSID","Major_Minor","maf","b_minor")]
		#  CHR       RSID Major_Minor b_minor
		#1   4 rs36103218         T/C   0.175

	S[1,c("rsid","chromosome","alleleA","alleleB")]
		#        rsid chromosome alleleA alleleB
		#1 rs36103218          4       C       T

	#-->so weight for alleleB=EA is -0.175 (this way is comparable with the chr19 betaJs)

	W4$weight_alleleB<--0.175
	META<-W4[,c("RSID","maf","weight_alleleB")]
	names(META)<-c("RSID","MAF_META","WEIGHT")

#------------------------------------
#Then creating the wanted table
#------------------------------------

#COMBINING THE DATASETS INTO ONE
	W1<-left_join(U,META[,-which(colnames(META)=="WEIGHT")],by="RSID")
	W2<-left_join(W1,FY19[,-which(colnames(FY19)=="WEIGHT")],by="RSID")

	WEIGHTS<-rbind(META[1,c("RSID","WEIGHT")],FY19[,c("RSID","WEIGHT")])

	ALL<-left_join(W2,WEIGHTS)

#DECIMAL PLACES
	ALL[,c("MAF_UKB","WEIGHT")]<-round(ALL[,c("MAF_UKB","WEIGHT")],digits=4)
	ALL[,c("MAF_YF")]<-round(ALL[,c("MAF_YF")],digits=2)

#SAVING
	write.table(ALL,"Table_GRSWeights10SNPs.txt",quote=F,row.names=F) 

#CREATING A VERSION WITH WEIGHT(MAF)

	ALL$WEIGHT_MAF<-ifelse(!(is.na(ALL$MAF_YF)),paste(ALL$WEIGHT,"(MAF_YF=",ALL$MAF_YF,")",sep=""), paste(ALL$WEIGHT,"(MAF_META=",ALL$MAF_META,")",sep="") )
	names(ALL)


	A<-ALL[,c("RSID", "chr", "position","EANEA","MINOR","MAF_UKB","WEIGHT_MAF")]

	write.table(A,"Table_GRSWeights10SNPs_NICER.txt",quote=F,row.names=F) 

#------------------------------------------------------------------------------------------------------------------------------------------------

#21.1.22
#creating a table with the following columns: "RSID","position", "NEA","EA", yf_betaj, yf_se, yf_p, yf_maf, alleleA, allele, ukwb_maf 

#Doing this ineractively in R


#cd [path]
#RDir="/apps/statistics2/"
#${RDir}R-4.1.1/bin/R

#-----------------------------------------------------------------------
#Rscript
#-----------------------------------------------------------------------

sessionInfo()
getwd()

library(dplyr)

#reading in the .config information

	jb<-"[path]FINEMAP/Output/topconfig_JointBetas.txt"
	Config<-read.table(paste(jb),sep=",",header=T)
	Config$zJ<-Config$betaJ/Config$seJ 
	Config$pJ<-2*pnorm(-abs(Config$zJ))
	head(Config)
		#> head(Config)
		#          SNP     betaJ       seJ         zJ            pJ
		#1 rs189621498  0.612669 0.0763152   8.028139  9.896267e-16
		#2  rs74719953 -0.358069 0.0667800  -5.361920  8.234213e-08
		#3  rs34945948  1.027630 0.0659601  15.579570  1.002266e-54
		#4  rs12985907 -0.800270 0.0345571 -23.157904 1.210219e-118
		#5   rs1801272 -1.010320 0.0831911 -12.144568  6.130527e-34
		#6   rs7250713  0.325132 0.0342050   9.505394  1.992922e-21
	C<-Config[,c(1:3,5)]
	names(C)<-c("RSID","YF_betaJ","YF_seJ","YF_pJ")	

#reading in the .snp information (to get rsid chromosome position allele1 allele2 maf beta se z(for getting p))
	s<-"[path]FINEMAP/Output/topconfig9SNPs.snp"
	S<-read.table(paste(s),header=T,stringsAsFactors=F) 
	S$p<-2*pnorm(-abs(S$z))
	head(S)
	dim(S)
	YF<-S[,c(2:9,17)]
		#> names(YF)
		#[1] "rsid"       "chromosome" "position"   "allele1"    "allele2"
		#[6] "maf"        "beta"       "se"         "p"

	names(YF)<-c("RSID","chromosome","position","allele1_snpfile","allele2_snpfile","YF_maf","YF_beta","YF_se","YF_p")

	FM<-left_join(C,YF,by="RSID")
	print(FM)

#Getting ea, nea, eaf from .meta file(LDStore)
	ldstore<-"[path]FINEMAP/Output/topconfig9SNPs.meta"
	l<-read.table(paste(ldstore),header=T,stringsAsFactors=F) 
	l<-l[,c("RSID","position","A_allele","B_allele","A_allele_freq","B_allele_freq")]
	names(l)<-c("RSID","position","NEA","EA","YF_neaf","YF_eaf")
	l$YF_Major<-ifelse(l$YF_neaf>0.5,l$NEA,l$EA)
	l$YF_Minor<-ifelse(l$YF_neaf>0.5,l$EA,l$NEA)
	l<-l[,-2] #we drop the position

	YFall<-left_join(FM,l,by="RSID")
	print(YFall)

	write.table(YFall,"topconfig9SNPs_YF.txt",quote=F,row.names=F) 







