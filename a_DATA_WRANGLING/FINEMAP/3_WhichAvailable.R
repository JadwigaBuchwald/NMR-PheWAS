#10.1.2022

#CREATING VECTOR "WhichAvailable.txt" BASED ON WHICH WE OBTAIN SUBSET OF Z AND LD FILES IN BASH

#Also along the way creating data (->FM_regionAvailability.txt) based on which we can then create plots of the old and new data (comparing positions/mafs of the new / old SNPs)
	a) Creating a variable MARKERNAME to both files (ukb_QCOK_unique_7col.stats & YFSFR_nmr_Extended_chr19_corrected_5col.z) based on alleles (ATTA/AGGA/ACCA/CTTC/CGGC/GTTG)
	b) Joining fryfs file to ukb file
	c) We obtain lists of which rows in yfsfr are available -> "WhichAvailable" in ukb: this is for filtering the z and ld files
	
#-----------------------------------------------------
#In bash

#declaring working directory (change directory): 
	cd /[path]

#declaring directory paths: 
	RDir="/[path]/"

#Opening R
	${RDir}R-4.1.1/bin/R

#-----------------------------------------------------

#Interactively in R


#-----------------------------------------------------------------------
#Rscript
#-----------------------------------------------------------------------

sessionInfo()

#libraries 
	library(tidyr)
	library(plyr)
	library(dplyr)


#Reading in the data

	#YFSFR
	YF<-read.table("./Data/YFSFR_nmr_Extended_chr19_corrected_5col.z", header=T, as.is=T, na.string=c("NA","nan",-9,"NaN","."))
	print(head(YF))
	print(str(YF))
	print(dim(YF))


	#UKBB

	UK<-read.table("./Data/ukb_QCOK_unique_7col.stats", header=T, as.is=T, na.string=c("NA","nan",-9,"NaN","."))
	print(head(UK))
	print(str(UK))
	print(dim(UK))

	#Looking at the heads it seems to be clear that noneff_allele=alleleA and eff_allele=alleleB. But we won't take this granted.

#-------------------------------------
#a) Creating a variable MARKERNAME to both files based on position and alleles (ATTA/AGGA/ACCA/CTTC/CGGC/GTTG)
#This variable can then be used to join the files
#-------------------------------------


	#YFSFR
	YF$MARKERNAME<-NA
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="A" & YF$eff_allele=="T" | YF$noneff_allele=="T" & YF$eff_allele=="A", paste(YF$position,"_","ATTA",sep=""), YF$MARKERNAME)
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="A" & YF$eff_allele=="G" | YF$noneff_allele=="G" & YF$eff_allele=="A", paste(YF$position,"_","AGGA",sep=""), YF$MARKERNAME)
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="A" & YF$eff_allele=="C" | YF$noneff_allele=="C" & YF$eff_allele=="A", paste(YF$position,"_","ACCA",sep=""), YF$MARKERNAME)
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="C" & YF$eff_allele=="T" | YF$noneff_allele=="T" & YF$eff_allele=="C", paste(YF$position,"_","CTTC",sep=""), YF$MARKERNAME)
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="C" & YF$eff_allele=="G" | YF$noneff_allele=="G" & YF$eff_allele=="C", paste(YF$position,"_","CGGC",sep=""), YF$MARKERNAME)
	YF$MARKERNAME<-ifelse(YF$noneff_allele=="G" & YF$eff_allele=="T" | YF$noneff_allele=="T" & YF$eff_allele=="G", paste(YF$position,"_","GTTG",sep=""), YF$MARKERNAME)

	print(names(YF))
	print(head(YF))
	print(dim(YF))


	#UK

	UK$MARKERNAME<-NA
	UK$MARKERNAME<-ifelse(UK$alleleA=="A" & UK$alleleB=="T" | UK$alleleA=="T" & UK$alleleB=="A", paste(UK$position,"_","ATTA",sep=""), UK$MARKERNAME)
	UK$MARKERNAME<-ifelse(UK$alleleA=="A" & UK$alleleB=="G" | UK$alleleA=="G" & UK$alleleB=="A", paste(UK$position,"_","AGGA",sep=""), UK$MARKERNAME)
	UK$MARKERNAME<-ifelse(UK$alleleA=="A" & UK$alleleB=="C" | UK$alleleA=="C" & UK$alleleB=="A", paste(UK$position,"_","ACCA",sep=""), UK$MARKERNAME)
	UK$MARKERNAME<-ifelse(UK$alleleA=="C" & UK$alleleB=="T" | UK$alleleA=="T" & UK$alleleB=="C", paste(UK$position,"_","CTTC",sep=""), UK$MARKERNAME)
	UK$MARKERNAME<-ifelse(UK$alleleA=="C" & UK$alleleB=="G" | UK$alleleA=="G" & UK$alleleB=="C", paste(UK$position,"_","CGGC",sep=""), UK$MARKERNAME)
	UK$MARKERNAME<-ifelse(UK$alleleA=="G" & UK$alleleB=="T" | UK$alleleA=="T" & UK$alleleB=="G", paste(UK$position,"_","GTTG",sep=""), UK$MARKERNAME)

	print(names(UK))
	print(head(UK))
	print(dim(UK))
	names(UK)<-c("rsid_uk", "position_uk","alleleA","alleleB","maf_uk","minor_allele","major_allele","MARKERNAME")

#-------------------------------------
#b) Joining fryfs file to ukb file  
#-------------------------------------

#JOINING

	d2<-join(YF,UK,by="MARKERNAME",type="left")

	names(d2)
	head(d2)
	dim(d2) #[1] 12060    13
	length(unique(d2$MARKERNAME)) #[1] 12060
	length(unique(d2$rsid)) #12060
	length(unique(d2$rsid_uk)) #10134 
	length(unique(d2$position)) #12060
	length(unique(d2$position_uk)) #10134

	#How many non missing values are there?
	length(d2$rsid_uk[!is.na(d2$rsid_uk)]) #10133 -->This is how many SNPs we have available for FINEMAP

write.table(d2,"./Data/FM_Availability.txt",sep="\t",quote=F,row.names=F)


#-------------------------------------
#c) We create "WhichAvailable.txt" list of the rows to be kept in z file and rows/columns in ld file
#-------------------------------------

#We get the vector of rows to be kept in the z file and the rows+columns in the ld file
	available<-which(!is.na(d2$rsid_uk))

write.table(available,"./Data/WhichAvailable.txt",sep="\t",quote=F,row.names=F, col.names=F)
