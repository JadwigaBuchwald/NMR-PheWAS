#10.1.2022

#Having a closer look at the SNPs that ended up in our new FINEMAP vs those that were filtered

#1. summary stats of maf and pos
#2. Histogram of BP positions (N(SNP)=12060 vs N(SNP)=10133)
#3. Scatter plot of mafs (N(SNP)=10133, maf in UK vs maf in FRYFS)
#4) Checking if noneff_allele=alleleA (as looks like based on eyeballing the data)


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
	library(ggplot2)
	
#Reading in data
d<-read.table("./Data/FM_Availability.txt", header=T, as.is=T, na.string=c("NA","nan",-9,"NaN","."))

#--------------------------------------
#1) SUMMARY STATISTICS
#--------------------------------------

#UKBIOBANK
#how many rsids available in the ukb
length(d$rsid_uk[!is.na(d$rsid_uk)]) #10133
#how many non missing mafs in the ukb
length(d$maf_uk[!is.na(d$maf_uk)]) #10133
summary(d$maf_uk)
#	> summary(d$maf_uk)
#   	Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 	0.0000  0.0339  0.1095  0.1592  0.2595  0.4999    1927
#how many non missing positions in the ukb
length(d$position_uk[!is.na(d$position_uk)]) #10133
summary(d$position_uk)
#	> summary(d$position_uk)
#	    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
#	38853297 39731747 40744346 40845680 41744831 43852500     1927



#FRYFS
#how many non missing mafs
length(d$maf[!is.na(d$maf)]) #12060
summary(d$maf)
	#> summary(d$maf)
	#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
	#0.01002 0.04070 0.10698 0.15960 0.26098 0.49998
#how many non missing positions
length(d$position[!is.na(d$position)]) #12060
summary(d$position)
	#> summary(d$position)
	#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
	#38853297 39913615 41118990 41192308 42267642 43852500

#--------------------------------------
#2) HISTOGRAM
#--------------------------------------

#Getting the histograms of the Base pair positions of the SNPs used in our new FINEMAP vs old FINEMAP in the same plot

#Preparing Data for plotting

	#We first need to create a stacked dataset (we repeat the 10133 so we get ALL vs SUBSET)
	subset<-subset(d,!is.na(d$maf_uk)) #we get the available data (SUBSET)

	#We create a new variable "Data"
	subset$Data="Subset, n(SNP)=10133" 
	d$Data="All, n(SNP)=12060"

	#We create a new variable MAF
	subset$MAF=subset$maf_uk
	d$MAF=d$maf

	#We stack subset to the whole data
	stacked<-rbind(d,subset) 

#---------------------

#Histograms in same plot, data=stacked, group=Data(all vs subset)

	p<-ggplot(stacked, aes(x=position, fill=Data, color=Data)) +
  	geom_histogram(position="identity", alpha=0.5)

#Printing above in gray scale + legend on top + legend text bigger + adding vertical lines to denote the area in which the previous 13 SNP configuration resided in.

	#Adding lines to denote previous 13 SNP area
	#Drawing the minimum and maximum positions for the top configuration SNPs in to the figure
		vertical.lines=c(40685483,41909719)

	pdf("./Plots/histgg_lines_10133vs12060.pdf")
		print(p+ scale_color_grey()+scale_fill_grey() + theme_classic()+ 
		theme(legend.position="top") + theme(legend.text=element_text(size=rel(1.2)))
		+geom_vline(xintercept = vertical.lines, linetype="dashed")
		+labs(title="Finemapped SNPs plot",x="Base pair position", y = "Count"))
	dev.off()


#--------------------------------------
#3) SCATTER PLOT OF MAFs
#--------------------------------------

#with labels
pdf("./Plots/scatter_maf_10133_labels.pdf")
	print(qplot(maf, maf_uk, data = d,xlab="maf_yf"))
	dev.off()

#--------------------------------------
#4) Checking if noneff_allele=alleleA (as looks like based on eyeballing the data)
#--------------------------------------

#we see if noneff_allele=alleleA: YES!
identical(subset$noneff_allele,subset$alleleA) #TRUE
same<-subset$noneff_allele==subset$alleleA
table(same)
#same
#TRUE
#10133
