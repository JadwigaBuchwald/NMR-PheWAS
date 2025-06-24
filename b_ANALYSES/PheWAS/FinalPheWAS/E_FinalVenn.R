#4.4.2023
#Jadwiga Buchwald

#Drawing the final Venn Diagram of our FDR significant results ((also marking the four FEV outcomes that had a statistically significant difference between their effect sizes for the ever and never smokers)


#----------------------------------------------------------------------
#ANALYSES
#----------------------------------------------------------------------

#DOing this interactively

#cd /[path]/
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R
#WorkDir="/[path]"

#-----------------------------------
#Rscript
#-----------------------------------

sessionInfo()
getwd()
#libraries
	library(dplyr) #for ggplot
	library(ggplot2) #for ggplot
	library(tidyr)
	library(plyr)

.libPaths("/[path]")
#install.packages("")
	library(VennDiagram)
	library(RColorBrewer)
	library(labeling)

#Reading in results
	Binary = read.csv("./IntermediateData/Final_25_BinaryLogisticResults.txt", as.is=T, header=T,sep="\t")
	str(Binary) # 25 x 30


	Ordinal = read.csv("./IntermediateData/Final_8_OrderedLogisticResults.txt", as.is=T, header=T,sep="\t")
	str(Ordinal) # 8 x 30

	Linear = read.csv(paste(WorkDir,"/FDR_final/IntermediateData/Final_38_LinearResults.txt",sep=""), as.is=T, header=T,sep="\t")
	str(Linear) #38 x 30

#Combining them

	Results <- rbind(Linear,Binary,Ordinal) #71 x30

#Saving this as a SupplementaryTable

	write.table(Results,"../SupplementaryTables/FinalResults71variables.txt",sep="\t",quote=TRUE,row.names=FALSE) 
	write.table(Results,"../SupplementaryTables/FinalResults71variables.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)

#Saving the same table but sorting based on p-value
	WS <- read.table("../SupplementaryTables/FinalResults71variables.txt",sep="\t",header=TRUE)
	A<- WS[,1:10]
	A<-A[order(A$pvalue_A),]
	write.table(A,"../SupplementaryTables/ST_Final71Results/d_71All_sortedbyp.txt",sep="\t",quote=TRUE,row.names=FALSE) 

	E<- WS[,c(1,2,3,11:17)]
	E<-E[order(E$pvalue_E),]
	write.table(E,"../SupplementaryTables/ST_Final71Results/e_71Ever_sortedbyp.txt",sep="\t",quote=TRUE,row.names=FALSE) 

	N<- WS[,c(1,2,3,18:24)]
	N<-N[order(N$pvalue_N),]
	write.table(N,"../SupplementaryTables/ST_Final71Results/f_71Never_sortedbyp.txt",sep="\t",quote=TRUE,row.names=FALSE) 

#Saving the same table but sorting based on p-value for Ever vs Never
	EvsN<-WS[,c(1,2,3,29,30,17,24,11:16,18:23)]
	EvsN<-EvsN[order(EvsN$pvalue_EvsN),]
	write.table(EvsN,"../SupplementaryTables/ST_Final71Results/c_71EvervsNever.txt",sep="\t",quote=TRUE,row.names=FALSE) 

#-----------------------------------------------
#Obtaining the lists for drawing the Venn Diagram
#-----------------------------------------------
	#We read in the Wholeset
	WS <- read.table("../SupplementaryTables/FinalResults71variables.txt",sep="\t",header=TRUE)
	
	table(WS$resType)
          	#LINEAR  LOGISTIC-BINARY ORDERED-LOGISTIC
          	#    38               25                8

	N <- subset(WS,WS$FDRsig_N==1)[,c("Phenotype")]
	length(N) #3
	E <- subset(WS,WS$FDRsig_E==1)[,c("Phenotype")]
	length(E) #26
	A <- subset(WS,WS$FDRsig_A==1)[,c("Phenotype")]
	length(A) #45


#-----------------------------------------------
#Printing all the variables that were not FDR sig in any of the subsets
#-----------------------------------------------

	WS$SumSig<-rowSums(WS[,c("FDRsig_A","FDRsig_E", "FDRsig_N")], na.rm=TRUE)

#Lets print the 14 that were not fdr sig in any group

	Insig <- subset(WS,WS$SumSig==0)[,c("Phenotype","pvalue_A","pvalue_N","pvalue_E")]
	dim(Insig) # 14 x4 
		Insig$Phenotype

		 [1] "n48 - Waist circumference"
		 [2] "n23007 - Antigen for human cytomegalovirus"
		 [3] "n30710 - C-reactive protein"
		 [4] "n26536 - BrainSegVol-to-eTIV (whole brain)"
		 [5] "n26678 - Volume of VA (left hemisphere)"
		 [6] "d1488 - Tea drinker"
		 [7] "d1498 - Coffee drinker (excluding decaf)"
		 [8] "d1498 - Coffee drinker (including decaf)"
		 [9] "d1508 - Decaf vs caffeinated coffee"
		[10] "d1508 - Decaf vs no coffee"
		[11] "d1508 - Instant vs no coffee"
		[12] "d1508 - Other caffeinated coffee vs none"
		[13] "d20116 - Initiation"
		[14] "d22506 - Occasional vs daily smoker"




#--------------------------------------------------------------------
#Drawing the venn diagram with our list of shortened variable descriptions
#--------------------------------------------------------------------

#simple plot
	venn.diagram(
  		x = list(A, E, N),
  		category.names = c("All (45)" , "Ever (26) " , "Never (3)"),
  		filename = './IntermediateData/FDRfinal_venn_diagrammApril2023.png',
  		output=TRUE
	)


#---------------------------
#Venn diagram - pretty version
#---------------------------
#FDR

#Script originally taken from here:
#https://r-graph-gallery.com/14-venn-diagramm.html

# Prepare a palette of 3 colors with R colorbrewer:
	#library(RColorBrewer)
		myCol <- brewer.pal(3, "Pastel2")
		myColreversed<- c(myCol[2],myCol[1],myCol[3])	
			#1=Green
			#2=Red
			#3=blue

venn.diagram(
        x = list(A, E, N),
        category.names = c("Ever (26) ","All (45)" , "Never (3)"),
        filename = '../PLOTS/FDRfinal_VennDiagram.png',
        
        # Circles
        lwd = 2,
        lty = 'blank',
        fill = myCol,
        
        # Numbers
        cex = .8,
        fontface = "bold",
        fontfamily = "sans",
        
        # Set names
        cat.cex = 0.8,
        cat.fontface = "bold",
        cat.fontfamily = "sans",
        rotation = 1,
	rotation.degree=180
)

#Trying to meet the plos genetics recuirements: arial, size 8 point, tiff
#Also notice that I had to make quite a few adjustments so that it would be comparable to the initial venn diagram.

venn.diagram(
        x = list(A,E,N),
        category.names = c("All (45)","Ever (26)", "Never (3)"),
        filename = '../PLOTS/FDRfinal_VennDiagram_arial.tiff',
	disable.logging = TRUE,
        
        # Output features
       imagetype="tiff" ,
        height = 2250 , 
        width = 2250 , 
        resolution = 600,
        compression = "lzw",
	euler.d =TRUE,
	scaled= TRUE,	
        
        # Circles
        lwd = 2,
        lty = 'blank',
        fill = myCol,
        
        # Numbers
        cex = 0.8,
        fontface = "bold",
        fontfamily = "arial",
        
        # Set names
        cat.cex = 0.8,
        cat.fontface = "bold",
	 cat.default.pos = "outer",
        cat.pos = c(45, 315, 180),
        cat.dist = c(0.025, 0.025, 0.025),
        cat.fontfamily = "arial",
	rotation = 1,
	rotation.degree=180
)


#---------------------------------
#Lists matching the Venn diagram
#---------------------------------

#Getting the lists of variables for the intersections
	overlap <- calculate.overlap(list(A,E,N))
	overlap

$a5
[1] "n30610 - Alkaline phosphatase"     "n30620 - Alanine aminotransferase"

$a2
 [1] "n2887 - CPD previously"
 [2] "n2897 - Age stopped smoking"
 [3] "n3456 - CPD currently"
 [4] "*n20150 - FEV1, best measure"
 [5] "*n20154 - FEV1, predicted %"
 [6] "*n20256 - FEV1 Z-score"
 [7] "n20258 - FEV1 to FVC ratio Z-score"
 [8] "*n3063 - FEV1"
 [9] "d20116 - Cessation"
[10] "1408 - Cheese intake"
[11] "c1239 - Current smoking"
[12] "c3506 - Smoking compared to 10 years previous"

$a4
[1] "nd1488 - Tea quantity"

$a6
character(0)

$a1
 [1] "n23109 - Impedance of arm"
 [2] "n23443 - Degree of unsaturation"
 [3] "n23449 - Linoleic acid"
 [4] "n23451 - Omega-3 to total fatty acids ratio"
 [5] "n23459 - Omega-6 to omega-3 ratio"
 [6] "n23462 - Glycine"
 [7] "n30150 - Eosinophill count"
 [8] "n30210 - Eosinophill %"
 [9] "n30630 - Apolipoprotein A"
[10] "n30680 - Calcium"
[11] "n30700 - Creatinine"
[12] "n30750 - Glycated haemoglobin"
[13] "n30880 - Urate"
[14] "n1807 - Father's age at death"
[15] "n25874 - Volume of grey matter in supracalcarine cortex"
[16] "nd1498 - Coffee quantity (excluding decaf)"
[17] "nd1498 - Coffee quantity (including decaf)"
[18] "n30510 - Creatinine in urine"
[19] "n30530 - Sodium in urine"
[20] "1787 - Maternal smoking around birth"
[21] "6149#100 - Free of mouth/teeth dental problems"
[22] "d1508 - Ground vs no coffee"
[23] "d1508 - Instant vs other caffeinated coffee"
[24] "20003#1140867092 - Serenace medication"
[25] "41200#L978 - Other operations on blood vessel"
[26] "41200#M706 - Radioactive seed implantation into prostate"
[27] "41272#W384 - Prosthetic replacement of hip joint"
[28] "20644 - Liking coffee without sugar"
[29] "20652 - Liking dark chocolate"
[30] "c1249 - Past tobacco smoking"

$a3
 [1] "n20151 - FVC, best measure"
 [2] "n20257 - FVC Z-score"
 [3] "n3062 - FVC"
 [4] "6149#6 - Mouth/teeth dental problems: Dentures"
 [5] "20003#1140861958 - Simvastatin medication"
 [6] "20004#1102 - Leg artery bypass surgery"
 [7] "1980 - Worrier / anxious feelings"
 [8] "41210#G725 - Anastomosis of ileum to anus (secondary)"
 [9] "41272#G725 - Anastomosis of ileum to anus"
[10] "41272#L601 - Endarterectomy of femoral artery"
[11] "20641 - Liking cigarette smoking"
[12] "1180 - Chronotype"

$a7
character(0)
#----------------------------------------------------
#I'll add the text in InkScape
#----------------------------------------------------

#---------------
Only All:
#---------------
[1] "n23109 - Impedance of arm"
 [2] "n23443 - Degree of unsaturation"
 [3] "n23449 - Linoleic acid"
 [4] "n23451 - Omega-3 to total fatty acids ratio"
 [5] "n23459 - Omega-6 to omega-3 ratio"
 [6] "n23462 - Glycine"
 [7] "n30150 - Eosinophill count"
 [8] "n30210 - Eosinophill %"
 [9] "n30630 - Apolipoprotein A"
[10] "n30680 - Calcium"
[11] "n30700 - Creatinine"
[12] "n30750 - Glycated haemoglobin"
[13] "n30880 - Urate"
[14] "n1807 - Father's age at death"
[15] "n25874 - Volume of grey matter in supracalcarine cortex"
[16] "nd1498 - Coffee quantity (excluding decaf)"
[17] "nd1498 - Coffee quantity (including decaf)"
[18] "n30510 - Creatinine in urine"
[19] "n30530 - Sodium in urine"
[20] "1787 - Maternal smoking around birth"
[21] "6149#100 - Free of mouth/teeth dental problems"
[22] "d1508 - Ground vs no coffee"
[23] "d1508 - Instant vs other caffeinated coffee"
[24] "20003#1140867092 - Serenace medication"
[25] "41200#L978 - Other operations on blood vessel"
[26] "41200#M706 - Radioactive seed implantation into prostate"
[27] "41272#W384 - Prosthetic replacement of hip joint"
[28] "20644 - Liking coffee without sugar"
[29] "20652 - Liking dark chocolate"
[30] "c1249 - Past tobacco smoking"


#---------------
Only Ever:
#---------------
 [1] "n20151 - FVC, best measure"
 [2] "n20257 - FVC Z-score"
 [3] "n3062 - FVC"
 [4] "6149#6 - Mouth/teeth dental problems: Dentures"
 [5] "20003#1140861958 - Simvastatin medication"
 [6] "20004#1102 - Leg artery bypass surgery"
 [7] "1980 - Worrier / anxious feelings"
 [8] "41210#G725 - Anastomosis of ileum to anus (secondary)"
 [9] "41272#G725 - Anastomosis of ileum to anus"
[10] "41272#L601 - Endarterectomy of femoral artery"
[11] "20641 - Liking cigarette smoking"
[12] "1180 - Chronotype"


#---------------
Ever AND All
#---------------
 [1] "n2887 - CPD previously"
 [2] "n2897 - Age stopped smoking"
 [3] "n3456 - CPD currently"
 [4] "*n20150 - FEV1, best measure"
 [5] "*n20154 - FEV1, predicted %"
 [6] "*n20256 - FEV1 Z-score"
 [7] "n20258 - FEV1 to FVC ratio Z-score"
 [8] "*n3063 - FEV1"
 [9] "d20116 - Cessation"
[10] "1408 - Cheese intake"
[11] "c1239 - Current smoking"
[12] "c3506 - Smoking compared to 10 years previous"

#---------------
Never/Ever/All
#---------------
n30610 - Alkaline phosphatase    
n30620 - Alanine aminotransferase

#---------------
Never and All
#---------------
nd1488 - Tea quantity
