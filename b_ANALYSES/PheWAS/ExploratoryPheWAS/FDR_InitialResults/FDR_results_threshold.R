#######################################
#Jadwiga Buchwald
#7.3.2023
#Getting the results from our exploratory PheWAS
#######################################


#61 different variables are highlighted when using the 2-stage method

#----------------------------------------------------------------------
#ANALYSES
#----------------------------------------------------------------------

#DOing this interactively

#cd [path]/PHEWAS_2Stage/FDR_initial
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R

#-----------------------------------
#Rscript
#-----------------------------------

sessionInfo()
getwd()

#Reading in results

	N = read.csv("../NEVER/ResultsFromSaved/results-combined.txt", as.is=T, header=T,sep="\t")
	str(N)

	E = read.csv("../EVER/ResultsFromSaved/results-combined.txt", as.is=T, header=T,sep="\t")
	str(E)

	A = read.csv("../ALL/ResultsFromSaved/results-combined.txt", as.is=T, header=T,sep="\t")
	str(A)

#-----------------------------------------------
#Getting FDR sig results
	#FDR explained: https://www.mv.helsinki.fi/home/mjxpirin/HDS_course/material/HDS2_FDR.html

	#We will use the BH method which is what Millard et al. used in their PheWAS. BH is less stringent than BY.

		#It assumes the tests are independent but in practice (based on Pirinen's explanation, see link above),
		#it has been proven to be quite robust to violations of the independence assumption.

		#In short: the BH FDR ensures that the rate of false positives among our findings is below alfa. I.e. ~ As we find 61 FDR sig outcomes in total
		#0.05*61=3.05 we expect roughly 3 of these to be false positives.

		#Note, the BH FDR is a so called step-up procedure. So after ordering all n results by p-value we start by assessing p_n <= n/n*alfa, 
		#and then if this doesn't hold we go to the next biggest p-value and continue assessing the p-values in order until p_i <= i/n*alfa, 
		#after which we reject the null hypothesis for all i outcomes.   
#-----------------------------------------------
#FDR using BH

	#Note: in p.adjust "fdr" is an alias for "BH" so they give the same results. Checked this a bit further down.

	N$fdrp<-p.adjust(N$pvalue,"fdr") #false discovery rate allowed to be less than or equal to 0.05
	sum(N$fdrp<0.05) #: 2

	E$fdrp<-p.adjust(E$pvalue,"fdr") #false discovery rate allowed to be less than or equal to 0.05
	sum(E$fdrp<0.05) #: 29

	A$fdrp<-p.adjust(A$pvalue,"fdr") #false discovery rate allowed to be less than or equal to 0.05
	sum(A$fdrp<0.05) #: 47


#-----------------------------------------------
##Alternatively counting by hand the BH critical value
#-----------------------------------------------

#-----------
#NEVER	
#-----------
	totaltests <- dim(N)[1] #16103
	N$BHcv<-as.numeric(rownames(N))*0.05/totaltests

#Identifying the largest rank position with a p-value below or equal the respective BH critical value:
	max(which(N$pvalue <= N$BHcv)) #2
	N$BHcv[2] #6.210023e-06

#-->cut off is: 0.05x2/16103=6.210023e-06

#----------
#EVER
#----------

totaltests <- dim(E)[1]
	#> totaltests
	#[1] 16648

	E$BHcv <- as.numeric(rownames(E))*0.05/totaltests

#Identifying the largest rank position with a p-value below or equal the respective BH critical value:

	max(which(E$pvalue <= E$BHcv)) #29
	E$BHcv[29] #8.709755e-05


#-->cut off is: 0.05x29/16648=8.709755e-05


#----------
#ALL
#----------

totaltests <- dim(A)[1]
	#> totaltests
	#[1] 21094

	A$BHcv <- as.numeric(rownames(A))*0.05/totaltests
	max(which(A$pvalue <= A$BHcv)) #47
	A$BHcv[47] #0.0001114061

-->cut off is: 0.05x47/21094=0.0001114061 (=1.114061e-4)


################################################

#Sanity Check with the Ever sample: Comparing the different ways side by side, checking that we are indeed getting the BH results when using the command p.adjust with "fdr" (fdrp.BH<-p.adjust(pvalue,method = "BH") #supposed to be less stringent than BY)

	#with the p.adjust
		E$pval.BH<-p.adjust(E$pvalue,method = "BH") #false discovery rate allowed to be less than or equal to 0.05
		sum(E$pval.BH<0.05) #: 29
		E[1:29,c("pvalue","fdrp", "pval.BH")]

	#By hand
		n<-dim(E)[1] #16648

		#Counting the bh critical values
		E$pval.bh <- as.numeric(rownames(E)) * 0.05 /n
		E[1:31,c("pvalue", "pval.BH", "pval.bh")]

		#Identifying all with pval <= pval.bh
		pass <- which(E$pvalue <= E$pval.bh) #length=26
	
		#the largest rank position 
		max(pass) #29
	
		#Therefore the p-value threshold is 29*0.05/16648=
		E$pval.bh[29] # 8.709755e-05

##For self: Seeing what the difference would have been if we had used BY

#	E$pval.BY<-p.adjust(E$pvalue,method = "BY") #false discovery rate allowed to be less than or equal to 0.05
#	sum(E$pval.BY<0.05) #: 15

##################################################	
	
#------------------------------
#Seeing how many unique variables
#------------------------------
#We take the rows that had the fdr significant findings
	Nf <-subset(N,fdrp<0.05)
	Ef <-subset(E,fdrp<0.05)
	Af <-subset(A,fdrp<0.05)

#We save the data (They are already sorted by p-value as that is how PHESANT outputted them)
	write.table(Nf,"../SupplementaryTables/FDRinitial_c_Never.csv",sep=",",quote=T,row.names=F, col.names=T)
	write.table(Ef,"../SupplementaryTables/FDRinitial_b_Ever.csv",sep=",",quote=T,row.names=F, col.names=T)
	write.table(Af,"../SupplementaryTables/FDRinitial_a_All.csv",sep=",",quote=T,row.names=F, col.names=T)

#We take the first column
	nf<-Nf[,1]
	ef<-Ef[,1]
	af<-Af[,1]

#We concatenate these
	list<-c(nf,ef,af)
	length(list) #78
	length(unique(list)) #61


#We take the "description" column
	nf<-Nf[,1]
	ef<-Ef[,1]
	af<-Af[,1]

#We concatenate these
	list<-c(nf,ef,af)
	length(list) #78 
	length(unique(list)) #61


#Then checking if we get 61 also when considering both the description AND the model used, and saving the info on the 61 variables

#We take the "varName","description", "resType" and "Cat3_Title" columns
	nf<-Nf[,c("varName","description","resType", "Cat3_Title")]
	ef<-Ef[,c("varName","description","resType", "Cat3_Title")]
	af<-Af[,c("varName","description","resType", "Cat3_Title")]

#We concatenate these
	list<-rbind(nf,ef,af) #dim: 78 x 4
	u<-unique(list) 
		#> dim(u)
		#[1] 61  4

	U<-u[order(u$resType,u$Cat3_Title),]

	write.table(U, "./IntermediateData/Initial_61_FDRVariables.txt",quote=F,sep="\t",row.names = F)




