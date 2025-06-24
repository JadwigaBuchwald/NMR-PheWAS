#Jadwiga Buchwald
#Gen to A_allele dosage (0 0 1 --> 0; 0 1 0 --> 1; 1 0 0 --> 2)

#Function for converting gen files to dosage format

#-----------------------------------------------------------------------
#Example of Rscript for running the function
#-----------------------------------------------------------------------

#FUNCTIONS:
#source("/fs/projects/ukbb/jadwiga/RFunctions/GenotypeFileConversion.R")

#Top SNP
#GenToDosage(Genin="rs56113850_inclusionsample.gen", Samplein="rs56113850_inclusionsample.sample", Dosageout="rs56113850_dosage")

#GRS 13 SNPs
#GenToDosage(Genin="GRS13SNPs_inclusionsample.gen", Samplein="GRS13SNPs_inclusionsample.sample", Dosageout="GRS13SNPs_dosage")

#-----------------------------------------------------------------------
#The function
#-----------------------------------------------------------------------


#Note: Interestingly reads T in as T when more than one SNPs in input data and as TRUE when only one SNP. 
#Doesn't matter though as we are only outputting the rsid, userid and dosage of A_allele (listed in snpstats file) 


GenToDosage<-function(Genin,Samplein,Dosageout){

	a<-read.table(paste(Genin,sep=""), as.is=T)
	print(paste(Genin,"read in to a.",sep=" "))
	print("dim(a)")
	print(dim(a))

	#we take columns 2-6, for first row eg for top SNP: 19:41353107_T_C rs56113850 41353107 T C
	tmp<-a[,c(2,3,4,5,6)]
	print("head(tmp)")
	print(head(tmp))
	
	#We take all the columns from 7 onwards. These contain the genotype infromation
	#Columns 7:10 for first row eg: 0.00000 0.00000 1.00000 0.00000 (so for this example TT, TC, CC)
	tmp2<-a[,7:length(a[1,])]

	#We create "out" matrix with as many rows as in tmp2 and ncol= (number of columns in tmp2) / 3. We set all values to NA. 
	ind<-seq(1,length(tmp2[1,]),by=3)
	print("head(ind)")
	print(head(ind))

	out<-matrix(nrow=length(tmp2[,1]),ncol=length(tmp2[1,])/3, data=NA)

	#We start filling in the values with the alleleA (T allele in this example) dosages 
	j<-0
		for(i in ind){
		j<-j+1
		out[,j]<- tmp2[,i]*2+tmp2[,i+1]*1
		out[,j] <- ifelse(tmp2[,i]==0&tmp2[,i+1]==0&tmp2[,i+2]==0,NA,out[,j])
		}
	out<-as.data.frame(out)
	print("dim(out)")
	print(dim(out))
	print("head(out)[,1:10]")
	print(head(out)[,1:10])



	#We read in sample file skipping first row ("ID_1 ID_2 missing sex")  and the second row ("0 0 0 D")
	sample_file<-read.table(paste(Samplein,sep=""), header=F, as.is=T, skip=2)
	

	#We name the columns in out with the sample ids
	names(out)<-c(sample_file[,1])
	print("dim(out)")
	print(dim(out))
	print("head(out)[,1:10]")
	print(head(out)[,1:10])



	#We combine the tmp and out files
	a<-cbind(tmp,out)
	print("head(a)[,1:10]")
	print(head(a)[,1:10])

	#We transform the data so that now the sample ids are as rows in the first column instead of being as columns. First 5 rows are the SNP annotation fields.
	a2<-as.data.frame(t(a))
	print("head(a2)")
	print(head(a2))

	#We keep only the rsid row and the sample id rows

	a3<-data.frame(rownames(a2),a2)
	final_out<-a3[c(2,6:length(a3[,1])),]
	final_out[1,1]<-"userId"

	print("dim, str and head for:final_out.")
	print(dim(final_out))
	print(str(final_out))
	print(head(final_out))
	
	
	#we save the file as a txt file
	write.table(final_out,paste(Dosageout,".txt",sep=""),col.names=F,row.names=F, sep="\t",quote=F)


}


