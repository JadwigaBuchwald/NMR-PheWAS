#15.3.2023
#Creating a Supplementary table of all our 61 variables: the coding in our initial analyses and then explaining the derived variables 


#-----------------
#Doing this interactively
#-----------------

#cd [path]/jadwiga/PHESANT/PHEWAS_2Stage/FDR_final
#RDir="/apps/statistics2/"
#${RDir}R-4.1.1/bin/R
#PHESANTDir="[path]/jadwiga/PHESANT/PHESANT-1.1"


#-----------------------------------
#Rscript
#-----------------------------------

sessionInfo()
getwd()




#-----------------
#Libraries:
#-----------------

library(tidyverse)
library(plyr)


#1. Get the list of 61: 

	#a) We get the following columns for 61 variables:
		#varName,varType,resType,description,Cat3_Title
		#30610,CONTINUOUS,LINEAR,Alkaline phosphatase,Blood assays,

		J = read.csv("../FDR_initial/IntermediateData/PHESANT_InitialResults_forForestquote.csv", as.is=T, header=T)
		str(J)
		dim(J)

		J<-J[,1:5]

	#b) And we sort all based on resType and Cat3_Title

		O<-J[order(J$resType,J$Cat3_Title),]

#2. From --variablelistfile="${PHESANTDir}variable-info/outcome-info.tsv" 
	#We get based on FieldID (varName) the relevant rows (we are esp. interested in DATA_CODING)
	#FieldID	TRAIT_OF_INTEREST	EXCLUDED	CAT_MULT_INDICATOR_FIELDS	CAT_SINGLE_TO_CAT_MULT	DATE_CONVERT	DATA_CODING	Path	Category	Field	ValueType
	#6149			NO_NAN			100538	UK Biobank Assessment Centre > Touchscreen > Health and medical history > Mouth	100046	Mouth/teeth dental problems	Categorical multiple


	#a) Read in the data and keep the wanted FieldID and DATA_CODING columns

		V<-read.csv(paste(PHESANTDir,"/variable-info/outcome-info.tsv",sep=""), as.is=T, header=T,sep="\t")
		dim(V)
		C<-V[,c("FieldID","DATA_CODING")]
		names(C)<-c("varName","DATA_CODING")

	#b) Merge with our data

		M<-join(O,C,by="varName",type="left")

	#c) Save as an excel file

		write.table(M,"./IntermediateData/List61Coding.csv",sep=",",quote=T,row.names=F, col.names=T)
	
		#Then read in to excel: under Data tab--> From Access and choosing all file types and finding the file--> delimiter=, and header=T

	#d) Then by hand using the find button (in outcome-info.tsv) to find the DATA_CODING for the variables for which varName has been altered and adding these to our excel in the place of the NAs

	-->Saving this as List61Coding_edited.csv
	
#3. By hand: I'll add a column get each of the unique codings and add them to the excel sheet in the relevant places.

	#[path]/jadwiga/PHESANT/PHESANT-1.1/ukb_data_codes/data_codes/datacode-100538.tsv
	#coding	meaning
	#-7	None of the above
	#-3	Prefer not to answer
	#1	Mouth ulcers
	#2	Painful gums
	#3	Bleeding gums
	#4	Loose teeth
	#5	Toothache
	#6	Dentures

	-->Saving this as List61Coding_editedlong.csv

4. Merging List61Coding_edited.csv with reassignment information from [path]/jadwiga/PHESANT/PHESANT-1.1/variable-info/data-coding-ordinal-info.txt

	E<-read.csv("./IntermediateData/List61Coding_edited.csv", as.is=T, header=T,sep=";")

#We merge the reassignement information with our dataframe E and then copy paste to our excel spread sheet
	Re<-read.table(paste(PHESANTDir,"/variable-info/data-coding-ordinal-info.txt",sep=""), as.is=T, header=T, sep=",")
	Re<-Re[,1:4]	
	names(Re)<-c("DATA_CODING",names(Re)[2:4])
	RI<-join(E,Re,by="DATA_CODING",type="left")

	#saving as tab delimited txt and as "," delimited csv
	write.table(RI,"./IntermediateData/List61_reassignments.csv",sep=",",quote=T,row.names=F, col.names=T)
	write.table(RI,"./IntermediateData/List61_reassignments.txt",sep="\t",quote=F,row.names=F, col.names=T)

	-->Opened in excel and copied the reassignements column and pasted to:

	###############################
	# List61Coding_editedlong.csv # 
	###############################
	#This we could include in our supplementary table
	#S1 Table b. Variable information on all 61 variables highlighted in our initial PheWAS analyses. 
	#Added the text: 
		Note: 
		All columns (except for 'Comments') have been obtained from PHESANT output and input files.
		Find more complete descriptions of variables and their coding at: https://biobank.ndph.ox.ac.uk/showcase/
		PHESANT recodes negative values as missing values



#5. Then taking the 3rd stage variables only and saving as:
	--> Saved as IntermediateData/List61Coding_editedlongReassignements.csv

#	I then by hand highlighted the variables for which we've made changes and added the descriptions for the derived variables under each of the highlighted variables in a new sheet.
#
#	As a sanity check I created the binary variable dentures(see D1_Binary.R) and made sure that I get the same n and the same logistic regression result as phesant gives.
#	Then I added by hand to our supplementary coding file the final coding used e.g. 1=Yes 0=No (for such variables) by phesant. 


	#S1 Table a. Variable information on all the third stage outcomes that we derived, recoded or analysed using a different model from our initial PheWAS (See S1 Table b). 
	#Added the text: 
	#Note: 
	#All derived variables have been marked with the prefix d. In grey above, the variable from which each new variable has been derived.
	#The prefix c stands for coding corrected, and t stands for treated differently i.e. analysed using a different model from the initial PheWAS runs. 
	
	

