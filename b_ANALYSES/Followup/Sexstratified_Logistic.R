#Checking for sex differences.
#1) Running all the variables from the final PheWAS (71 variables) analyses (ALL/EVER/NEVER) seperately for males and females. 
#   Running the group comparison analyses for seeing whether there are statistically significant (both 0.005 and  boferroni (<0.05/71)) differences 
#   between the betas for males and females. 
#3) For those variables that had a p<0.05 for the difference we plot the forest plot.    


# cd /fs/projects/ukbb/jadwiga/PHESANT/SECONDARY
# RDir="/apps/statistics2/"
# ${RDir}R-4.1.1/bin/R

 setwd("/fs/projects/ukbb/jadwiga/PHESANT/SECONDARY")
 DataDir="/fs/projects/ukbb/jadwiga/Phenotypes/DataWrangling/Extracting_ALL/"


sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(tidyr)


#----------------------------------------------------------------------------------------
#READING IN THE DATA 

#Reading in the phenotype data

	P = read.csv("../PHEWAS_2Stage/FDR_final/IntermediateData/ukb_sorted_n343662_61variables.csv")
	dim(P)

		#> dim(P)
		#[1] 343662    415


	DERIVED = read.table("../PHEWAS_2Stage/FDR_final/IntermediateData/DerivedBinaryPhenosForAnalysis.txt",sep="\t",quote="\"",header=TRUE)
#---------------------------------------------------------------------------------------

#Preparing the other 13 binary variables

#List of the 13 variables I need
	Wanted = read.table("../PHEWAS_2Stage/FDR_final/IntermediateData/FromHome/Final_25_BinaryLogisticResults.txt",sep="\t",quote="\"",header=TRUE)
	subset(Wanted,Wanted$resType=="LOGISTIC-BINARY")[,1]
 [1] "1787 - Maternal smoking around birth" : <0 --> NA
 [2] "6149#100 - Free of mouth/teeth dental problems": "x6149_0_0","x6149_0_1","x6149_0_2","x6149_0_3","x6149_0_4","x6149_0_5"
 [3] "6149#6 - Mouth/teeth dental problems: Dentures": "x6149_0_0","x6149_0_1","x6149_0_2","x6149_0_3","x6149_0_4","x6149_0_5"
# [4] "d1488 - Tea drinker"
# [5] "d1498 - Coffee drinker (excluding decaf)"
# [6] "d1498 - Coffee drinker (including decaf)"
# [7] "d1508 - Decaf vs caffeinated coffee"
# [8] "d1508 - Decaf vs no coffee"
# [9] "d1508 - Ground vs no coffee"
#[10] "d1508 - Instant vs no coffee"
#[11] "d1508 - Instant vs other caffeinated coffee"
#[12] "d1508 - Other caffeinated coffee vs none"
#[13] "d20116 - Cessation"
#[14] "d20116 - Initiation"
#[15] "d22506 - Occasional vs daily smoker"
[16] "20003#1140861958 - Simvastatin medication" : "x20003_0_0","x20003_0_1", "x20003_0_2",.."x20003_0_47" /1140861958
[17] "20003#1140867092 - Serenace medication": "x20003_0_0","x20003_0_1", "x20003_0_2",.."x20003_0_47" / 1140867092 (serenace 500micrograms capsule)
[18] "20004#1102 - Leg artery bypass surgery": "x20004_0_0",...,"x20004_0_31" / 1102
[19] "1980 - Worrier / anxious feelings": <0 --> NA
[20] "41200#L978 - Other operations on blood vessel" : "x41200_0_0",...,"x41200_0_52"
[21] "41200#M706 - Radioactive seed implantation into prostate" : "x41200_0_0",...,"x41200_0_52"
[22] "41210#G725 - Anastomosis of ileum to anus (secondary)" : "x41210_0_0", ..., "x41210_0_96"
[23] "41272#G725 - Anastomosis of ileum to anus" : "x41272_0_0",...,"x41272_0_123"
[24] "41272#L601 - Endarterectomy of femoral artery" : "x41272_0_0",...,"x41272_0_123"
[25] "41272#W384 - Prosthetic replacement of hip joint" : : "x41272_0_0",...,"x41272_0_123"


	#Keeping the needed variables
		simplelist<-which(names(P) %in% c("eid","x1787_0_0","x1980_0_0")) #1 10 12
		mouthminmax<-which(names(P) %in% c("x6149_0_0","x6149_0_5")) # 23 28
		medicationminmax <- which(names(P) %in% c("x20003_0_0","x20003_0_47")) # 29 76
		legminmax <- which(names(P) %in% c("x20004_0_0","x20004_0_31")) #77 108
		operations41200minmax <- which(names(P) %in% c("x41200_0_0","x41200_0_52")) # 139 191
		operations41210minmax <- which(names(P) %in% c("x41210_0_0","x41210_0_96")) #192 288
		operations41272minmax <- which(names(P) %in% c("x41272_0_0","x41272_0_123")) #289 412

		W<-P[,c(1,10,12,23:28,29:76,77:108,139:191,192:288,289:412)]
		rm(P)

	#Changing the -1 and -3 values into NAs for maternal smoking and worrier
		table(W$x1787_0_0)
		W$x1787_0_0<-ifelse(W$x1787_0_0<0,NA,W$x1787_0_0)
		W$x1980_0_0<-ifelse(W$x1980_0_0<0,NA,W$x1980_0_0)

#-------------------------------
#Creating the 11 number#number variables from the 6 multiple choice variables
#-------------------------------

	#Creating the 2 mouth variables from x6149_0_0-x6149_0_5
		#------------------------------------------------------
		#a)  x6149_100 CAT-MUL Mouth/teeth dental problems

		#1	Mouth ulcers
		#2	Painful gums
		#3	Bleeding gums
		#4	Loose teeth
		#5	Toothache
		#6	Dentures
		#-7	None of the above
		#-3	Prefer not to answer

		#PHESANT recoded -7=100 and -7 was none of the above so 0-1 variable (1: free from mouth/teeth problems)


		# b) x6149_6 CAT-MUL Mouth/teeth dental problems: Dentures
			#Same coding as above
		#------------------------------------------------------


		table(W$x6149_0_0)
		#    -7     -3      1      2      3      4      5      6
		#208402    743  34997   6389  34325   7492   6376  44616

		table(S$x6149_0_1)
		#   2    3    4    5    6
		#3104 8489 4403 4510 9892


	#a) Creating the none of the above version i.e. should=6149#100
		W$x6149_100<-ifelse(W$x6149_0_0==-3,NA,W$x6149_0_0)
		W$x6149_100<-ifelse(W$x6149_100==-7,1,0)
			table(W$x6149_100)
			#  0      1
			#134195 208402
			#134195+208402 #342597

			#-->Yey, we get the exact same n numbers as phesant outputs for 6149#100



	#b) Creating the dentures version i.e. should = 6149#6

		#Dealing with W$x6149_0_0
			summary(W$x6149_0_0) #NA's 322
			W$x6149_6<-ifelse(W$x6149_0_0==-3,NA,W$x6149_0_0)
			summary(W$x6149_6) #NA's  1065
			W$x6149_6<-ifelse(W$x6149_6==6,1,0) #Keeps Na's as Na's
			summary(W$x6149_6) #NA's 1065
		
		#Dealing with W$x6149_0_1 to W$x6149_0_5
		##First replacing missing values so we don't loose most of our data when we go through these variables
			#test<-W[1:10,4:9]
			#test[,2:3][is.na(test[,2:3])]<- -3
			W[,5:9][is.na(W[,5:9])]<- -3
			W$x6149_6<-ifelse(W$x6149_0_1==6 | W$x6149_0_2==6| W$x6149_0_3==6| W$x6149_0_4==6| W$x6149_0_5==6,1,W$x6149_6)	
			table(W$x6149_6)
				#     0      1
				#284781  57816
				#PHESANT n: 284781/57816(342597) (Using command: Wanted[3,])

		#Finally we can delete the columns that are no longer needed
		W<-W[,-c(4:9)]



	#Creating the 2 medication variables from "x20003_0_0","x20003_0_1", "x20003_0_2",.."x20003_0_47"
		#------------------------------------------------------
		[16] "20003#1140861958 - Simvastatin medication"  /1140861958
		[17] "20003#1140867092 - Serenace medication" / 1140867092 (serenace 500micrograms capsule)
Phenotype         resType  Cat3_Title
16 20003#1140861958 - Simvastatin medication LOGISTIC-BINARY Medications
17    20003#1140867092 - Serenace medication LOGISTIC-BINARY Medications
        beta_A      lower_A    upper_A        se_A     pvalue_A
16  0.01729144  0.006387652  0.0282134 0.005563156 1.898246e-03
17 -0.98144853 -1.467981301 -0.4753969 0.248231007 9.496879e-05
                    n_A FDRsig_A     beta_E    lower_E    upper_E        se_E
16 304495/39124(343619)        0 0.04034117 0.02334123 0.05738325 0.008673439
17    343609/10(343619)        1         NA         NA         NA          NA
       pvalue_E                 n_E FDRsig_E      beta_N     lower_N    upper_N
16 3.395111e-06 93474/16863(110337)        1 0.006245367 -0.01235187 0.02489773
17           NA                <NA>       NA          NA          NA         NA
          se_N  pvalue_N                  n_N FDRsig_N          varName
16 0.009488385 0.5110301 122892/12976(135868)        0 20003#1140861958
17          NA        NA                 <NA>       NA 20003#1140867092
                                                 description
16                    Treatment/medication code: simvastatin
17 Treatment/medication code: serenace 500micrograms capsule
                ShortName FDRsigSum pvalue_EvsN EverNeversig
16 Simvastatin medication         1 0.007994972            0
17    Serenace medication         1          NA           NA

		#--> so n for these variables in all: 
		#simvastatin: 304 495/39124(343619)
		#serenace: 343609/10(343619)

#Perhaps because have to also have zGS and covariate data available!

		#a) Creating simvastatin variable: 1140861958

			#Dealing with the x20003_0_0 variable
#			summary(W$x20003_0_0) #NA's 93225
#			#Create new variable which indicates if simvastatin was used(1) or not(0) keeps NA's as NA's)
#			W$x20003_1140861958<-ifelse(W$x20003_0_0==1140861958,1,0) #Keeps NA's#
#			summary(W$x20003_1140861958) #NA's 93225
#
#			#Dealing with the rest of the variables "x20003_0_1", "x20003_0_2",.."x20003_0_47"
#			#Create the new variable based on the condition
#			#Using the apply function to iterate over the rows (1 for rows) of the specified columns (5:51).
#
#
#			W$temp <- apply(W[,5:51], 1, function(row) {
#			  # Check for value 1140861958 ignoring NA values
#				ifelse(any(row == 1140861958, na.rm = TRUE), 1, 0)
#			})
#
#			#Now getting the ones and NA's from the initial version
#			W$x20003_1140861958<-ifelse(W$temp==1,1,W$x20003_1140861958)
#
#			summary(W$x20003_1140861958) #NA's 93225
#			table(W$x20003_1140861958)
#			#       0      1
			#211313  39124

			#NOTE: when I simply replaced all non 1 values with zeros I got
			#     0      1
			#304538  39124
			#--> which is closer to what PHESANT gives. especially as the n may drop as have to also have zGS and covariate data available!
			#So perhaps NA is not really missing but means that they don't use the medication and now 0 would only be a selected set of 
			#individuals using some other medication but not this specific medication.
			#-->THUS I'll use the version where we don't have any NA's:

			 W$x20003_1140861958 <- apply(W[,4:51], 1, function(row) {
			  # Check for value 1140861958 ignoring NA values
				ifelse(any(row == 1140861958, na.rm = TRUE), 1, 0)
			})
				table(W$x20003_1140861958)
				#
				#    0      1
				#304538  39124



		#b) Creating serenace variable: 1140867092

			W$x20003_1140867092 <- apply(W[,4:51], 1, function(row) {
			  # Check for value 1140867092 ignoring NA values
				ifelse(any(row == 1140867092, na.rm = TRUE), 1, 0)
			})
	
			table(W$x20003_1140867092)
			#    0      1
			#343652     10



		#Finally we can delete the columns that are no longer needed
			W<-W[,-c(4:51)]


#-------------------------------
#Creating the 20004#1102 - Leg artery bypass surgery": "x20004_0_0",...,"x20004_0_31" / 1102
#-------------------------------
                               Phenotype         resType Cat3_Title    beta_A
18 20004#1102 - Leg artery bypass surgery LOGISTIC-BINARY Operations 0.2517398
     lower_A   upper_A       se_A     pvalue_A                n_A FDRsig_A
18 0.1051744 0.4032366 0.07477827 0.0009274439 343415/204(343619)        0
      beta_E   lower_E   upper_E       se_E     pvalue_E                n_E
18 0.3434958 0.1756309 0.5178101 0.08564536 8.270835e-05 110173/164(110337)
   FDRsig_E    beta_N    lower_N   upper_N      se_N  pvalue_N
18        1 0.2087017 -0.3027355 0.7856175 0.2609373 0.4505357
                 n_N FDRsig_N    varName
18 135853/15(135868)        0 20004#1102
                                        description                 ShortName
18 Operation code: fem-pop bypass/leg artery bypass Leg artery bypass surgery
   FDRsigSum pvalue_EvsN EverNeversig
18         1   0.6235574            0

#-->N: 343415/204(343619)


			W$x20004_1102 <- apply(W[,4:35], 1, function(row) {
			  # Check for value 1102 ignoring NA values
				ifelse(any(row == 1102, na.rm = TRUE), 1, 0)
			})
	
			table(W$x20004_1102)
			#    0      1
			#343458    204


			#Finally we can delete the columns that are no longer needed
				W<-W[,-c(4:35)]


#-------------------------------
#Creating
#[20] "41200#L978 - Other operations on blood vessel" : "x41200_0_0",...,"x41200_0_52"
#[21] "41200#M706 - Radioactive seed implantation into prostate" : "x41200_0_0",...,"x41200_0_52"
#-------------------------------
   
Wanted[20:21,]
                                                  Phenotype         resType
20            41200#L978 - Other operations on blood vessel LOGISTIC-BINARY
21 41200#M706 - Radioactive seed implantation into prostate LOGISTIC-BINARY
           Cat3_Title     beta_A    lower_A    upper_A       se_A     pvalue_A
20 Summary Operations  0.9439710  0.5013023  1.4307601 0.22585136 6.740306e-05
21 Summary Operations -0.2105219 -0.3114523 -0.1074325 0.05149512 5.216681e-05
                  n_A FDRsig_A     beta_E    lower_E     upper_E      se_E
20  343629/33(343662)        1         NA         NA          NA        NA
21 343339/323(343662)        1 -0.2013843 -0.3815748 -0.01422192 0.0919339
     pvalue_E                n_E FDRsig_E     beta_N    lower_N    upper_N
20         NA               <NA>       NA  1.0800239  0.4307346  1.8263784
21 0.03151785 110248/100(110348)        0 -0.3333394 -0.5050257 -0.1557943
         se_N     pvalue_N                n_N FDRsig_N    varName
20 0.33127005 0.0023728143  135874/16(135890)        0 41200#L978
21 0.08759502 0.0001810503 135787/103(135890)        0 41200#M706
                                                                                 description
20 Operative procedures - main OPCS4: L97.8 Other specified other operations on blood vessel
21      Operative procedures - main OPCS4: M70.6 Radioactive seed implantation into prostate
                                     ShortName FDRsigSum pvalue_EvsN
20            Other operations on blood vessel         1          NA
21 Radioactive seed implantation into prostate         1   0.2987331
   EverNeversig
20           NA
21            0


#n:
#343629/33(343662) 
#343339/323(343662)


			W$x41200_L978 <- apply(W[,4:56], 1, function(row) {
			  # Check for value L978 ignoring NA values
				ifelse(any(row == "L978", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41200_L978)
			#    0      1
			#343629     33


			W$x41200_M706 <- apply(W[,4:56], 1, function(row) {
			  # Check for value M706 ignoring NA values
				ifelse(any(row == "M706", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41200_M706)
			#     0      1
			#343339    323


			#Finally we can delete the columns that are no longer needed
				W<-W[,-c(4:56)]


#-------------------------------
#Creating
[22] "41210#G725 - Anastomosis of ileum to anus (secondary)" : "x41210_0_0", ..., "x41210_0_96"
#-------------------------------
Wanted[22,]
                                             Phenotype         resType
22 41210#G725 - Anastomosis of ileum to anus (secondary) LOGISTIC-BINARY
           Cat3_Title     beta_A    lower_A    upper_A      se_A    pvalue_A
22 Summary Operations -0.4562645 -0.7913477 -0.1005442 0.1709608 0.009413834
                 n_A FDRsig_A     beta_E   lower_E   upper_E     se_E
22 343637/25(343662)        0 -0.9756133 -1.434563 -0.501467 0.234158
       pvalue_E               n_E FDRsig_E beta_N lower_N upper_N se_N pvalue_N
22 3.622128e-05 110337/11(110348)        1     NA      NA      NA   NA       NA
    n_N FDRsig_N    varName
22 <NA>       NA 41210#G725
                                                                                            description
22 Operative procedures - secondary OPCS4: G72.5 Anastomosis of ileum to anus and creation of pouch HFQ
                                  ShortName FDRsigSum pvalue_EvsN EverNeversig
22 Anastomosis of ileum to anus (secondary)         1          NA           NA

#n: 343637/25(343662)

			W$x41210_G725 <- apply(W[,4:100], 1, function(row) {
			  # Check for value G725 ignoring NA values
				ifelse(any(row == "G725", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41210_G725)
			#     0      1
			#343637     25

			#Finally we can delete the columns that are no longer needed
				W<-W[,-c(4:100)]

#Second time I tried this I ran out of memory so running the above in chunks:
# Initialize an empty vector to store results
results <- vector("numeric", nrow(W))

# Define the chunk size
chunk_size <- 1000

# Function to process each chunk
process_chunk <- function(chunk) {
  apply(chunk, 1, function(row) {
    ifelse(any(row == "G725", na.rm = TRUE), 1, 0)
  })
}

# Process data in chunks
for (i in seq(1, nrow(W), by = chunk_size)) {
  # Define the start and end of the chunk
  start <- i
  end <- min(i + chunk_size - 1, nrow(W))
  
  # Process the chunk
  results[start:end] <- process_chunk(W[start:end, 4:100])
}

# Assign the results to a new column in the data frame
W$x41210_G725 <- results

# Print the current names
names(W)

table(W$x41210_G725)
W<-W[,-c(4:100)]

#-------------------------------
#Creating
[23] "41272#G725 - Anastomosis of ileum to anus" : "x41272_0_0",...,"x41272_0_123"
[24] "41272#L601 - Endarterectomy of femoral artery" : "x41272_0_0",...,"x41272_0_123"
[25] "41272#W384 - Prosthetic replacement of hip joint" : : "x41272_0_0",...,"x41272_0_123"
#-------------------------------
Wanted[23:25,]
                                      Phenotype         resType
23        41272#G725 - Anastomosis of ileum to anus LOGISTIC-BINARY
24    41272#L601 - Endarterectomy of femoral artery LOGISTIC-BINARY
25 41272#W384 - Prosthetic replacement of hip joint LOGISTIC-BINARY
           Cat3_Title     beta_A    lower_A     upper_A       se_A     pvalue_A
23 Summary Operations -0.3425859 -0.6274274 -0.04167335 0.14532728 2.160587e-02
24 Summary Operations  0.2469990  0.1173915  0.38045534 0.06612631 2.320100e-04
25 Summary Operations -0.2603203 -0.3874780 -0.12984531 0.06487642 7.426071e-05
                  n_A FDRsig_A     beta_E    lower_E    upper_E       se_E
23  343625/37(343662)        0 -0.8351388 -1.2203557 -0.4332736 0.19653925
24 343400/262(343662)        0  0.2898451  0.1474081  0.4369074 0.07267195
25 343465/197(343662)        1 -0.3718848 -0.5805759 -0.1548475 0.10647505
       pvalue_E                n_E FDRsig_E      beta_N    lower_N    upper_N
23 2.901625e-05  110332/16(110348)        1          NA         NA         NA
24 8.650883e-05 110126/222(110348)        1 -0.06270076 -0.5027494 0.42524167
25 6.080682e-04  110281/67(110348)        0 -0.19532685 -0.4014815 0.01989286
        se_N   pvalue_N               n_N FDRsig_N    varName
23        NA         NA              <NA>       NA 41272#G725
24 0.2245146 0.79045124 135873/17(135890)        0 41272#L601
25 0.1051809 0.06896519 135813/77(135890)        0 41272#W384
                                                                                                                   description
23                                  Operative procedures - OPCS4: G72.5 Anastomosis of ileum to anus and creation of pouch HFQ
24                     Operative procedures - OPCS4: L60.1 Endarterectomy of femoral artery and patch repair of femoral artery
25 Operative procedures - OPCS4: W38.4 Revision of one component of total prosthetic replacement of hip joint not using cement
                             ShortName FDRsigSum pvalue_EvsN EverNeversig
23        Anastomosis of ileum to anus         1          NA           NA
24    Endarterectomy of femoral artery         1   0.1351899            0
25 Prosthetic replacement of hip joint         1   0.2381283            0


#n:
#343625/37(343662)
#343400/262(343662)
#343465/197(343662)

			W$x41272_G725 <- apply(W[,4:127], 1, function(row) {
			  # Check for value G725 ignoring NA values
				ifelse(any(row == "G725", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41272_G725)
			#     0      1
			#343625     37
# Initialize an empty vector to store results
results <- vector("numeric", nrow(W))

# Define the chunk size
chunk_size <- 1000

# Function to process each chunk
process_chunk <- function(chunk) {
  apply(chunk, 1, function(row) {
    ifelse(any(row == "G725", na.rm = TRUE), 1, 0)
  })
}

# Process data in chunks
for (i in seq(1, nrow(W), by = chunk_size)) {
  # Define the start and end of the chunk
  start <- i
  end <- min(i + chunk_size - 1, nrow(W))
  
  # Process the chunk
  results[start:end] <- process_chunk(W[start:end, 4:127])
}

# Assign the results to a new column in the data frame
W$x41272_G725 <- results

table(W$x41272_G725)

			W$x41272_L601 <- apply(W[,4:127], 1, function(row) {
			  # Check for value L601 ignoring NA values
				ifelse(any(row == "L601", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41272_L601)
			#     0      1
			#343400    262
# Initialize an empty vector to store results
results <- vector("numeric", nrow(W))

# Define the chunk size
chunk_size <- 1000

# Function to process each chunk
process_chunk <- function(chunk) {
  apply(chunk, 1, function(row) {
    ifelse(any(row == "L601", na.rm = TRUE), 1, 0)
  })
}

# Process data in chunks
for (i in seq(1, nrow(W), by = chunk_size)) {
  # Define the start and end of the chunk
  start <- i
  end <- min(i + chunk_size - 1, nrow(W))
  
  # Process the chunk
  results[start:end] <- process_chunk(W[start:end, 4:127])
}

# Assign the results to a new column in the data frame
W$x41272_L601 <- results

table(W$x41272_L601)


			W$x41272_W384 <- apply(W[,4:127], 1, function(row) {
			  # Check for value W384 ignoring NA values
				ifelse(any(row == "W384", na.rm = TRUE), 1, 0)
			})
	
			table(W$x41272_W384)
			#     0      1
			#343465    197

# Initialize an empty vector to store results
results <- vector("numeric", nrow(W))

# Define the chunk size
chunk_size <- 1000

# Function to process each chunk
process_chunk <- function(chunk) {
  apply(chunk, 1, function(row) {
    ifelse(any(row == "W384", na.rm = TRUE), 1, 0)
  })
}

# Process data in chunks
for (i in seq(1, nrow(W), by = chunk_size)) {
  # Define the start and end of the chunk
  start <- i
  end <- min(i + chunk_size - 1, nrow(W))
  
  # Process the chunk
  results[start:end] <- process_chunk(W[start:end, 4:127])
}

# Assign the results to a new column in the data frame
W$x41272_W384 <- results

table(W$x41272_W384)


			#Finally we can delete the columns that are no longer needed
				W<-W[,-c(4:127)]

#Merging with derived variables

DERIVED = read.table("../PHEWAS_2Stage/FDR_final/IntermediateData/DerivedBinaryPhenosForAnalysis.txt",sep="\t",quote="\"",header=TRUE)

Analysis<-join(W,DERIVED, by="eid", type="left") #[1] 343662     26


#write.table(Analysis,"./IntermediateData/SexDiff_BINARY_forAnalysis.txt",sep="\t", quote=F, row.names=F)



###########################################################################################################################################
#						Logistic regression
###########################################################################################################################################


 
#I created the function testBinary which is an adaptation of the PHESANT one: 
#https://github.com/MRCIEU/PHESANT-MR-pheWAS-smoking/blob/master/2-PHESANT/PHESANT-from-saved/testBinary.r


a) Reading in Phenodata and confounder data

	P<-read.table("./IntermediateData/SexDiff_BINARY_forAnalysis.txt", sep="\t",header=TRUE)
	dim(P)	 # 343662     26

	#we read in the confounder file
		
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=TRUE, header=TRUE)
		names(C)
		str(C)
		dim(C)

#b) Reading in trait data for All/Ever/Never

	#Reading in our GRS data (for the All dataset)
		TA = read.csv("../TRAIT/zGRS10SNPs_topconfig.csv", as.is=TRUE, header=TRUE)
		T <- join(TA,C[,c("eid","x31_0_0")], by = "eid", type ="left")
		T_FA = subset(T,T$x31_0_0==0)
		T_MA = subset(T,T$x31_0_0==1)


	#Reading in our GRS data (for the Ever dataset)
		TE = read.csv("../TRAIT/Ever_zGRS10SNPs_110348.csv", as.is=TRUE, header=TRUE)
		TE <- join(TE,C[,c("eid","x31_0_0")], by = "eid", type ="left")
		T_FE = subset(TE,TE$x31_0_0==0)
		T_ME = subset(TE,TE$x31_0_0==1)


	#Reading in our GRS data (for the Never dataset)
		TN = read.csv("../TRAIT/Never_zGRS10SNPs_135890.csv", as.is=TRUE, header=TRUE)
		TN <- join(TN,C[,c("eid","x31_0_0")], by = "eid", type ="left")
		T_FN = subset(TN,TN$x31_0_0==0)
		T_MN = subset(TN,TN$x31_0_0==1)



#Creating a dataframe with VarnameComplicated, Phenotype, Cat3_Title
		Wanted = read.table("../PHEWAS_2Stage/FDR_final/IntermediateData/FromHome/Final_25_BinaryLogisticResults.txt",sep="\t",quote="\"",header=TRUE)
		subset(Wanted,Wanted$resType=="LOGISTIC-BINARY")[,1]

		F2<-subset(Wanted,Wanted$resType=="LOGISTIC-BINARY")[,c(1,3)]
		names(P[,-1]) #excluding "eid"
		F2
		#By hand based on the above, creating the variable VarnameComplicated
		F2$VarnameComplicated<-c("x1787_0_0","x6149_100","x6149_6","d1488_TeaDrinker","d1498_CoffeeDrinker_excldecaf","d1498_CoffeeDrinker_incldecaf",
			"d1508_DecafvsCaf","d1508_Decafvsnone","d1508_Groundvsnone","d1508_Instvsnone","d1508_InstvsOtherCaf","d1508_Othervsnone","d20116_Cessation",
			"d20116_Initiation","d22506_OccasionalvsDaily","x20003_1140861958","x20003_1140867092","x20004_1102","x1980_0_0","x41200_L978",
			"x41200_M706","x41210_G725","x41272_G725","x41272_L601","x41272_W384")
		F2[,c(1,3)]
	
#-----------------------function: testBinary----------------------------------------------

#Note: Remember that a function can only return one object! 
#That is why I'm returning the data for the forest plot and for the results table in the same dataframe and then dividing them into two after using the function.

testBinary<-function(dataset, Beta, Pvalue, Se, N, Lower, Upper, Phenodata, Traitdata, Covariatedata){
	
	OUT<-data.frame()
	
	for (i in colnames(Phenodata[,-1])) {

		workName<-i
		resType<-"LOGISTIC-BINARY"
		data=dataset

		#Creating a merged dataset (exposure+outcome+covariates)
			p_i<-Phenodata[,c("eid", i)]
			D <- join(Traitdata,p_i, by = "eid", type ="left")
			D <- join(D,Covariatedata, by = "eid", type ="left")
			names(D)
			dim(D)

		#Preparing the data for our analysis
			pheno = D[,i]
			phenoFactor = factor(pheno)
			exp = D[,"zGRS"]
			confNames = colnames(Covariatedata)
        		confNames = confNames[-which(confNames=="eid")]
			confs = D[,confNames]

		#Getting the n
			facLevels = levels(phenoFactor)
			num0 = length(which(phenoFactor==facLevels[1]))
			num1 = length(which(phenoFactor==facLevels[2]))
			numNotNA = length(which(!is.na(phenoFactor)))
			n<-paste(num0,"/",num1,"(",numNotNA,")",sep="")

		#Running the logistic regression
			mylogit <- glm(phenoFactor ~ exp + ., data=confs, family="binomial")

		#Getting the results
			sumx = summary(mylogit)
		
			beta = sumx$coefficients["exp","Estimate"]
                	pvalue = sumx$coefficients['exp','Pr(>|z|)']
			se = sumx$coefficients["exp","Std. Error"]
                
                	cis = confint(mylogit, "exp", level=0.95)
                	lower = cis["2.5 %"]
                	upper = cis["97.5 %"]

		OUT[which(colnames(Phenodata[,-1])==i),1:12]<-c(workName, resType, beta, se, pvalue, data, beta, lower, upper, se, pvalue, n)
		print(OUT)
		}

	names(OUT)<-c("VarnameComplicated", "resType", "beta", "se", "pvalue", "data",Beta, Lower, Upper, Se, Pvalue, N)
	return(OUT) 
				
}


#-----------------------------------------------------------------------------------------
#----------------------------
#For All
#----------------------------
	#Females
		O<-testBinary(dataset="Females All", Beta="beta_FA", Pvalue="pvalue_FA", Se="se_FA", N="n_FA", Lower="lower_FA", Upper="upper_FA", Phenodata=P, Traitdata=T_FA, Covariatedata=C[,-2])
		FD_fa<-O[,c(1,2,3:6)]
		RD_fa<-O[,c(1,2,7:12)]
		FD_fa
		RD_fa


	#Males
		O<-testBinary(dataset="Males All", Beta="beta_MA", Pvalue="pvalue_MA", Se="se_MA", N="n_MA", Lower="lower_MA", Upper="upper_MA", Phenodata=P, Traitdata=T_MA, Covariatedata=C[,-2])
		FD_ma<-O[,c(1,2,3:6)]
		RD_ma<-O[,c(1,2,7:12)]
		FD_ma
		RD_ma

#Merging the results into Results_ALL_Logistic
		names(RD_ma)
		#We join the results (we delete resType from male versions not to have that multiple times)
			Results<-join(RD_fa, RD_ma[,-2], by="VarnameComplicated", type="left")
		#We get the Phenotype name
			Results<-join(Results, F2, by="VarnameComplicated", type="left")
		#Ensuring the columns we want numeric are numeric (as currently they are in character format)
			#Using lapply to apply the as.numeric function to each of the selected columns
			Results[, c(3:7,9:13)] <- lapply(Results[,c(3:7,9:13)], as.numeric)
			Results

#######################################################################################################################################
# running the female vs male analysis and checking bonferroni significance
#######################################################################################################################################

		bf_threshold <- 0.05/71
		Results$x = (Results$beta_FA-Results$beta_MA)^2 / (Results$se_FA^2+Results$se_MA^2)
      		Results$pvalue_FAvsMA = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) #25 x 18

		Results$FemaleMaleALLbfsig <- ifelse(Results$pvalue_FAvsMA < bf_threshold,1,0)
		dim(subset(Results, Results$FemaleMaleALLbfsig==1)) #0 x 19 
		dim(subset(Results, Results$pvalue_FAvsMA<0.05)) #1 x 19 (d20116 - Cessation)
		
#######################################################################################################################################
			
	#Ordering the Results by p-value and saving them
		RD_ALL<- Results[order(Results$pvalue_FAvsMA),]
	#Selecting and Reordering the columns
		names(RD_ALL)
		[1] "VarnameComplicated" "resType"            "beta_FA"
 [4] "lower_FA"           "upper_FA"           "se_FA"
 [7] "pvalue_FA"          "n_FA"               "beta_MA"
[10] "lower_MA"           "upper_MA"           "se_MA"
[13] "pvalue_MA"          "n_MA"               "Pheno"
[16] "Cat3_Title"         "x"                  "pvalue_FAvsMA"
[19] "FemaleMaleALLbfsig"
		RD_ALL<- RD_ALL[,c("Phenotype","resType","Cat3_Title","pvalue_FAvsMA","FemaleMaleALLbfsig","beta_FA", "lower_FA",
           			"upper_FA","se_FA","pvalue_FA","n_FA","beta_MA","lower_MA","upper_MA","se_MA","pvalue_MA","n_MA")]

		#Will delete the row: 41200#M706 - Radioactive seed implantation into prostate (cases=0 for females)
		RD_ALL[,1]
		RD_ALL<-RD_ALL[-25,]
	write.table(RD_ALL,"./IntermediateData/SexDiff_Results_ALL_LOGISTIC.txt",sep="\t", quote=F, row.names=F) 





#----------------------------
#For Ever
#----------------------------
#Note: have to exclude Serenace(seventh column) and initiation (17th column)

#Females	
	O<-testBinary(dataset="Females Ever", Beta="beta_FE", Pvalue="pvalue_FE", Se="se_FE", N="n_FE", Lower="lower_FE", Upper="upper_FE", Phenodata=P[,-c(7,17)], Traitdata=T_FE, Covariatedata=C[,-2])
	FD_fe<-O[,c(1,2,3:6)]
	RD_fe<-O[,c(1,2,7:12)]
	FD_fe
	RD_fe

#Males	
	O<-testBinary(dataset="Males Ever", Beta="beta_ME", Pvalue="pvalue_ME", Se="se_ME", N="n_ME", Lower="lower_ME", Upper="upper_ME", Phenodata=P[,-c(7,17)], Traitdata=T_ME, Covariatedata=C[,-2])
	FD_me<-O[,c(1,2,3:6)]
	RD_me<-O[,c(1,2,7:12)]
	FD_me
	RD_me

#Merging the results into Results_EVER_Logistic
		names(RD_me)
		#We join the results (we delete resType from male versions not to have that multiple times)
			Results<-join(RD_fe, RD_me[,-2], by="VarnameComplicated", type="left")
		#We get the Phenotype name
			Results<-join(Results, F2, by="VarnameComplicated", type="left")
		#Ensuring the columns we want numeric are numeric (as currently they are in character format)
			#Using lapply to apply the as.numeric function to each of the selected columns
			Results[, c(3:7,9:13)] <- lapply(Results[,c(3:7,9:13)], as.numeric)
			Results

#######################################################################################################################################
# running the female vs male analysis and checking bonferroni significance
#######################################################################################################################################

		bf_threshold <- 0.05/71
		Results$x = (Results$beta_FE-Results$beta_ME)^2 / (Results$se_FE^2+Results$se_ME^2)
      		Results$pvalue_FEvsME = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) #24 x 17

		Results$FemaleMaleEVERbfsig <- ifelse(Results$pvalue_FEvsME < bf_threshold,1,0)
		dim(subset(Results, Results$FemaleMaleEVERbfsig==1)) #0 x 19 
		dim(subset(Results, Results$pvalue_FEvsME<0.05)) #0 x 19 
		
#######################################################################################################################################
			

#Ordering the Results by p-value and saving them
		RD_EVER<- Results[order(Results$pvalue_FEvsME),]
	#Selecting and Reordering the columns
		RD_EVER<- RD_EVER[,c("Phenotype","resType","Cat3_Title","pvalue_FEvsME","FemaleMaleEVERbfsig","beta_FE", "lower_FE",
           			"upper_FE","se_FE","pvalue_FE","n_FE","beta_ME","lower_ME","upper_ME","se_ME","pvalue_ME","n_ME")]
		dim(RD_EVER) #24 17
	
	#Noticed I shouldn't have included other operations on blood vessel (was not analysed by PHESANT due to too little cases) or radioactive seed implantation to prostate (male specific variable)
		print(RD_EVER$Phenotype)
		RD_EVER<-RD_EVER[-c(6,23),]	

	write.table(RD_EVER,"./IntermediateData/SexDiff_Results_EVER_LOGISTIC.txt",sep="\t", quote=F, row.names=F) 








#----------------------------
#For Never
#----------------------------
#Note: have to exclude: cessation (16), initiation(17), occasionalvsdaily (24), serenace(7), anastomosis of ileum to anus (secondary)(11),anastomosis of ileum to anus(12) 
names(P)

#Females
	O<-testBinary(dataset="Females Never", Beta="beta_FN", Pvalue="pvalue_FN", Se="se_FN", N="n_FN", Lower="lower_FN", Upper="upper_FN", Phenodata=P[,-c(7,11,12,16,17,24)], Traitdata=T_FN, Covariatedata=C[,-2])
	FD_fn<-O[,c(1,2,3:6)]
	RD_fn<-O[,c(1,2,7:12)]
	FD_fn
	RD_fn

#Males
	O<-testBinary(dataset="Males Never", Beta="beta_MN", Pvalue="pvalue_MN", Se="se_MN", N="n_MN", Lower="lower_MN", Upper="upper_MN", Phenodata=P[,-c(7,11,12,16,17,24)], Traitdata=T_MN, Covariatedata=C[,-2])
	FD_mn<-O[,c(1,2,3:6)]
	RD_mn<-O[,c(1,2,7:12)]
	FD_mn
	RD_mn


#Merging the results into Results_NEVER_Linear
		names(RD_mn)
		#We join the results (we delete resType from the female and male versions not to have that multiple times)
			Results<-join(RD_fn, RD_mn[,-2], by="VarnameComplicated", type="left")
		#We get the Phenotype name
			Results<-join(Results, F2, by="VarnameComplicated", type="left")
		#Ensuring the columns we want numeric are numeric (as currently they are in character format)
			#Using lapply to apply the as.numeric function to each of the selected columns
			str(Results)
			Results[, c(3:7,9:13)] <- lapply(Results[,c(3:7,9:13)], as.numeric)
			Results
		#We notice at this point that for variable Radioactive seed implantation into prostate there are zero cases for females. 
		#As it is only a male related phenotype we delete it
			Results<-Results[-8,] 

#######################################################################################################################################
# running the female vs male analysis and checking bonferroni significance
#######################################################################################################################################

		bf_threshold <- 0.05/71
		Results$x = (Results$beta_FN-Results$beta_MN)^2 / (Results$se_FN^2+Results$se_MN^2)
      		Results$pvalue_FNvsMN = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) 

		Results$FemaleMaleNEVERbfsig <- ifelse(Results$pvalue_FNvsMN < bf_threshold,1,0)
		dim(subset(Results, Results$FemaleMaleNEVERbfsig==1)) #0 x 19 
		dim(subset(Results, Results$pvalue_FNvsMN<0.05)) #1 x 19 (d1508 - Decaf vs caffeinated coffee)
		
#######################################################################################################################################
		


#Ordering the Results by p-value and saving them
		RD_NEVER<- Results[order(Results$pvalue_FNvsMN),]
	#Selecting and Reordering the columns
		RD_NEVER<- RD_NEVER[,c("Phenotype","resType","Cat3_Title","pvalue_FNvsMN","FemaleMaleNEVERbfsig","beta_FN", "lower_FN",
           			"upper_FN","se_FN","pvalue_FN","n_FN","beta_MN","lower_MN","upper_MN","se_MN","pvalue_MN","n_MN")]
		dim(RD_NEVER) #18 17
	write.table(RD_NEVER,"./IntermediateData/SexDiff_Results_NEVER_LOGISTIC.txt",sep="\t", quote=F, row.names=F) 




#----------------------------------------
	
#-----------------------------
#FOREST PLOT DATA
#------------------------------

#make a list of the phenotypes that were 0.05 sig in any of the above three results. Then create the forest plot data for those variables.

#Note: 
#have to exclude: 
#Radioactive seed implantation into prostate (zero cases for females): x41200_M706. 
#other operations on blood vessel  for Never:  x41200_L978 
	#Deleting Radioactive seed implantation into prostate (zero cases for females): x41200_M706
		FD_fa <- FD_fa[-which(FD_fa$VarnameComplicated=="x41200_M706"),]
		FD_ma <- FD_ma[-which(FD_ma$VarnameComplicated=="x41200_M706"),]
		FD_fe <- FD_fe[-which(FD_fe$VarnameComplicated=="x41200_M706"),]
		FD_me <- FD_me[-which(FD_me$VarnameComplicated=="x41200_M706"),]
	#For the never sets also deleting other operations on blood vessel:  x41200_L978 
		FD_fn <- FD_fn[-c(which(FD_fn$VarnameComplicated=="x41200_M706"),which(FD_fn$VarnameComplicated=="x41200_L978")),]
		FD_mn <- FD_mn[-c(which(FD_mn$VarnameComplicated=="x41200_M706"),which(FD_mn$VarnameComplicated=="x41200_L978")),]
		

	#Concatenating the forest plot data
		Forest<-rbind(FD_fa,FD_ma, FD_fe,FD_me, FD_fn,FD_mn)
	#adding pheno and cat3, and adding variable NotBFsig
		F<-join(Forest,F2, by="VarnameComplicated", type="left")
		F$NotBFsig<-ifelse(F$pvalue < 0.05/71,0,1)
	#Reordering rows by phenotype
		F<- F[order(F$Cat3_Title,F$Phenotype),]
	#Creating the list of variables we want to plot
		listALL <- as.vector(unique(subset(RD_ALL, RD_ALL$pvalue_FAvsMA < 0.05)$Pheno))
		listEVER <- as.vector(unique(subset(RD_EVER, RD_EVER$pvalue_FEvsME < 0.05)$Pheno))
		listNEVER <- as.vector(unique(subset(RD_NEVER, RD_NEVER$pvalue_FNvsMN < 0.05)$Pheno))
		list<-unique(c(listALL,listEVER,listNEVER)) #2 different variables
	#Selecting the rows we want to keep
		Forest<-subset(F,F$Pheno %in% list)

	#Selecting and Reordering the columns
		Forest<- Forest[,c("Pheno","resType","Cat3_Title","beta","se","pvalue", "data","NotBFsig")]

	#Saving the data to be plotted
		rm(F)
		write.table(Forest,"./IntermediateData/SexDiff_ForPlottingForest_LOGISTIC.txt",sep="\t", quote=F, row.names=F) 





























