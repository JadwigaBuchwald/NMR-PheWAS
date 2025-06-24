# NMR PheWAS
 Scripts accompanying https://doi.org/10.1101/2023.12.22.23300430 


#I have tried to add a numbering to the scripts so that the order of the scripts is self explanatory. 
#However, some of my original scripts did not have this so within some of the scripts the scripts have been referred to without the numbers. 
#Also, if I remember correctly in some cases the scripts would not run if they began with a number... or then the name of the job was not allowed to start with a number when using grun?

#Note, GRS is the same as GS in the manuscript.

#--------------Data Wrangling----------------------

#a_DATA_WRANGLING/FINEMAP
	A) Downloading finemap: A_Downloading_finemap_v1.4.sh
	B) Obtaining a list of the rsids that were present in our FRYFS Finemapping analysis (2.5Mb flanks on both sides of the top SNP): B_FRYFS_zRSIDs.sh

	1) Getting the rsids (.gen and .stats) found in the fryfs z files for our UKBB sample: 1_gen_zrsids_chr19_WB20210809.sh
	2) Obtaining QC passed ukbb SNPs for region to be FINEMAPped: 2_UKBB_QC.sh
	3) Creating a vector "WhichAvailable.txt" which we then use to obtain the wanted subset of the yfsfr Z and LD files (for finemapping) using bash: 3_WhichAvailable.R
	4) Creating z and ld files for FINEMAP: 4_SubsetForFINEMAP.sh & trans.sh
	5) Plotting the available/filtered SNPs: 5_Plots_AvailablevsFiltered.R
	6) Running FINEMAP: 6_Finemap.sh & master
	

#a_DATA_WRANGLING/GRS/WhiteBritish
	1a) Extract the wanted top config SNPs (nine of them): 1a_extractgen_chr19topconfig.sh & rsidlist.txt
	1b) Getting chr4 SNP and top SNP genotype data from the bgen files using qctool v2: 1b_GenFromBgen_chr4andTopSNP.sh & GRS_chr4SNP_rsid.txt & rs56113850.txt 
	2) Converting gen files to dosage format: 2_gentodosage.R & GenotypeFileConversion.R
	3) Joining the GRS chr4 and chr19 dosage datasets: 3_JoinDosage.sh
	4a) Extract the summary statistics for the wanted top config SNPs (.stats): 4a_extractsnpstats.sh & rsidlist.txt
	4b) Getting snpstats for our chr4 SNP and the top SNP, QC:d white british non relatives (N=343662) using qctool v2: 4a_extractsnpstats.sh 
	5) Adding chr 4 stats and chr 19 stats together for the SNPs to be included in the GRS: 5_Combining_GRS10SNPs_stats.sh
	6) Obtaining the top configuration joint betas from the finemap results: 6_topconfig_JointBetas.sh 
	7) Creating the GRS: 7_GRS.R & chr4SNP_details.txt & topconfig_JointBetas.txt



#--------------ANALYSES----------------------

	#b_ANALYSES/GS_SanityCheck
		1) GS and TopSNP GS_TopSNP_README.txt: can be found c_FIGURES/Scripts/F_Scatter_GS_TopSNP_locally.R

		2) GS and CPD GS_CPD_README.tx: visual inspection can be found: FIGURES/Scripts/F_Loess.R 
		while the modelling and comparison of slower vs faster metabolizers can be found: d_TABLES_and_OTHER/ModelCPDbyzGRS.R	
	
	#b_ANALYSES/PheWAS/
		A) Getting the latest version of PHESANT (v1.1): A_DownloadingPHESANT.sh
		B) Checking the wanted R version and required R packages work: B_CheckingRequirements.R


		#ExploratoryPheWAS/

			#EVER
			1) A_OneAftertheOther.sh --scripts for running the following scripts:
				1_Scan_p1.sh
				1_Scan_p2to100.sh
				1_Scan_p101to200.sh
				2_CombineResults.sh

			# ALL and NEVER work the same way
	


	#b_ANALYSES/Followup (numbering in this folder doesn't necessarily reflect the order they were analysed)

		1) Current versus Former analysis b_ANALYSES/Followup/CurrentvsFormerFEV.R
		2) Cessation sensitivity: b_ANALYSES/Followup/Cessation_Sensitivity.R
		


#--------------TABLES----------------------

	1) Forming a table of our FINEMAP results and forming a table of the weights used for calculating the GRS: T_FINEMAPresults_and_WeightsGRS.R
	2) Obtaining descriptive statistics for age, sex, the NRM genetic score & smoking variables in the UKB: T_Descriptives.R
	3) Modelling CPD by GS, and comparison of slower vs faster metabolizers: ModelCPDbyzGRS.R

#--------------FIGURES----------------------
	1) Scatter plot of GRS and Top SNP: F_Scatter_GS_TopSNP_locally.R
	2) Loess plots for main text and loess + scatterplots for supplemenetary material: F_Loess.R
	3) Current vs Former forest plot: F_CurrentFormer_ForestPlot.R