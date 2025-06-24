#10.1.2022

#QC of UKBB (in this script using bash commands)
#I.e. Obtaining the SNPs that pass the quality control: 
#SNPs to be deleted: SNPs with HW_exact_p_value<=1e-6 || missing_proportion>0.05 || info<=0.7

#1) awk: From the ukb snpstats: keeping the SNPs that pass the QC --> ukb_QCOK.stats
#2) grep: Deleting duplicate positions: ukb_QCOK.stats --> ukb_QCOK_unique.stats
#3) awk: Extracting rsid, position and alleles (+ maf for plotting) from ukb_QCOK_unique.stats as well as the FRYFS z file -> ukb_QCOK_unique_7col.stats & ./Data/YFSFR_nmr_Extended_chr19_corrected_5col.z

#----------------------------------------------
#Doing steps 1-3 with bash interactively

#declaring directory: 
#cd /[path]

#declaring directories: 
#Dir="/[path]/"
#Dir_fy="/[path]/"


#----------------------------------------------


#----------------------------------------------
#1) QC of UKBB: From the ukb snpstats: keeping the SNPs that pass the QC --> ukb_QCOK.stats
#----------------------------------------------

#Getting rid of unnecessary rows
	
	#Looking at the top and last rows of our snp-stats 
	head ${Dir}UK_zrsids_WB20210809.stats
	tail ${Dir}UK_zrsids_WB20210809.stats

	#We only keep the rows that do not have # in them (Delete the first rows and the last row)
	#with the condition(|) that '#' does not(-v, this inverses the condition) appear in the row (grep). 
	cat ${Dir}UK_zrsids_WB20210809.stats | grep -v '#' > ./Data/UK_zrsids_WB20210809.stats

#SNPs to be deleted: SNPs with HW_exact_p_value<=1e-6 || missing_proportion>0.05 || info<=0.7

	#We print the wanted column names
	awk 'NR==1 { print $8,$19,$17 }' ./Data/UK_zrsids_WB20210809.stats
	#HW_exact_p_value missing_proportion info

	#We do the QC
	awk '{if($8<=1e-6 || $19>0.05 || $17<=0.7)print > "./Data/ukb_failedqc.txt";else print > "./Data/ukb_passedqc.stats"}' ./Data/UK_zrsids_WB20210809.stats

	#Note, header stays with ukb_failedqc.txt
	head -1 ./Data/ukb_failedqc.txt > ./Data/header_ukb.stats #header is here

#We check amount of SNPs in UKzrsids, those passing qc and those failing

	#We start with:
	wc -l ./Data/UK_zrsids_WB20210809.stats
	#12045 ./Data/UK_zrsids_WB20210809.stats #this includes the header
	#So n(SNP)=12044
	
	#Failed
	wc -l ./Data/ukb_failedqc.txt
	#1897 ./Data/ukb_failedqc.txt #this includes the header
	#So failing qc n(SNP)=1896

	#Passing QC
	wc -l ./Data/ukb_passedqc.stats
	#10148 ./Data/ukb_passedqc.stats  #this does not include the header
	#So passing qc n(SNP)=10148

#Finalizing

	#We add the header
	cat ./Data/header_ukb.stats ./Data/ukb_passedqc.stats > ./Data/ukb_QCOK.stats
	#we remove the file without the header
	rm ./Data/ukb_passedqc.stats
	#we remove the other files (except for header file which we will use later as well) as they not needed (failed file and snpsatst without unnecessary rows)
	rm ./Data/ukb_failedqc.txt ./Data/UK_zrsids_WB20210809.stats


#----------------------------------------------
#2) We delete duplicate positions: ./Data/ukb_QCOK.stats > ./Data/ukb_QCOK_unique.stats
#----------------------------------------------


# 	#seeing if any duplicate positions:
	#Taking position 
	awk 'NR>1 { print $4 }' ./Data/ukb_QCOK.stats > temp.txt 
	uniq -d temp.txt > ./Data/ukb_duplicates.txt
	rm temp.txt
#	#the following positions have duplicates
	#39527625
	#39587975
	#40196287
	#41232212
	#41426422
	#41452494
	#42960362

	
	#We delete SNPs with these positions from ./Data/ukb_QCOK.stats

#	-To invert the Grep output , use the -v flag
	
	grep -vFwf ./Data/ukb_duplicates.txt ./Data/ukb_QCOK.stats > ./Data/ukb_QCOK_unique.stats

	#Checking the files
	wc -l ./Data/ukb_QCOK.stats
	#10149 ./Data/ukb_QCOK.stats
	wc -l ./Data/ukb_QCOK_unique.stats
	#10135 ./Data/ukb_QCOK_unique.stats

	#10149 - 10135 =14 (so the 7 duplicates have been deleted)

	#we remove the files we don't need anymore
	#rm ./Data/ukb_QCOK.stats

#----------------------------------------------
#3) Extracting rsid, position and alleles (+ maf for plotting) from ukb_QCOK_unique.stats as well as the FRYFS z file -> ukb_QCOK_unique_7col.stats & ./Data/YFSFR_nmr_Extended_chr19_corrected_5col.z
#----------------------------------------------

#UKBIOBANK
#columns 2,4,5,6,14,15,16
	#We print the wanted column names
	awk 'NR==1 { print $2,$4,$5,$6,$14,$15,$16 }' ./Data/ukb_QCOK_unique.stats
	#rsid position alleleA alleleB minor_allele_frequency minor_allele major_allele
	awk ' { print $2,$4,$5,$6,$14,$15,$16 }' ./Data/ukb_QCOK_unique.stats > ./Data/ukb_QCOK_unique_7col.stats
	wc -l ./Data/ukb_QCOK_unique_7col.stats
	#10135 ./Data/ukb_QCOK_unique_7col.stats


#FRYFS
#columns 1,3,4,5,6
	awk 'NR==1 { print $1,$3,$4,$5,$6 }' ${Dir_fy}YFSFR_nmr_Extended_chr19_corrected.z
	#rsid position noneff_allele eff_allele maf
	awk ' { print $1,$3,$4,$5,$6 }' ${Dir_fy}YFSFR_nmr_Extended_chr19_corrected.z > ./Data/YFSFR_nmr_Extended_chr19_corrected_5col.z



