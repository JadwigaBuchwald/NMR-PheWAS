#Getting the rsids (.gen and .stats) found in the fryfs z files for our sample


#################################
#----------
#interactively in bash
#----------

#declaring directory: 
#cd /[path]

#Running the sh script
#grun -n gen_zrsids_chr19_WB20210809 ./Scripts/gen_zrsids_chr19_WB20210809.sh

#################################

#----------
#sh script
#----------

#declaring directories: 
	genoDir="/[path]/"
	qctoolDir="/[path]/"




#----------
#INPUT
#-g bgen file
#-s sample file
#-incl-rsids list of variants to be included in our 5Mb region(top SNP+-2.5Mb, 19:38853297-43852500)
#-incl-samples list of samles to be included

#incl sample list contains:
#* White British
#* Every pair unrelated to KING's kinship value 0.0442
#* Removed heterozygosity and missingness outliers 
#* Removed sex chromosome aneuploidies 
#* Removed mismatches between reported and inferred sex
#* Removed individuals that UKBB did not use in relatedness calculations
#* Withdrawals were removed as in sample file downloaded 9 Aug 2021.

#OUTPUT
#-og name for gen file
#-os name for sample file

#NOTE:
#1) Have to use qctool v2 as BGEN v1.2 format has been used for the imputed ukbiobank data!
#2) Have to include .gen ending for the output genotype file (if that is desired) or it will automatically create a vcf file
#3) Have to have a final enter in the rsid list file

#----------

${qctoolDir}qctool_v2.0-rc5 -g ${genoDir}ukb_imp_chr19_v3.bgen -s ${genoDir}ukb22627_imp_chr1-22_v3_s487395.sample -og UK_zrsids_WB20210809.gen -os UK_zrsids_WB20210809.sample -incl-rsids ./fy_zrsidlist12060.txt -incl-samples ${genoDir}inclusions/22627_imp_chr1-22_v3_include_WhiteBritish_unrelated.0442_goodQC_withdrawals_20210809.txt
${qctoolDir}qctool_v2.0-rc5 -g UK_zrsids_WB20210809.gen -snp-stats -osnp UK_zrsids_WB20210809.stats