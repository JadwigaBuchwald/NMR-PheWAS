#Getting genotype data from the bgen files using qctool v2 


#################################
#----------
#interactively in bash
#----------

#declaring directory: 
#cd /[path]

#Running the sh script
#grun -n 1b_GenFromBgen_chr4andTopSNP.sh ./1b_GenFromBgen_chr4andTopSNP.sh

#################################

#----------
#sh script
#----------

#declaring directories: 
	genoDir="/[path]/"
	qctoolDir="/[path]/"




#INPUT
#-g bgen file
#-s sample file
#-incl-rsids list of rsids to be included
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
#2) Have to have a final enter in the rsid list file
#3) Have to include .gen ending for the output genotype file (if that is desired) or it will automatically create a vcf file


########################
#Actual command 
#Using Variant inclusion (incl-rsids) and sample inclusion (incl.samples) options
########################
${qctoolDir}qctool_v2.0-rc5 -g ${genoDir}ukb_imp_chr4_v3.bgen -s ${genoDir}ukb22627_imp_chr1-22_v3_s487395.sample -og GRSchr4SNP_WB20210809.gen -os GRSchr4SNP_WB20210809.sample -incl-rsids GRS_chr4SNP_rsid.txt -incl-samples ${genoDir}inclusions/22627_imp_chr1-22_v3_include_WhiteBritish_unrelated.0442_goodQC_withdrawals_20210809.txt
${qctoolDir}qctool_v2.0-rc5 -g ${genoDir}ukb_imp_chr19_v3.bgen -s ${genoDir}ukb22627_imp_chr1-22_v3_s487395.sample -og rs56113850_WB20210809.gen -os rs56113850_WB20210809.sample -incl-rsids rs56113850.txt -incl-samples ${genoDir}inclusions/22627_imp_chr1-22_v3_include_WhiteBritish_unrelated.0442_goodQC_withdrawals_20210809.txt

