#21.12.21

#We obtain a list of the rsids that were present in our FRYFS Finemapping analysis (2.5Mb flanks on both sides of the top SNP)

#----------------------------------------------
#change directory: 
cd /[path]

#Declaring directory
Dir_fy="/[path]/"
#----------------------------------------------

#	#Taking first column of the z file used in the finemap analyses. We obtain the rsid column without the header (row number >1)
	awk 'NR>1 { print $1 }' ${Dir_fy}YFSFR_nmr_Extended_chr19_corrected.z > fy_zrsidlist12060.txt 


 