#24.1.22

#Adding chr 4 stats and chr 19 stats together for the SNPs to be included in the GRS
####################################################
#----------
#interactively in bash
#----------

#change directory: 
	#cd /[path]


#declaring directories: 
	chr4statsDir="/[path]/"

#Creating one stats file for all 10 GRS SNPs 
	#For chr 19: keeping all but header (row number >1)
		awk 'NR>1' topconfig9SNPs.stats > temp
	#concatenating chr 4(including header) and chr 19 stats (without header)
		cat ${chr4statsDir}GRS_14thSNP_WB20210809.stats temp > GRS10SNPs.stats
	#removing unnecessary files
		rm temp