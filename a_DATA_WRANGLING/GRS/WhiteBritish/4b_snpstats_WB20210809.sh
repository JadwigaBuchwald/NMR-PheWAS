#Getting snpstats for our chr4 SNP and the top SNP, QC:d white british non relatives (N=343662) using qctool v2 

####################################################
#----------
#interactively in bash
#----------

#declaring directory: 
	#cd /[path]

#Running the sh script
	#grun -n 4b_snpstats_WB20210809 ./4b_snpstats_WB20210809.sh


####################################################

#----------
#sh script
#----------

#declaring directories: 
	qctoolDir="/[path]/"


#INPUT
#-g gen file

#OUTPUT
#-osnp name for snpstats file



${qctoolDir}qctool_v2.0-rc5 -g GRSchr4SNP_WB20210809.gen -snp-stats -osnp GRSchr4SNP_WB20210809.stats
${qctoolDir}qctool_v2.0-rc5 -g rs56113850_WB20210809.gen -snp-stats -osnp rs56113850_WB20210809.stats
