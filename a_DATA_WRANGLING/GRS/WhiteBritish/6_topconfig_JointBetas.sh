#Obtaining the top configuration joint betas from the finemap results (-> topconfig_JointBetas.txt)

#Doing this interactively in bash

#cd /[path]

#---------
#From .config file (1rank 2config 3prob 4log10bf 5odds 6k 7prob_norm_k 8h2 9h2_0.95CI 10mean 11sd) we want config, mean and sd
#For each(config, mean and sd) Getting the first two lines and then from that the last one (i.e. dropping the header and only taking the first row of data)
	#list of rsids in the top configuration
		head -n 2 topconfig.txt | cut -d " " -f 2 | tail -1 > tempconfig.txt #Second column
	#The joint betas for the top configuration SNPs
		head -n 2 topconfig.txt | cut -d " " -f 10 | tail -1 > tempmean.txt #10th column
	#The standard errors for the joint beta estimates
		head -n 2 topconfig.txt | cut -d " " -f 11 | tail -1 > tempsd.txt #11th column
#We form one data set
	cat tempconfig.txt tempmean.txt tempsd.txt > topconfigtemp.txt #we stack these three rows.
#We remove the temporary files
	rm temp*.txt


#Transposing the file
	bashfolder="/[path]/"
	bash ${bashfolder}trans.sh topconfigtemp.txt > topconfig_JointBetas.txt
#Again, removing temporary files
	rm topconfigtemp.txt

#By hand adding the header: SNP,betaJ,seJ
