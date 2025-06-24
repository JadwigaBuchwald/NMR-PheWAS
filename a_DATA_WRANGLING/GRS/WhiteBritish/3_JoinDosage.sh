#19.1.2022

#Joining the GRS chr4 and chr19 dosage datasets
	
#Using the join command: requires both data having an identical join field (by default it uses the first field, this must be in the same order in both files)
#################################
#----------
#interactively in bash
#----------

#change directory: 
#cd /[path]

#Declaring directory:
#Dir="/[path]/"

#Running the bash script
#grun -n JoinDosage ./JoinDosage.sh

#################################


#More on the join command:
#https://shapeshed.com/unix-join/


#Checking all looks good afterwards:
#head GRStopconfig_10SNPs_WB20210809_dosage.txt #looked good
#wc -l GRStopconfig_10SNPs_WB20210809_dosage.txt  #343663 so perfect as top row has userid and rsids


#----------------------------------------------


join ${Dir}GRSchr4SNP_WB20210809_dosage.txt GRSchr19SNPs_WB20210809_dosage.txt > GRStopconfig_10SNPs_WB20210809_dosage.txt


