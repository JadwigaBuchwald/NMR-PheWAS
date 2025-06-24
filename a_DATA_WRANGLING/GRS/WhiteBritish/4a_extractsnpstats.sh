#Extract the summary statistics for the wanted top config SNPs (.stats)
#9 SNPs

#################################
#----------
#interactively in bash
#----------

#change directory: 
#cd /[path]

#Declaring directory:
#Dir="/[path]/"

#Running the bash script
#grun -n extractsnpstats ./extractsnpstats.sh

#################################


#we keep only the rows from stats file that include the rsid present in the rsidlist.txt
#code from: https://unix.stackexchange.com/questions/293684/basic-grep-awk-help-extracting-all-lines-containing-a-list-of-terms-from-one-f
	grep -w -F -f rsidlist.txt ${Dir}UK_zrsids_WB20210809.stats > temp.txt
#we take the header (the line with "chromosome" in it)
	grep -w "chromosome" ${Dir}UK_zrsids_WB20210809.stats > header.txt
#We stack these together
	cat header.txt temp.txt > topconfig9SNPs.stats
#remove the unnecesary files
	rm header.txt temp.txt


