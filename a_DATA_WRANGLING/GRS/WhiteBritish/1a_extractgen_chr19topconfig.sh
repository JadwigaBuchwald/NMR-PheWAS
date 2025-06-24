#Extract the wanted top config SNPs (.gen)
#9 chr19 SNPs

#################################
#----------
#interactively in bash
#----------

#declaring directory: 
#cd /[path]

#Running the sh script
#grun -n extractgen ./extractgen.sh

#################################


#----------
#sh script
#----------

#declaring directories: 
	Dir="/[path]/"

#-----------

#we keep only the rows from gen file that include the rsid present in the rsidlist.txt (this contains the rsids of the 9 top config SNPs)
grep -w -F -f rsidlist.txt ${Dir}UK_zrsids_WB20210809.gen > topconfig9SNPs.gen
#the above code from: https://unix.stackexchange.com/questions/293684/basic-grep-awk-help-extracting-all-lines-containing-a-list-of-terms-from-one-f
