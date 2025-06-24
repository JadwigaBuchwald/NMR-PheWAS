#10.1.2022


#Selecting rows from z file based on row number, and rows and columns from ld file based on vector of row/column numbers to be selected
#The vector of row/column numbers to be selected (WhichAvailable.txt) has been created in R

#Note z file has header, ld does not. Both need to be space-delimited files


#----------------------------------------------


#Doing this with bash interactively

#declaring directory: 
#cd /[path]

#declaring directories: 
#zDir="/[path]/"
#ldDir="/[path]/"
#bashfunctions="/[path]/"

#----------------------------------------------



#We extract the rows/ columns based on WhichAvailable list of rows wanted
#Based on script: https://unix.stackexchange.com/questions/418271/bash-filter-rows-by-line-number

#z 
	#selecting wanted rows (because of header in z file we always want the [row in WhichAvailable +1] )
		awk 'NR==FNR{ pos[$1+1]; next }FNR in pos' ./Data/WhichAvailable.txt $zDir/YFSFR_nmr_Extended_chr19_corrected.z > z_selected.txt
		wc -l z_selected.txt
		#10133 z_selected.txt

	#Note: have to add header to the selected z file at the end!
		head -1 $zDir/YFSFR_nmr_Extended_chr19_corrected.z > header.txt
		cat header.txt z_selected.txt > ../Input/subset10133.z
		rm header.txt z_selected.txt

	#Checking dimensions
		#rows:
		wc -l ../Input/subset10133.z #10134 ../Input/subset10133.z

		#columns:
		head -n 1 ../Input/subset10133.z | awk '{print NF}' #8


#ld


	#ld selecting wanted rows
		awk 'NR==FNR{ pos[$1]; next }FNR in pos' ./Data/WhichAvailable.txt $ldDir/FRYFS_chr19_nmr_Extended.ld > ld_selected.txt

	#selecting wanted columns
	
		#We create array of columns to be kept (need to have commas as delimiters)
		#We transpose the tab delimited file WhichAvailable.txt to a comma delimited file WhichAvailable_transposed.txt
		#We use the function in trans.sh to do this
			bash ${bashfunctions}trans.sh ./Data/WhichAvailable.txt > WhichAvailable_transposed.txt

		#We read this list as an array in to bash
			readarray keep < WhichAvailable_transposed.txt
		#We cut based on this array
			cut -d " " -f$keep  ld_selected.txt > ../Input/subset10133.ld
		
		#We remove the intermediate files
			rm ld_selected.txt WhichAvailable_transposed.txt 

		#We check the dimensions:
			#rows:
			wc -l ../Input/subset10133.ld #10133 ../Input/subset10133.ld

			#columns:
			head -n 1 ../Input/subset10133.ld | awk '{print NF}' #10133


#Note: For v1.4 of FINEMAP z file needs the alleles in header as: allele1 and allele2 (as opposed to noneff_allele eff_allele in v1.2). I changed these by hand before running FINEMAP. 

