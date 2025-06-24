#2023 March 16th
#Extracting the 61 variables
#Jadwiga Buchwald

#Extracting the 61 variables that were at least FDR significant for ALL, EVER or NEVER in our broad brush PHESANT analyses.

cd [path]


DataDir="[path]"
phenoFile="${DataDir}ukb_all_1_2_3_sorted_n343662_phesant_header.csv"
#wc -l $phenoFile #343663 

#Getting the columns we want to extract

	#First we get the list of variables wanted
		#We obtain the list of 61 variable names we want 
		#We take the first column from [path]PHESANT_Results_forForest.csv
		cut -d "," -f 1 [path]PHESANT_Results_forForest.csv > list_of_59.txt
		cut -f 1 ../FDR_initial/IntermediateData/Initial_61_FDRVariables.txt > ./IntermediateData/list_of_61.txt


	#We then add a x at the beginning and _0 at the end and make sure they are in the original ukbb format 
		awk '(NR>1) {print $0}' ./IntermediateData/list_of_61.txt > ./IntermediateData/temp.txt
		head -n 1 ./IntermediateData/list_of_61.txt > ./IntermediateData/header.txt
	#note: sed stream editor, s substitution command, /replacement_from/replacement_to/, statement, ^matches the beginning of the line, #.* matches everything after #, $ matches the end of the line
		cat ./IntermediateData/temp.txt | sed 's/^/x/g' | sed 's/#.*//g'  | sed 's/$/_0/g'  > ./IntermediateData/temp2.txt
		cat ./IntermediateData/header.txt ./IntermediateData/temp2.txt > ./IntermediateData/list_of_61.txt
		rm ./IntermediateData/header.txt ./IntermediateData/temp.txt ./IntermediateData/temp2.txt

	#Then getting the columns we want
		head -n 1 $phenoFile | sed 's/,/\n/g' | cat -n | grep -f ./IntermediateData/list_of_61.txt > ./IntermediateData/columns_wanted.txt

	#We now get 411 columns! 
	#note
	#a) As for example many have multiple measurements such as the spirometry measurements. PHESANT then takes the mean of these.
	#b) Some of the binary variables are from a list of options? e.g. operative procedures

	
	#We take the first column and transpose it in to a row with commas seperating the values.
	bashfolder=[path]
	cut -f 1 ./IntermediateData/columns_wanted.txt > ./IntermediateData/colnum.txt
	bash ${bashfolder}trans.sh ./IntermediateData/colnum.txt  > ./IntermediateData/colnum_transposed.txt

	#We delete the empty spaces/ tabs
	#^[ \t]* : Search pattern ( ^ – start of the line; [ \t]* match one or more blank spaces including tab)
	sed 's/[ \t]*//g' ./IntermediateData/colnum_transposed.txt > ./IntermediateData/colnum_transp.txt

	rm ./IntermediateData/colnum.txt ./IntermediateData/colnum_transposed.txt

#Extracting the wanted columns 

	#Note!!! First by hand we add 1 (eid) at the beginning of the list and 16696, 16246 & 16412  (see below)

	#We save our list of wanted variables
	colnum_list=($(cat ./IntermediateData/colnum_transp.txt)) 

	#Getting the wanted data
	cut -d, -f $colnum_list  $phenoFile > ./IntermediateData/ukb_sorted_n343662_61variables.csv


#-----------------------------------------------
#We check all looks good (the two missing phenotypes are included):

	#amount of rows
		wc -l ./IntermediateData/ukb_sorted_n343662_61variables.csv
		#343663 ./IntermediateData/ukb_sorted_n343662_61variables.csv


	#amount of columns
	head -n 1 ./IntermediateData/ukb_sorted_n343662_61variables.csv | awk -F, '{print NF}'
	#415

	#first few rows and columns:
	head -n 3 ./IntermediateData/ukb_sorted_n343662_61variables.csv | cut -d, -f 1,2,3,4
	#"eid","x48_0_0","x1180_0_0","x1239_0_0"
	#"1000015","87.40000000000001","2","1"
	#"1000039","116","3","0"



#-----------------------------------------------
#Later noticed that I am missing 
#26678 & 25874 & 26536

	head -n 1 $phenoFile | sed 's/,/\n/g' | cat -n | grep '26678'
	# 16696  "x26678_2_0"
	# 16697  "x26678_3_0"

	head -n 1 $phenoFile | sed 's/,/\n/g' | cat -n | grep '25874'
	# 16246  "x25874_2_0"
	# 16247  "x25874_3_0"

	head -n 1 $phenoFile | sed 's/,/\n/g' | cat -n | grep '26536'
	# 16412  "x26536_2_0"
	# 16413  "x26536_3_0"


#As PHESANT USES FIRST OCCURENCE ONLY I added 16696, 16246 & 16412 to our extraction list above and redid the extraction.


