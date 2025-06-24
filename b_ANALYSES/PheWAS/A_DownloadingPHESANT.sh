#Jadwiga Buchwald
#1.2.2022
#Getting the latest version of PHESANT (v1.1)

#Interactively in bash
	#change directory
		#cd /[path]
	#run the sh script
		#grun -n DownloadingPHESANT ./A_DownloadingPHESANT.sh

#--------------------------------------------------

#DOWNLOADING, UNZIPPING AND REMOVING .ZIP FILE

#To get URL link: 
#	Open GitHub repository, make sure you have the latest version (click tags instead of branches and choose the most recent).
#	Right click the Download Zip link, and copy the URL.

#&& lets you do something based on whether the previous command completed successfully

wget "https://github.com/MRCIEU/PHESANT/archive/refs/tags/v1.1.zip" && \
unzip v1.1.zip && \
rm ./v1.1.zip
