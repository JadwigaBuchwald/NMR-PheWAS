#Jadwiga Buchwald
#10.1.2022
#finemap for YFSFR (N=2119) Extended model results, region: top SNP ±2.5Mb, only including the subset of SNPs available and passing QC also in UKBiobank, n(SNP)=10133

#In bash

#change directory
#cd /[path]
#grun -n Finemap ./Scripts/Finemap.sh

#folder in which the latest version(1.4) of finemap can be found
folder=/[path]/

#Running finemap so that we allow for max 20 causal SNPs
${folder}finemap_v1.4_x86_64 --sss --in-files ./Input/master --log --n-causal-snps 20