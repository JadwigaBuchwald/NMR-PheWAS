#Annotation of the MRBase PheWAS eQTLs that were statistically significant in our top SNP (rs56113850) PheWAS 

#(Actual analysis and plotting of the MRBase and FinnGen PheWAS results performed locally using R studio.)



#2023 Sep 13


#Note: The eQTLs ar most probably from this paper: https://www.nature.com/articles/s41588-021-00913-z
#Since when checking the eQTL traits at https://gwas.mrcieu.ac.uk/datasets/
#author is Vosa U and year 2018: eg https://gwas.mrcieu.ac.uk/datasets/eqtl-a-ENSG00000015153/


#----------------------------------------------------------------------
#In Rstudio (locally)
#----------------------------------------------------------------------


#Picking the eQTL rows and only keeping the columns rank, trait and p

      GED<- MBfdr_trait[67:80,c("rank","trait","p")]
      str(GED)
      genelist<-GED$trait
      
      #-->Copied this genelist and used this for doing the annotation on atlas. 



#----------------------------------------------------------------------
#Using R-4.1.1 interactively -opening R in bash
#----------------------------------------------------------------------

#cd /[PATH]/ukbb/jadwiga/MRBaseandFinnGen
#RDir="/apps/statistics2/"
#${RDir}R-4.1.1/bin/R



#----------------------------------------------------------------------
#Rscript 
#----------------------------------------------------------------------


#-----------------------------------
#Annotation
#-----------------------------------
sessionInfo()
getwd()

#loading biomaRt

	.libPaths("/[PATH]/ukbb/jadwiga/Rlib4.1.1")

      	#library(BiocManager)
      	#BiocManager::install("biomaRt")

	library(biomaRt)


# Specifying the list of genes we want annotated
	
	gene_list<-c("ENSG00000015153","ENSG00000090006","ENSG00000115421","ENSG00000120049","ENSG00000123815","ENSG00000126838",
		"ENSG00000139998","ENSG00000162551","ENSG00000188493", "ENSG00000221962", "ENSG00000233622", "ENSG00000268339","ENSG00000269843","ENSG00000269858")


# Specifying the Ensembl database (human genes)

      ensembl <- useEnsembl(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")
      

# Defining the attributes we want to retrieve

	head(listAttributes(ensembl))
	dim(listAttributes(ensembl)) #[1] 3169    3
	listAttributes(ensembl)[1:30,]

	attributes <- c("ensembl_gene_id", "chromosome_name", "band", "start_position", "end_position", "hgnc_symbol", "description","gene_biotype")


# Retrieving gene annotations

	gene_annotations <- getBM(attributes = attributes, filters = "ensembl_gene_id", values = gene_list, mart = ensembl)
     

#Saving the results

	write.table(gene_annotations,"./MRBaseGeneExpressionResults_annotations.txt",sep="\t",quote=F,row.names=F) 


####################################################################

#-----------------------------------
#Combining the annotations with the results (rank and p-value) and reordering the columns
#-----------------------------------

#----------------------------------------------------------------------
#Back in Rstudio (locally)
#----------------------------------------------------------------------

#Getting the Gene-Expression Data

      #Reading the annotated data
      annotations <-read.table("MRBaseGeneExpressionResults_annotations.txt",dec=",",sep="\t",header=T)
      str(annotations)
      
      #There is nothing for ENSG00000268339 but I checked the GWAS Catalog: https://www.ebi.ac.uk/gwas/search?query=ENSG00000268339 it also says that there are no results found for that search term.
      #Also, checked that i did have it included in my genelist I annotated on atlas.
      
      #Joining GED and annotations
      
      names(GED)<-c("rank","ensembl_gene_id","p")
      str(GED)
      GeneExpression<-join(GED, annotations, by="ensembl_gene_id", type="left")
      
      GE<-GeneExpression[order(GeneExpression$p),]
      names(GE)
      GEreordered<-GE[,c("ensembl_gene_id","rank", "p","chromosome_name", "band", "start_position", "end_position", "hgnc_symbol","description", "gene_biotype")]
      
      # Save the data frame as an Excel file
      write.xlsx(GEreordered, file="MRBase_GeneExpressionAnnotations_ForST.xlsx")
      
