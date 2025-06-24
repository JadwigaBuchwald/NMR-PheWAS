#26.6.2023
# Jadwiga Buchwald


#These were run locally using RStudio

#----------------------------------------------------
#Drawing forest plots using nightingale's ggforestplot:
#https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html
#----------------------------------------------------


#######################################################

#-----------------------------------
#Rscript
#-----------------------------------

setwd("[path]")
sessionInfo()
getwd()

#-----------------
#Libraries:
#-----------------

#install.packages("devtools")
library(devtools)

#install.packages("ggforestplot") #Did not work now taht i updated R
#devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

#install.packages("tidyverse")
library(tidyverse)
library(plyr)
library(dplyr)

#For adding the curly brackets
library(grid)
library(pBrackets) 
#-----------------------------------

#-----------------
# Getting data:
#-----------------

	L<-read.table("./CurrentFormer_4FEVVariables_LinearForest.txt", sep="\t",header=TRUE)
	str(L)
	dim(L) #8 x 8

#Renaming the data variable with a capital letter so that it looks better in the plot.
	names(L)[names(L)=="data"]<-"Data"

#-----------------
# Forest Plot:
#-----------------

	p <- ggforestplot::forestplot(
	  df = L,
	  name = Phenotype,
	  estimate = beta,
	  se = se,
 	 colour= Data,
	  pvalue = pvalue,
	  psignif = 8.709755e-05,
	  xlab = "Beta \n 1-SD increment in phenotype per 1-SD increment in the GS of the NMR",
	  title = "Linear Regression",
	  logodds = FALSE
	)+
 	 ggforce::facet_col(
 	   facets = ~Cat3_Title,
 	   scales = "free_y",
 	   space = "free"
 	 ) 

#-------------------------------------------------
# Version with brackets
#-------------------------------------------------

#In order to be able to save the plot with ggsave I have to be able to save everything into an object.

	bracketsGrob <- function(...){
	  l <- list(...)
	  e <- new.env()
 	 e$l <- l
	  grid:::recordGrob(  {
	    do.call(grid.brackets, l)
	  }, e)
	}

# note that units here are "npc", the only unit (besides physical units) that makes sense
# when annotating the plot panel in ggplot2 (since we have no access to 
# native units)

#Saving the brackets into objects (left hand side of plot is 0 and righthand side of plot is 1)

	b1 <- bracketsGrob(0.24, 0.905, 0.37, 0.905, lwd=1, col="black",h=0.02)
	b2 <- bracketsGrob(0.15, 0.665, 0.32, 0.665, lwd=1, col="black",h=0.02)
	b3 <- bracketsGrob(0.7, 0.43, 0.875, 0.43, lwd=1, col="black",h=0.02)
	b4 <- bracketsGrob(0.25, 0.19, 0.385, 0.19, lwd=1, col="black",h=0.02)

#Saving everything in one and adding the text (for annotate, the x and y axis go as in the plot)

	plotforsaving <- p + 
	  annotation_custom(b1) + annotation_custom(b2)+ annotation_custom(b3)+  annotation_custom(b4)+
 	 annotate("text",x=-0.035, y=4.36,label=expression(italic(p) == "0.0193"))+
 	 annotate("text",x=-0.045, y=3.36,label=expression(italic(p) == "0.0334"))+
 	 annotate("text",x=0.035, y=2.36,label=expression(italic(p) == "0.0008"))+
	  annotate("text",x=-0.035, y=1.36,label=expression(italic(p) == "0.0089"))

#Saving plot
	ggsave(
	  "./Plots/CurrentvsFormer_GS_ggsave.pdf"
	)   

