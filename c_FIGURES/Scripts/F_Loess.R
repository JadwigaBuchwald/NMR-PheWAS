#26.6.2023
# Jadwiga Buchwald

#(Done locally with RStudio: Scatterplots_GRS_ValidityCheck.R)


#Table of contents
#1. Figure for main text
	# Loess curves for standardized versions (only 95% of data i.e. from -2 to 2) p2z, p4z, p6z 
#2. Figure for supplementary material
	# Scatter + loess of 
			#A) UKB zGS x CPD p1z scatter p2z loess
			#B) FY zNMR x CPD p3z scatter p4z loess
			#C) FY NMR x CPD p3 scatter p4 loess
			#D) zNMR x Cot+3HC p5z scatter p6z loess



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

#libraries
library(plyr)
library(dplyr)
library(ggplot2)

#For plotting two figures in one:
#.libPaths("[path]")
library(cowplot) 
library(gridExtra)
library(grid)


#-----------------------------------
# Getting the GRS x CPD data ready (only need to do this once, if adjustments to plots are required skip this)
#-----------------------------------


# Reading in our data
D = read.csv("./zGRS10SNPs_sexage_SmokingVariables_343662.csv", as.is=T, header=T)
str(D)


# Reading in our confounders so that we get the 10 PCs for our model
C = read.csv("[path]/ukb_n343662_phesant_confounders.csv")
names(C)
str(C)

#Only keeping the 10 PCs and eid as I already have sex and age
C<-C[,c(1, 4:13)]

# Merging D and C
D <- join(D,C, by = "eid", type ="left")
str(D)

# Making the Sex variable for the plots
D$Sex <- factor(D$sex_31, 
                levels = c(0,1), 
                labels = c("Female", "Male")) 

# Recoding the negative CPD values as missing (note there are no 0 values)
D$CPD<-ifelse(D$CPD_3456>0, D$CPD_3456,NA)  

# Getting the GRS data (non standardized)
G<-read.csv("[path]/topconfigalleleA_GRS_topSNPCiealleleB.txt",as.is=T, header=T,sep="\t")
dim(G)
#[1] 343662     14
#Keeping the wanted variables
WG<-G[,c("userId","GRS")]
names(WG)<-c("eid","GRS")	

# Merging them together 

D<-join(WG,D,by="eid",type="left")

#> dim(D)
#[1] 343662     33

# Only keeping the variables needed for plotting
D<-D[,c("eid","GRS","zGRS","Sex","CPD")] 

# Saving
write.table(D,"GRS_CPD_plotting.txt",quote=F,row.names=F) 


#######################################################
#-------------------------------------
# Reading in data
#-------------------------------------


# Reading in the data

  D<-read.table("./GRS_CPD_plotting.txt", header=T)

  YF<-read.table("./FRYFS_phenos_n2119.txt",header=T,stringsAsFactors=F)
	# Standardize NMR
  		YF$zNMR <- (YF$NMR - mean(YF$NMR)) / sd(YF$NMR) 

#######################################################
#--------------------------------------
# Figure for main text
#--------------------------------------

# Getting the n's

	# UKB
	# N for CPD and NMR points (non-missing for both):	
  		dim(subset(D,!(is.na(D$CPD)))) #23682     5
  
	# YF
  		dim(YF) #2119   10
  		dim(subset(YF,!(is.na(YF$CPD)))) #1946   10
  		dim(subset(YF,!(is.na(YF$Cotplus3HC)))) #2119   10

# UKB: zGS x CPD
	p2zlabs<-ggplot(D, aes(x=zGRS, y=CPD, color=Sex)) + 
      		geom_smooth(method=loess, aes(fill=Sex))+
       		theme(legend.position = "none", panel.grid.minor = element_blank(),
            		panel.background = element_blank(), 
            	axis.line = element_line(colour = "black"))+
      		labs(x=expression("zGS"))

	# Only plotting 95% of data	
		p2z_coord22labs<- p2zlabs +coord_cartesian(xlim =c(-2, 2),ylim =c(0, 20))

	#Adding UKB text
		UKB_loess<-grid.arrange(grobs=list(p2z_coord22labs), nrow=1, 
                            top=textGrob("UKB",gp = gpar(fontface = "bold", cex = 1.5))
    				)

# YF: zGS x CPD
	p4zlabs<-ggplot(YF, aes(x=zNMR, y=CPD, color=Sex)) + 
      		geom_smooth(method=loess, aes(fill=Sex))+
            	theme(legend.position = "none", panel.grid.minor = element_blank(),
            		panel.background = element_blank(), 
            	axis.line = element_line(colour = "black"))

	#Only plotting 95 % of data
		 p4z_coord22labs <- p4zlabs + coord_cartesian(xlim =c(-1.9, 2),ylim =c(0, 20))



# YF: zNMR x Cot+3HC
	p6zlabs<-ggplot(YF, aes(x=zNMR, y=Cotplus3HC, color=Sex)) + 
      		 geom_smooth(method=loess, aes(fill=Sex))+
           	 theme(legend.position = "none", panel.grid.minor = element_blank(),
            		panel.background = element_blank(), 
            	 axis.line = element_line(colour = "black"))+
      		 labs( y=expression("Cot + 3HC"))
    
	#Only plotting 95 % of data
        	p6z_coord22labs<- p6zlabs + coord_cartesian(xlim =c(-1.9, 2),ylim=c(0,350))


# Adding YFS + FINRISK text
	YF_loess<-grid.arrange(grobs=list(p4z_coord22labs,p6z_coord22labs), ncol=2,
                           top=textGrob("FINRISK + YFS",gp = gpar(fontface = "bold", cex = 1.5)),
                           widths = c(1,1))

# Saving legend to be put underneath the plots    
	legendbottom <- get_legend(ggplot(D, aes(x=GRS, y=CPD, color=Sex)) + 
                                 geom_smooth(method=loess, aes(fill=Sex))+
                                 theme(legend.position = "bottom")
    				)

# Combining and adding A and B to plot
	Threeloesslandscape<-plot_grid(UKB_loess,YF_loess, labels = c("A","B"), ncol = 2, nrow = 1, rel_widths = c(1, 2),rel_heights = c(1,1))
    
#Adding legend    
   	AllThreePlots<-plot_grid(Threeloesslandscape,legendbottom,nrow = 2, rel_heights = c(1, 0.1))
    
# Saving
	 save_plot("./Plots/Threeloesslandscape_yfrom0_GS.pdf", AllThreePlots, 
              ncol = 3, # we're saving a grid plot of 2 columns
              nrow = 1, # and 2 row, 
              base_aspect_ratio = 0.8 #default is 1.1. which works well, when we don't have a legend, each individual subplot should have an aspect ratio of 1.3
              
    		)
    
#--------------------------------------
#######################################################
#--------------------------------------
# Figure for Supplementary material
#--------------------------------------

#---------------------------------------------
    # Scatter + loess of  
	#A) UKB zGS x CPD p1z scatter p2z loess
	#B) FY zNMR x CPD p3z scatter p4z loess
	#C) FY NMR x CPD p3 scatter p4 loess
	#D) zNMR x Cot+3HC p5z scatter p6z loess
   
    #needed plots: p1z, p2z, p3z, p4z, p3, p4, p5z, p6z
#--------------------------------------------- 


    #A) UKB zGRS x CPD

    #--------------------------------------    
    #Scatter
    p1z<-ggplot(D, aes(x=zGRS, y=CPD, color=Sex)) + 
      geom_point(size=0.5)+
      geom_smooth(method=loess, aes(fill=Sex))+
      #theme(legend.position = "none")+
      theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      annotate("text",x=-3.75, y=140,label="n = 23682")+
      labs(x=expression(" "), y=expression(" "))
    
    #Loess
    p2z<-ggplot(D, aes(x=zGRS, y=CPD, color=Sex)) + 
      geom_smooth(method=loess, aes(fill=Sex))+
      #      theme(legend.position = "none")+
      theme(legend.position = "none", panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(x=expression(" "), y=expression(" "))
    #--------------------------------------  
    
    #A) Combining
    
    plotsUK<-list(p1z,p2z)
    pa<-grid.arrange(grobs=c(plotsUK), ncol=2, 
                           bottom=textGrob("zGS", vjust=-0.4), 
                           left=c("CPD",hjust=-0.4),
                           top=textGrob("UK Biobank",gp = gpar(fontface = "bold", cex = 1.5)),
                           widths = c(1,1))
    # vjust controls the vertical position of the title. 
    # hjust controls the horizontal position of the title.
    
    #B) & C)
    
    #--------------------------------------
    
    # NMR x CPD - Scatter plot  
    p3<-ggplot(YF, aes(x=NMR, y=CPD, color=Sex)) + 
      geom_point(size=0.5)+
      geom_smooth(method=loess, aes(fill=Sex))+
      #theme(legend.position = "none")+
      theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      annotate("text",x=0.25, y=65,label="n = 1946")+
      labs(x=expression(" "), y=expression(" "))
    
    # zNMR x CPD - Scatter
    p3z<-ggplot(YF, aes(x=zNMR, y=CPD, color=Sex)) + 
      geom_point(size=0.5)+
      geom_smooth(method=loess, aes(fill=Sex))+
      #theme(legend.position = "none")+
      theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      annotate("text",x=0.25, y=65,label="n = 1946")+
      labs(x=expression(" "), y=expression(" "))
    
    # NMR x CPD -loess
    p4<-ggplot(YF, aes(x=NMR, y=CPD, color=Sex)) + 
      geom_smooth(method=loess, aes(fill=Sex))+
      #      theme(legend.position = "none")+
      theme(legend.position = "none", panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(x=expression(" "), y=expression(" "))
    
    # zNMR x CPD -loess
    p4z<-ggplot(YF, aes(x=zNMR, y=CPD, color=Sex)) + 
      geom_smooth(method=loess, aes(fill=Sex))+
      #      theme(legend.position = "none")+
      theme(legend.position = "none", panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(x=expression(" "), y=expression(" "))
    
    #--------------------------------------

    #B) Combining
    plotsYF_CPD<-list(p3z,p4z)
    
    pb<-grid.arrange(grobs=c(plotsYF_CPD), ncol=2, 
                       bottom=textGrob("zNMR", vjust=-0.4), 
                       left=c("CPD",hjust=-0.4),
                       top=textGrob("FINRISK + YFS",gp = gpar(fontface = "bold", cex = 1.5)),
                       widths = c(1,1))    
    
    #C) Combining
    plotsYF_CPDNMR<-list(p3,p4)
    
    pc<-grid.arrange(grobs=c(plotsYF_CPDNMR), ncol=2, 
                     bottom=textGrob("NMR", vjust=-0.4), 
                     left=c("CPD",hjust=-0.4),
                     top=textGrob("FINRISK + YFS",gp = gpar(fontface = "bold", cex = 1.5)),
                     widths = c(1,1))
    
    #D)  
    #--------------------------------------
    
    p5z<-ggplot(YF, aes(x=zNMR, y=Cotplus3HC, color=Sex)) + 
      geom_point(size=0.5)+
      geom_smooth(method=loess, aes(fill=Sex))+
      #theme(legend.position = "none")+
      theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      annotate("text",x=0.25, y=1050,label="n = 2119")+
      labs(x=expression(" "), y=expression(" "))
    
    p6z<-ggplot(YF, aes(x=zNMR, y=Cotplus3HC, color=Sex)) + 
      geom_smooth(method=loess, aes(fill=Sex))+
      #      theme(legend.position = "none")+
      theme(legend.position = "none", panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(x=expression(" "), y=expression(" "))
    
    
    #--------------------------------------
    
    #D) Combining
    
    plotsYF_Cotplus3hc<-list(p5z,p6z)
    
    pd<-grid.arrange(grobs=c(plotsYF_Cotplus3hc), ncol=2, 
                     bottom=textGrob("zNMR", vjust=-0.4), 
                     left=c("Cot + 3HC",hjust=-0.4),
                     top=textGrob("FINRISK + YFS",gp = gpar(fontface = "bold", cex = 1.5)),
                     widths = c(1,1))
    
    
    # Adding labels first
    Combined<-plot_grid(pa,pb,pc,pd, labels = c("A","B","C","D"), ncol = 2,nrow = 2, rel_heights = c(1,1,1,1))
    
    #LegendBottom
    Legendbottom<-grid.arrange(grobs=list(Combined,legendbottom), nrow=2, 
                              heights = c(0.9,0.1))  
        
    # saving
    save_plot("./Plots/SupFig_ScatterLoess.pdf", Legendbottom, 
              ncol = 2, # we're saving a grid plot of 2 columns
              nrow = 2, # and 2 row, 
              base_aspect_ratio = 1.3 #default is 1.1. 
    )