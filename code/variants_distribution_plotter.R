#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#This Rscript is meant to be run using the terminal, from the same folder in which the variants_unique_ann.csv file is located
# To run this script from the terminal, use Rscript ../../../../../../code/variants_distribution_plotter.R variants_unique_ann.csv abl
#Df name is the location of the file within the rscript
#gene_name will help us decide on which regions of the protein to focus on in the plot
input_df=read.csv(args[1])
gene_name=args[2]

# args="data/Consensus_Data/Novogene_lane15/sample_3/sscs/variant_caller_outputs/variants_unique_ann.csv"
# gene_name="ABL"
library(dplyr)
library(plotly)
library(stringr)
library(ggplot2)
library(htmlwidgets)
cleanup=theme_bw() +
  theme(plot.title = element_text(hjust=.5),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(face="bold",color="black",size="11"),
        text=element_text(size=11,face="bold"),
        axis.title=element_text(face="bold",size="11"))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(33, "Set2"))

if(gene_name%in%c("abl","ABL")){
  input_df=input_df%>%mutate(region=case_when(protein_start<250&protein_start>=242~1,
                                              protein_start<258&protein_start>=250~2,
                                              protein_start<266&protein_start>=258~3,
                                              protein_start<274&protein_start>=266~4,
                                              protein_start<282&protein_start>=274~5,
                                              protein_start<290&protein_start>=282~6,
                                              protein_start<298&protein_start>=290~7,
                                              protein_start<306&protein_start>=298~8,
                                              protein_start<314&protein_start>=306~9,
                                              protein_start<322&protein_start>=314~10,
                                              protein_start<330&protein_start>=322~11,
                                              protein_start<338&protein_start>=330~12,
                                              protein_start<346&protein_start>=338~13,
                                              protein_start<354&protein_start>=346~14,
                                              protein_start<362&protein_start>=354~15,
                                              protein_start<370&protein_start>=362~16,
                                              protein_start<378&protein_start>=370~17,
                                              protein_start<386&protein_start>=378~18,
                                              protein_start<394&protein_start>=386~19,
                                              protein_start<402&protein_start>=394~20,
                                              protein_start<410&protein_start>=402~21,
                                              protein_start<418&protein_start>=410~22,
                                              protein_start<426&protein_start>=418~23,
                                              protein_start<434&protein_start>=426~24,
                                              protein_start<442&protein_start>=434~25,
                                              protein_start<450&protein_start>=442~26,
                                              protein_start<458&protein_start>=450~27,
                                              protein_start<466&protein_start>=458~28,
                                              protein_start<474&protein_start>=466~29,
                                              protein_start<482&protein_start>=474~30,
                                              protein_start<490&protein_start>=482~31,
                                              protein_start<498&protein_start>=490~32,
                                              T~0))
}
input_df=input_df%>%rowwise()%>%mutate(ID=paste(protein_start,amino_acids,sep=""))
plot=ggplot(input_df%>%filter(consequence_terms%in%"missense_variant",protein_start>=242,protein_start<=494),aes(x=protein_start,y=ct))+
  geom_col(color="black",aes(fill=factor(region)))+
  scale_fill_manual(values=getPalette(33))+
  cleanup
plotlyplot=ggplotly(plot)
htmlwidgets::saveWidget(
  widget = plotlyplot, #the plotly object
  file = "variants_distribution.html", #the path & file name
  selfcontained = T #creates a single html file
)
