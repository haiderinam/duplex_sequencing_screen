heatmap_plotting_function=function(df_forplot,resi_start,resi_end,fill_variable="ct",fill_name="Count"){
  # output_df=read.csv("data/Consensus_Data/novogene_lane18/sample12/sscs/variant_caller_outputs/variants_unique_ann.csv")
  # heatmap_plotting_function(output_df,242,321,"ct")

  # output_df=read.csv("data/Consensus_Data/novogene_lane18/sample12/sscs/variant_caller_outputs/variants_unique_ann.csv")
  # output_df=screen_compare_means
  # df_forplot=output_df
  # fill_variable="ct"
  # resi_start=242
  # resi_end=321
  df_forplot$fill_variable=df_forplot[[fill_variable]]

  twist=read.csv("data/codon_table.csv",header = T)
  twist=twist%>%filter(Twist%in%T)

  df_forplot=df_forplot%>%filter(protein_start%in%c(resi_start:resi_end),
                                 alt_codon%in%twist$Codon,
                                 !consequence_terms%in%"stop_gained")

  df_grid  = expand.grid(protein_start = c(resi_start:resi_end),alt_aa = unique(df_forplot$alt_aa))
  df_forplot=merge(df_grid,df_forplot,by=c("protein_start","alt_aa"),all=T)

  ###Trying to add in the score as WT for residues that are wt
  reference_seq=read.table("data/Refs/ABL/abl_cds_translation.txt")
  df_forplot=df_forplot%>%
    rowwise()%>%
    # group_by(protein_start)%>%
    mutate(ref_aa=case_when(ref_aa%in%NA~substr(reference_seq,protein_start,protein_start),
                            T~ref_aa))%>%
    mutate(wt=case_when(ref_aa==alt_aa~T,
                        T~F))


  df_forplot$alt_aa=factor(df_forplot$alt_aa,levels=c("P","G","Y","W","F","V","L","I","A","T","S","Q","N","M","C","E","D","R","K","H"))

  output_ggplot=ggplot(df_forplot,aes(x=protein_start,y=alt_aa))+
    geom_tile(data=subset(df_forplot,!is.na(fill_variable)),aes(fill=fill_variable))+
    scale_fill_gradient2(low ="darkblue",mid="white", high ="red",name=fill_name)+
    geom_tile(data=subset(df_forplot,is.na(fill_variable)&wt%in%F),aes(color="white"),linetype = "solid",color="white", fill = "gray90", alpha = 0.8)+
    geom_tile(data=subset(df_forplot,is.na(fill_variable)&wt%in%T),aes(color="white"),linetype = "solid",color="white", fill = "yellow", alpha = 0.4)+
    theme(panel.background=element_rect(fill="white", colour="black"))+scale_x_continuous(name="Residue on the ABL Kinase",limits=c(resi_start,resi_end),expand=c(0,0))+
    ylab("Mutant Amino Acid")

  output_ggplot
}
