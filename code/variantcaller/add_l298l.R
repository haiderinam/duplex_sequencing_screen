add_l298l=function(input_df_nol298l,input_df_l298l){
  library(dplyr)
  # No L298L means a CTG at residue 298
  # L298L means a CTA at residue 298
  # The two types of dataframes come from the same data source.
  # That data source was aligned first to a reference without L298L and then to a reference with L298L
  # input_df_nol298l=read.csv("data/Consensus_Data/novogene_lane18/sample11/no_l298l/sscs/variants_unique_ann.csv",header = T,stringsAsFactors = F)
  # input_df_l298l=read.csv("data/Consensus_Data/novogene_lane18/sample11/sscs/variant_caller_outputs/variants_unique_ann.csv",header = T,stringsAsFactors = F)

  input_df_l298ladded=rbind(input_df_nol298l%>%filter(!protein_start%in%c(290:297),!protein_start%in%c(299:305)),
                            input_df_l298l%>%filter(protein_start%in%c(290:305),!protein_start%in%c(298)))
  # input_df_l298ladded=rbind(input_df_nol298l%>%filter(!protein_start%in%c(290:305)),
  #                           input_df_l298l%>%filter(protein_start%in%c(290:305)))

  # input_df_l298ladded=input_df_l298ladded%>%filter(protein_start%in%c(242:322))%>%rowwise()%>%filter(alt_codon%in%twist$Codon)
  input_df_l298ladded=input_df_l298ladded%>%arrange(protein_start)
  input_df_l298ladded
}
