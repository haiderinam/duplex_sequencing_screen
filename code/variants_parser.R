# The following function takes in output from the variant caller, and filters it
# Filters for twist codon
# Calculates MAFs
# Calculates total mutant cells/mL based on total cells/mL and MAF. The total cells value defaults to 1.
# Parses out the ref amino acid and the alt amino acid
# Figures out which codons would realistically be the shortest codons (based on hamming distance)
#
variants_parser=function(input_df,totalcells=1){
  intended_codons=read.csv("data/codon_table.csv")
  intended_codons=intended_codons%>%filter(Twist%in%T)

  input_df=input_df%>%filter(alt_codon%in%intended_codons$Codon)
  input_df=input_df%>%
    rowwise()%>%
    mutate(alt_codon_shortest=shortest_codon_finder(ref_codon,alt_aa)[[1]][1],
           n_nuc_min=shortest_codon_finder(ref_codon,alt_aa)[[2]][1])

  input_df=input_df%>%mutate(maf=ct/depth)
  input_df$totalcells=totalcells
  input_df=input_df%>%mutate(totalmutant=maf*totalcells)
  input_df_simple=input_df%>%dplyr::select(alt_start_pos,protein_start,ref,alt,ref_aa,alt_aa,ref_codon,alt_codon,alt_codon_shortest,n_nuc_min,consequence_terms,ct,depth,maf,totalcells,totalmutant)
  input_df_simple
}
