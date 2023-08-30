depth_finder=function(input_df,colname){
  #This function returns the mean depth at a desired residue
  #Mean depth is the average of the bwa-mem depth of all mutants seen at that residue
  # input_df=il3D0.D2
  # resi=c(242:250)
  # colname="depth.y"
  input_df$coverage2=input_df[[colname]]
  depths=input_df%>%
    group_by(protein_start)%>%
    summarize(depth_mean=mean(coverage2,na.rm=T))%>%
    filter(!protein_start%in%NA)
  # protein_start=242
  # depths[depths$protein_start==protein_start,"depth_mean"][[1]]

  input_df=input_df%>%
    rowwise()%>%
    mutate(coverage2=case_when(coverage2%in%NA~
                                 depths[depths$protein_start==protein_start,"depth_mean"][[1]],
                                                 T~coverage2))

  input_df[,eval(colname)]=input_df$coverage2
  input_df%>%dplyr::select(-coverage2)
}

