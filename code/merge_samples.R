merge_samples=function(inputx,inputy){
  #This function takes variant from two samples, merges them, and adds counts and allele frequencies
  # inputx="Novogene_lane11/sample1" #Input the directory names for the sequencing Lane and the sample name
  # inputy="Novogene_lane11/sample2" #Input the directory names for the sequencing Lane and the sample name
  samplex=inputx
  if(class(inputx)=="character"){ #aka saying that if inputx is not already present as a dataframe
    samplex=read.csv(paste("data/Consensus_Data/",inputx,"/variant_caller_outputs/variants_unique_ann.csv",sep=""),header=T,stringsAsFactors = F)
  }

  sampley=read.csv(paste("data/Consensus_Data/",inputy,"/variant_caller_outputs/variants_unique_ann.csv",sep=""),header=T,stringsAsFactors = F)
  samples_xy=merge(samplex,sampley,by=c("type",
                                        "alt_start_pos",
                                        "alt_end_pos",
                                        "ref",
                                        "alt",
                                        "protein_start",
                                        "protein_end",
                                        "amino_acids",
                                        "codons",
                                        "impact",
                                        "polyphen_prediction",
                                        "consequence_terms"),all=T)

  samples_xy[samples_xy$ct.x%in%NA,"ct.x"]=0
  samples_xy[samples_xy$ct.y%in%NA,"ct.y"]=0
  samples_xy[samples_xy$depth.x%in%NA,"depth.x"]=0
  samples_xy[samples_xy$depth.y%in%NA,"depth.y"]=0
  samples_xy=samples_xy%>%
    mutate(ct=ct.x+ct.y,depth=depth.x+depth.y)%>%
    dplyr::select("type",
                  "alt_start_pos",
                  "alt_end_pos",
                  "ref",
                  "alt",
                  "ct",
                  "depth",
                  "protein_start",
                  "protein_end",
                  "amino_acids",
                  "codons",
                  "impact",
                  "polyphen_prediction",
                  "consequence_terms")

  samples_xy
}
