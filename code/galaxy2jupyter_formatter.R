galaxy2jupyter_formatter=function(dirname){
  #This function just takes the output of the nvc processed files from
  # the galaxy function and reformats it a little.
  # Mostly takes out the columns for A,C,G,T followed by the abundance
  # and melts it into a single REF and ALT allele format.
  library(dplyr)
  # dirname="/Users/haiderinam/OneDrive - The Pennsylvania State University/RProjects/duplex_sequencing_screen/data/Dunovo/Novogene_lane4/practice"
  files=list.files(dirname)
  # files=list.files("../data/Dunovo/ToDo/")
  for(i in 1:length(files)){
    # i=1
    filename=paste(dirname,"/",files[i],sep = "")
    variants_raw=read.table(filename,header = F,stringsAsFactors = F)
    names(variants_raw)=scan(filename,what="",n=13,sep="\t")
    # variants_raw=read.csv(filename,
    #                       header=T,
    #                       stringsAsFactors = F,
    #                       check.names = F)
    variants_raw=variants_raw[,-1]
    variants_raw=variants_raw%>%filter(POS>=130862000,
                                       POS<=130884200,
                                       CVRG>=2000,
                                       !MINOR%in%".")
    # ggplot(variants_raw,aes(x=MAF))+geom_histogram()

    variants_raw=variants_raw%>%dplyr::select(!c("MINOR","MAF"))
    variants_updated=melt(variants_raw,
                          id.vars = c("CHR","POS","CVRG","ALLELES","MAJOR","BIAS"),
                          measure.vars = c("A","C","G","T"),
                          variable.name = "MINOR",
                          value.name = "Nucleotide_Ct")
    variants_updated=variants_updated%>%mutate(MAF=Nucleotide_Ct/CVRG)%>%
      filter(!Nucleotide_Ct%in%0,!MAF>=.9,!MAJOR==MINOR)
    variants_updated=variants_updated%>%dplyr::select(c(CHR,POS,CVRG,ALLELES,MAJOR,MINOR,MAF,Nucleotide_Ct,BIAS))
    # head(variants_updated)
    variants_updated$REF=variants_updated$MAJOR
    variants_updated$ALT=variants_updated$MINOR
    # write.csv(variants_updated,gsub(x=(files[i]),pattern = ".csv",replacement="_routput.csv"))
    write.csv(variants_updated,paste(dirname,"/",gsub(x=(files[i]),pattern = ".tabular",replacement="_routput.csv"),sep=""))
  }
}
