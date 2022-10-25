#Barcode distribution plotter
#10.8.22
barcodes=read.table("data/Consensus_Data/Novogene_lane14/sample14_combined/sscs.barcodes.lane14.s1.tsv")
names(barcodes)="ct"
barcodes$sample="sample1"
hist(barcodes$ct,breaks=30)
#This script takes in a single or duplex sequencing fasta file and plots the distribution of barcodes
barcodes.1=read.table("data/Consensus_Data/Novogene_lane14/sample14_combined/sscs.barcodes.lane14.s1.tsv")
barcodes.14=read.table("data/Consensus_Data/Novogene_lane14/sample14_combined/sscs.barcodes.lane14.s14.tsv")
names(barcodes.1)="ct"
barcodes.1$sample="sample1"
names(barcodes.14)="ct"
barcodes.14$sample="sample14"
barcodes=rbind(barcodes.1,barcodes.14)
# barcodes$V1=as.numeric(barcodes$V1)
# hist(barcodes.14$ct,breaks=30)
ggplot(barcodes,aes(x=ct,color=sample))+geom_density(adjust=3)
ggplot(barcodes.14,aes(x=ct))+geom_histogram(bins=50)
median(barcodes.14$ct)
ggplot(barcodes.1,aes(x=ct))+geom_histogram(bins=50)+scale_x_continuous(limits=c(0,200))
ggplot(barcodes.1,aes(x=ct))+geom_histogram(bins=50)
# ggplot(barcodes,aes(x=ct))+geom_density(adjust=3)
