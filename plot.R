# load colors for per gene alleles
colors <- read.table('colors.txt',fill=T,header=T,stringsAsFactors=F)


# load plotting lib
library(ggplot2)

# know of 4 files each describing variation across alleles of a gene
for(gene in c('MAOA','CompositeRecoded','COMT')) { #'DAT1',
   cat(gene,"\n")
 # get this gene's color spectrum from preloaded text file
 # remove empty -- overengeerning? ggplot will only ask for as many colors as
 #                                 there are factors so empty values wouldn't be a problem?
 color     <- colors[,gene]
 noncolors <-  which(color=="")
 if(length(noncolors)>0) color <- color[-noncolors]


 # read in table for each gene type
 table <- read.table( paste('data/',gene,'_AgeInverseCentered_RestingState.csv',sep=""),   sep=",", header=1, row.names=1)

 # set names
 # levels(table$Subcortical)
 #[1] "Accum"   "Caudate" "Putamen"
 levels(table$Subcortical)<-c("Nucleus Accumbens", "Caudate", "Putanmen" )

 # > levels(table$PFC)
 # [1] "Dorsal_ACC"           "InferiorFrontalGyrus" "Lateral_OFC"          "Medial_OFC"           "MiddleFrontalGyrus" 
 # [6] "SuperiorFrontalGyrus" "Ventral_ACC" 
 levels(table$PFC)<-c("dACC","IFG", "Lateral OFC",
                      "Medial OFC", "MFG", "SFC", "vACC")

 # how does the table call this gene (ie. append .Genotype)
 genename <- paste( gene, '.Genotype' , sep="")

 # set genotype as a factor
 table[,genename] <- as.factor(table[,genename])
 names(table)[3] <- gene
 # this gets a bit hairy
 # "gene" at the for loop level is "COMT", "DAT1", etc
 # the "gene" as dataframe column name (eg table$COMT) is the gene type (e.g. Va/Met)
 if(gene == 'COMT' ) {
   levels(table[,gene])<-c("Val/Val" ,"Val/Met", "Met/Met")
 } else if(gene == 'MOAO'){
   levels(table[,gene])<-c("4R/4R" ,"3R/4R", "3R/3R")
 } else if(gene == 'DAT1'){
   levels(table[,gene])<-c("10R/10R" ,"9R")
 }else{ #COMPOSITE
   levels(table[,gene])<-c("0.5" ,"1.0","1.5","2.0","2.5","3.0")
 }

 #levels(table[,genename]) <- c(1,2,3) # set the level names to something useful

 # plot
 p <- ggplot(data=table,
       aes_string(x="1/(AgeInverseCentered+1/15.36)",y="reducedCorrPercentileContiguous", group=gene,color=gene)
     ) + theme_bw()
 p <- p + ggtitle(gene) +
  geom_line()+                          # add a line
  facet_grid(PFC ~  Subcortical)+       # break up by PFC+Subcortical comparison
  scale_color_manual(values=color)+     # set the desired colors
  theme(text=element_text(size=8))+     # reduce font size everywhere to 8
  scale_y_continuous("Correlation\n(Fisher's z)")+ # y axis title
  scale_x_continuous('Age (Years)',            # x axis title
                     breaks=c(10,15,20))  # only show 3 numbers on the x axis

 # put plot into pdf
 # TODO:
 #   * maybe put all plots in one pdf -- means moving the pdf() call to before the for loop and the dev.off() call to after
 #   * fiddle with pdf size so axes are readable (width,height)
 pdf(paste('imgs/',gene,'_facet.pdf',sep=""),width=11,height=8)#, width=600,height=400)
 print(p)
 dev.off()

 table$PFCSubCombo <- sprintf("%s - %s",table$PFC,table$Subcortical)
 for (PFCSubCombo in unique(table$PFCSubCombo)) {
   cat("\t",PFCSubCombo,"\n")
   subtable <- table[table$PFCSubCombo==PFCSubCombo,]

   pbase <- ggplot(data=subtable,
         aes_string(x="1/(AgeInverseCentered+1/15.36)",y="reducedCorrPercentileContiguous", group=gene,color=gene)
        ) + theme_bw()
   # single plot for each
   p <- pbase + ggtitle(sprintf("%s: %s",gene, PFCSubCombo)) +
    geom_line()+                          # add a line
    scale_color_manual(values=color)+     # set the desired colors
    theme(text=element_text(size=8))+     # reduce font size everywhere to 8
    scale_y_continuous("Correlation\n(Fisher's z)")+ # y axis title
    scale_x_continuous('Age',            # x axis title
                       breaks=c(10,15,20))  # only show 3 numbers on the x axis
   imgsavedir <- file.path('imgs','idv',gene);
   dir.create(imgsavedir,recursive=T)
   pdf(paste(imgsavedir,'/',PFCSubCombo,'.pdf',sep=""),width=11,height=8)#, width=600,height=400)
   print(p)
   dev.off()
 }
 
}
