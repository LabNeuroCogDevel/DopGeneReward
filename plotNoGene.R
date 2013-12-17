# load colors for per gene alleles
colors <- read.table('colors.txt',fill=T,header=T,stringsAsFactors=F)


# load plotting lib
library(ggplot2)
table <- read.table('data/AgeInverse_lmerCellMeans_reducedCorrPercentileContiguous.csv' ,   sep=",", header=1, row.names=1)


 # set names
 # levels(table$Subcortical)
 #[1] "Accum"   "Caudate" "Putamen"
 levels(table$Subcortical)<-c("Nucleus Accumbens", "Caudate", "Putamen" )

 # > levels(table$PFC)
 # [1] "Dorsal_ACC"           "InferiorFrontalGyrus" "Lateral_OFC"          "Medial_OFC"           "MiddleFrontalGyrus" 
 # [6] "SuperiorFrontalGyrus" "Ventral_ACC" 
 levels(table$PFC)<-c("dACC","IFG", "Lateral OFC",
                      "Medial OFC", "MFG", "SFG", "vACC")


 # plot
 p <- ggplot(data=table,
       aes_string(x="1/(AgeInverseCentered+1/15.36)",y="reducedCorrPercentileContiguous") 
     ) + theme_bw()
 p <- p + ggtitle('Age Effects') +
  geom_line()+                          # add a line
  facet_grid(PFC ~  Subcortical)+       # break up by PFC+Subcortical comparison
  scale_color_manual(values=color)+     # set the desired colors
  theme(text=element_text(size=8))+     # reduce font size everywhere to 8
  scale_y_continuous("Correlation\n(Fisher's z)",  # y axis title
                     breaks=c(.1,.2,.3,.4))+
  scale_x_continuous('Age (Years)',            # x axis title
                     breaks=c(10,15,20))  # only show 3 numbers on the x axis

 # put plot into pdf
 # TODO:
 #   * maybe put all plots in one pdf -- means moving the pdf() call to before the for loop and the dev.off() call to after
 #   * fiddle with pdf size so axes are readable (width,height)
 pdf(paste('imgs/Age_nogene_facet.pdf',sep=""),width=11,height=8)#, width=600,height=400)
 print(p)
 dev.off()

 table$PFCSubCombo <- sprintf("%s - %s",table$PFC,table$Subcortical)
 for (PFCSubCombo in unique(table$PFCSubCombo)) {
   cat("\t",PFCSubCombo,"\n")
   subtable <- table[table$PFCSubCombo==PFCSubCombo,]

   pbase <- ggplot(data=subtable,
         aes_string(x="1/(AgeInverseCentered+1/15.36)",y="reducedCorrPercentileContiguous")
        ) + theme_bw()
   # single plot for each
   p <- pbase + ggtitle(PFCSubCombo) +
    geom_line()+                          # add a line
    scale_color_manual(values=color)+     # set the desired colors
    theme(text=element_text(size=8))+     # reduce font size everywhere to 8
    scale_y_continuous("Correlation\n(Fisher's z)")+ # y axis title
    scale_x_continuous('Age',            # x axis title
                       breaks=c(10,15,20))  # only show 3 numbers on the x axis
   imgsavedir <- file.path('imgs','idv','withoutgenes');
   dir.create(imgsavedir,recursive=T)
   pdf(paste(imgsavedir,'/',PFCSubCombo,'.pdf',sep=""),width=11,height=8)#, width=600,height=400)
   print(p)
   dev.off()
 }
 
