#ER
library(vcd)
library(RColorBrewer)
library(readxl)

data_frame<-readxl::read_excel(here::here("v1/ERforBoxplot(no blanks)07102024.xlsx"))
df<-table(data_frame$`cohort`, data_frame$`CategoriesAdd7102024`)
M=as.table(as.matrix(df[,1:3]))
names(dimnames(M))=c("cohort","CategoriesAdd7102024")
labs<-round(prop.table(M,margin=1), 2)
vcd::mosaic(M,pop=FALSE, gp_varnames=grid::gpar(fontsize=16),
       gp_labels=grid::gpar(fontsize=16))
vcd::labeling_cells(text=labs,margin=0)(M)

barplot(t(labs), col=RColorBrewer::brewer.pal(3,"Set1"), cex.names = 1.28, cex.axis = 1.28)
legend("bottomright",legend=colnames(labs), inset=c(0,1.1),xpd=TRUE,fill=RColorBrewer::brewer.pal(3,"Set1"), horiz=TRUE, cex=1.1)

#PR
library(vcd)
library(RColorBrewer)
library(readxl)

data_frame<-(read_excel(here::here("v1/PRforBoxplot(no blanks)07112024.xlsx")))
df<-table(data_frame$`cohort`, data_frame$`CategoriesAdd08222024`)
M=as.table(as.matrix(df[,1:2]))
names(dimnames(M))=c("cohort","CategoriesAdd08222024")
labs<-round(prop.table(M,margin=1), 2)
mosaic(M,pop=FALSE)
labeling_cells(text=labs,margin=0)(M)

barplot(t(labs), col=brewer.pal(3,"Set1"), cex.names = 1.28, cex.axis = 1.28)
legend("bottomright",legend=colnames(labs), inset=c(0.21,1.1),xpd=TRUE,fill=brewer.pal(3,"Set1"), horiz=TRUE, cex=1.1)

#HER2
data_frame<-(read_excel(here::here("v1/HER2forBarChart(no blanks)07112024.xlsx")))
df<-table(data_frame$`cohort`, data_frame$`Category`)
M=as.table(as.matrix(df[,1:3]))
names(dimnames(M))=c("cohort", "Category")
labs<-round(prop.table(M,margin=1), 2)
mosaic(M,pop=FALSE)
labeling_cells(text=labs,margin=0)(M)

barplot(t(labs), col=brewer.pal(3,"Set1"), cex.names = 1.28, cex.axis = 1.28)
legend("bottomright",legend=colnames(labs),inset=c(0.1,1.1),
       xpd=TRUE, fill=brewer.pal(3,"Set1"), horiz=TRUE, cex=1.1)


#HRHER2
data_frame<-(read_excel(here::here("v1/HRHER2forR(no blanks) MR 09102024.xlsx")))
df<-table(data_frame$`cohort`, data_frame$`HRHER2_MR`)
M=as.table(as.matrix(df[,1:4]))
names(dimnames(M))=c("cohort","HRHER2_MR")
labs<-round(prop.table(M,margin=1), 2)
mosaic(M,pop=FALSE)
labeling_cells(text=labs,margin=0)(M)

barplot(t(labs), col=brewer.pal(4,"Set1"),cex.names = 1.28, cex.axis = 1.28)
legend("bottomright",legend=colnames(labs), inset=c(0.04,1.1),xpd=TRUE,fill=brewer.pal(4,"Set1"), horiz=TRUE, cex=1.1)


#Ki67
library(readxl)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)

ki67<-read_excel(here::here("v1/Ki67forBoxplot(no blanks)07112024.xlsx"))
ggplot(ki67, aes(x=cohort,y=PPN_BC_Tumor,fill=cohort))+geom_boxplot(show.legend = FALSE)+
  labs(x="",y="Ki-67 Percent Positive Nuclei") +
  scale_fill_brewer(palette="Set1") + stat_compare_means() + theme(text=element_text(size=16, face="bold",color="#000000"),
                                                                   axis.text=element_text(color="#000000"))


#PAM50
library(vcd)
library(RColorBrewer)
library(readxl)

data_frame<-(read_excel(here::here("v1/PAM5007122024.xlsx")))
df<-table(data_frame$`cohort2`, data_frame$`subtype2`)
M=as.table(as.matrix(df[,1:5]))
names(dimnames(M))=c("cohort2", "subtype2")
labs<-round(prop.table(M,margin=1), 2)
mosaic(M,pop=FALSE)
labeling_cells(text=labs,margin=0)(M)

barplot(t(labs), col=brewer.pal(5,"Set1"), cex.names = 1.28, cex.axis = 1.28)
legend("bottom",legend=colnames(labs),inset=c(0.5,1.1),
       xpd=TRUE, fill=brewer.pal(5,"Set1"), horiz=TRUE, cex=1.1)
