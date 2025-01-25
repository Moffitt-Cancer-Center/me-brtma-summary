#ER
library(readxl)

data_frame<-read_excel(here::here("v1/ERforBoxplot(no blanks)07102024.xlsx"))
table(data_frame$`cohort`, data_frame$`CategoriesAdd7102024`)

fisher.test(data_frame$`cohort`, data_frame$`CategoriesAdd7102024`)

#PR
library(readxl)

data_frame<-read_excel(here::here("v1/PRforBoxplot(no blanks)07112024.xlsx"))
table(data_frame$`cohort`, data_frame$`CategoriesAdd08222024`)

chisq.test(data_frame$`cohort`, data_frame$`CategoriesAdd08222024`)

#HER2
library(readxl)

data_frame<-read_excel(here::here("v1/HER2forBarChart(no blanks)07112024.xlsx"))

table(data_frame$cohort,data_frame$`Category`)

chisq.test(data_frame$cohort,data_frame$`Category`, correct=FALSE)

#HR/HER2
library(readxl)
data_frame<-(read_excel(here::here("v1/HRHER2forR(no blanks) MR 09102024.xlsx")))
ft<-table(data_frame$`cohort`,data_frame$`HRHER2_MR`)
table(data_frame$`cohort`,data_frame$`HRHER2_MR`)

fisher.test(ft, workspace=2e6)

#Ki-67 (included in other file)

#PAM50
