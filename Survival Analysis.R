##Importing the data


library(readxl)
library(dplyr)
library(tidyr)
library(survival)
cancer <- read_excel("E:\\Study\\R Projects\\Survival/Cancer_data.xlsx")

tbl_df(cancer)
glimpse(cancer)

##Data manipulation

sum(is.na(cancer))

red_data <- cancer[,c("Survival_days","Status","Prior_therapy")]
tbl_df(red_data)

##K-M model

attach(red_data)
KM <- survfit(Surv(Survival_days,Status)~Prior_therapy)
summary(KM)

##Plot
plot(KM)


