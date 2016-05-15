TechJobs <- read.csv("~/Desktop/techjobs5.13.16.csv",header = TRUE)
install.packages("DT")
install.packages("readr")

library(DT)
library(readr)

louisvillejobs <- read_csv('~/Desktop/techjobs5.13.16.csv')
louisvillejobs %>% datatable()