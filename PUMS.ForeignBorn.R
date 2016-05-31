#############################################################################################
###### cross tab employment status by educational attainment
###### 2014 1-year pums
###### Louisville MSA; KY-IN

## load packages
library(dplyr)


library(survey)

## read in combined Kentucky and Indiana pums file
kentucky <- read.csv("path/file.csv")
indiana <- read.csv("path/file.csv")

##Combine data sets
kyIn <- rbind(kentucky, indiana)

## vector to approximate Louisville MSA with full PUMA (State + Puma Code)
MSA <- c(2101701, 2101702, 2101703, 2101704, 2101705, 2101706, 2101600, 2101800, 1803400, 1803300)

(pums14 %>%
        filter(FPUMA %in% MSA) %>% ## filter to Louisville MSA (approximate)
        filter(AGEP >= 25) %>% ## filter to pop 25 and over
        filter(NATIVITY>1) %>% ## filter to foreign born
        select(SERIALNO, NATIVITY, NOP, OCCP, POBP, WAGP, SEMP, SOCP, SPORDER, FPUMA, PWGTP, AGEP, ESR, SCHL) ## keep needed variables only
) -> data

summary(data) ## check to see if filters worked

## recode educational attainment values
(data %>%
        mutate(ED = ifelse(SCHL <= 15, 1, 
                           ifelse(SCHL == 16 | SCHL == 17, 2, 
                                  ifelse(SCHL == 18 | SCHL == 19, 3,
                                         ifelse(SCHL == 20, 4,
                                                ifelse(SCHL == 21, 5,
                                                       ifelse(SCHL == 22, 6, 
                                                              ifelse(SCHL == 23, 7, 8))))))))
) -> data

## recode employment status values
(data %>%
        mutate(EMP = ifelse(ESR == 1 | ESR == 2, 1,
                            ifelse(ESR == 3, 2,
                                   ifelse(ESR == 4 | ESR == 5, 3, 4))))
) -> data

summary(data) ## check recodes

## create survey design
design <- svydesign(id=c(data$SERIALNO, data$SPORDER), weights=data$PWGTP, data=data)
## simple crosstab
svytable(~EMP+ED, design)
plot(svytable(~EMP+ED, design))
