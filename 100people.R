#############################################################################################
###### If Louisville were 100 People project
###### 2014 1-year pums
###### Louisville MSA; KY-IN

## load packages
library(dplyr)
library(survey)

## read in combined Kentucky and Indiana pums file
kentucky <- read.csv("~/../Desktop/pumsKy.csv")
indiana <- read.csv("~/../Desktop/pumsIn.csv")

##Combine data sets
kyIn <- rbind(kentucky, indiana)

## vector to approximate Louisville MSA with full PUMA (State + Puma Code)
pums14 <- c(2101701, 2101702, 2101703, 2101704, 2101705, 2101706, 2101600, 2101800, 1803400, 1803300)

sixteen <- (kyIn %>%
                    filter(FPUMA %in% pums14) %>% ## filter to Louisville MSA (approximate)
                    filter(AGEP >= 16) %>% ## filter to pop 16 and over
                    select(FPUMA, ESR, AGEP, PWGTP))


##Recode ESR data
sixteen <- (sixteen %>%
                    mutate(Emp = ifelse(ESR == 1|ESR == 2 |ESR == 3 | ESR == 4|ESR == 5, 1, 
                                        ifelse(ESR == 6, 2, 3)
                    )))
##filter for labor force
##calulcate number in labor force, not in labor force
x <- count(sixteen, Emp, wt= PWGTP, sort = FALSE)

##Percentage in labor force, not in labor force 

x %>% 
        mutate(freq=n/sum(n))

########### Employed
########### Earning more than x
########### Educational Attainment <- less than a bachelors
########### Working in Manufacturing (take away all)
########### Female (take away 3/4)
########### Disablity
########### Age
########### English
















##Using filter for those in labor force
##Calculate percent employed

##Using percent filter for those in labor force
##