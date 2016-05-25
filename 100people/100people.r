####################################################################################
###### If Louisville were 100 People project
###### 2014 1-year pums
###### Louisville MSA; KY-IN
####################################################################################

############################ LOAD PACKAGES #####################################
################################################################################

library(dplyr)

############################ LOAD DATA #########################################
################################################################################

# read in combined Kentucky and Indiana pums file
<<<<<<< HEAD:100people.R
kentucky <- read.csv("~/../Desktop/rProjects/100people/pumsKy.csv")
indiana <- read.csv("~/../Desktop/rProjects/100people/pumsIn.csv")
=======
kentucky <- read.csv("~/../Desktop/pumsKy.csv")
indiana <- read.csv("~/../Desktop/pumsIn.csv")
>>>>>>> 08c8e3d07508baed09db4d1f55cf1d1de19fd1cc:100people/100people.r

##Combine data sets
kyIn <- rbind(kentucky, indiana)

############################ FUNCTIONS #########################################
################################################################################

##Percentage of total function for n
percent <- function(data){
        data <-  data %>% mutate(per=n/sum(n))
        print(data)
}

##countWeight <- function(y, z){
##   count(y, z, wt = PWGTP, sort = FALSE)
##}


############################ MAIN DATA FILTER ##################################
################################################################################

## vector to approximate Louisville MSA with full PUMA (State + Puma Code, excludes Washington, IN)
pums14 <- c(2101701, 2101702, 2101703, 2101704, 2101705, 2101706, 2101600, 2101800, 1803400, 1803300)

workforce <- (kyIn %>%
                      filter(FPUMA %in% pums14) %>% ## filter to Louisville MSA (approximate)
                      filter(AGEP >= 16) %>% ## filter to pop 16 and over
                      select(FPUMA, ESR, AGEP, PWGTP, WAGP, SCHL, MANU)) ## Select variables of interest 

####################################################################################
############################ 100 PEOPLE COUNTDOWN BEGINS ###########################
####################################################################################

############################ Labor Force ###########################

laborforce <- (workforce %>%
                       mutate(Emp =  ifelse(ESR == 1|ESR == 2 |ESR == 3, "In Labor Force", 
                                            ifelse(ESR == 6, "Not in Labor Force", 3)))%>% 
                       filter(Emp != 3))

x <- count(laborforce, Emp, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(x)


############################ Employed ##############################
## in Labor Force - Employed 
employed <- (workforce %>%
                     mutate(Emp = ifelse(ESR == 1|ESR == 2, "Employed", 
                                         ifelse(ESR == 3, "Unemployed", 3)))%>%
                     filter(Emp != 3))

e <- count(employed, Emp, wt=PWGTP, sort = FALSE)
percent(e)


############################ Employed Earning more than $28,974 ##############################
##Note*  $28,974 represents (25th pct wage production + install,maint,repair occupations), simulating 
##wage for entry level jobs in specific areas of manufacturing 
## employed earning less than x 

earning <- (workforce %>%
                    mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                        ifelse(ESR == 3, 2, 3)  ))%>%
                    filter(Emp < 2) %>%
                    mutate(Earn = ifelse(WAGP > 28974.4, "Earns more", "Earns less than or equal to $28,974")))

earns <- count(earning, Earn, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(earns)


############################ EDUCATIONAL ATTAINMENT ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  with less than a bachlor's degree

education <- (workforce %>%
                      mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                          ifelse(ESR == 3, 2, 3))) %>%
                      filter(Emp < 3) %>%
                      mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                      mutate(Edu = ifelse(SCHL >= 21, "Bachelors or above", "Less than Bachelors"))%>%
                      filter(Earn>1))


ed <- count(education, Edu, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(ed)


############################ MANUFACTURING ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  
##with less than a bachlor's degree, not working in manufacturing

manufacturing <- (workforce %>%
                          mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                              ifelse(ESR == 3, 2, 3)))%>%
                          filter(Emp < 3) %>%
                          mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                          mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
                          filter(Earn>1)%>%
                          mutate(manu = ifelse(MANU == 3, "In Manufacturing", "Not in Manufacturing" )))

Manufa <- count(manufacturing, manu, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(Manufa)