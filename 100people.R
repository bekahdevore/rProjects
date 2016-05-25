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
kentucky <- read.csv("~/../Desktop/pumsKy.csv")
indiana <- read.csv("~/../Desktop/pumsIn.csv")

##Combine data sets
kyIn <- rbind(kentucky, indiana)

############################ FUNCTIONS #########################################
################################################################################

##Percentage "n" in df  
percent <- function(data){
        data <-  data %>% mutate(per=n/sum(n))
        print(data)
}

############################ MAIN DATA FILTER ##################################
################################################################################

## vector to approximate Louisville MSA with full PUMA (State + Puma Code, excludes Washington, IN)
pums14 <- c(2101701, 2101702, 2101703, 2101704, 2101705, 2101706, 2101600, 2101800, 1803400, 1803300)

workforce <- (kyIn %>%
                      filter(FPUMA %in% pums14) %>% ## filter to Louisville MSA (approximate)
                      filter(AGEP >= 16) %>% ## filter to pop 16 and over
                      select(FPUMA, ESR, AGEP, PWGTP, WAGP, SCHL)) ## Select variables of interest 

####################################################################################
############################ 100 PEOPLE COUNTDOWN BEGINS ###########################
####################################################################################

############################ Labor Force ###########################
##filter for labor force
##Recode ESR data
laborforce <- (workforce %>%
                       mutate(Emp = ifelse(ESR == 1|ESR == 2 |ESR == 3, 1, #In Labor Force = 1
                                           ifelse(ESR == 6, 2, 3) #Not in Labor Force =2, other= 3
                       ))%>%
                       filter(Emp < 3)
)

##calulcate number then percent in labor force, not in labor force
x <- count(laborforce, Emp, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(x)
print("1 = in Labor Force; 2 = Not in Labor Force")


############################ Employed ##############################
##of Labor Force - Employed 
employed <- (workforce %>%
                     mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                         ifelse(ESR == 3, 2, 3)
                     ))%>%
                     filter(Emp < 3)
)
e <- count(employed, Emp, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(e)
print("1 = Employed; 2 = Unemployed")


############################ Employed Earning more than $28,974 ##############################
##Note*  $28,974 represents (25th pct wage production + install,maint,repair occupations), simulating 
##wage for entry level jobs in specific areas of manufacturing 

earning <- (workforce %>%
                    mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                        ifelse(ESR == 3, 2, 3)
                    ))%>%
                    filter(Emp < 2) %>%
                    mutate(Earn = ifelse(WAGP > 28974.4, 1, 2) )
)
earns <- count(earning, Earn, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(earns)
print("1 = earns more than $28,974; 2 = earns <= $28,974")

############################ Educational Attainment ##############################
## less than a bachelors of employed and unemployed making less than $28,974

education <- (workforce %>%
                      mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                          ifelse(ESR == 3, 2, 3)
                      ))%>%
                      filter(Emp < 3) %>%
                      mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                      mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
                      filter(Earn>1)
)

ed <- count(education, Edu, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(ed)
print("1 = bachelor's degree or above ; 2 = less than bachelor's degree")

############################ Working in Manufacturing ##############################
## less than a bachelors of employed and unemployed
########### Working in Manufacturing (take away all)

########### Female (take away 3/4)
########### Disablity
########### Age
########### English
















##Using filter for those in labor force
##Calculate percent employed

##Using percent filter for those in labor force
##