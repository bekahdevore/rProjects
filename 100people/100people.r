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
kentucky <- read.csv("~/../Desktop/rProjects/100people/pumsKy.csv")
indiana <- read.csv("~/../Desktop/rProjects/100people/pumsIn.csv")

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
                      select(FPUMA, ESR, AGEP, PWGTP, WAGP, SCHL, MANU, SEX, AGEP, DIS, ENG)) ## Select variables of interest 
########### Age                    
age <- (workforce %>%
                mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                    ifelse(ESR == 3, 2, 3)))%>%
                filter(Emp < 3) %>%
                mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
                filter(Earn>1)%>%
                mutate(manu = ifelse(MANU == 3, "In Manufacturing", "Not in Manufacturing"))%>%
                filter(MANU != 3) %>% #keep participants not already working in manufacturing
                mutate(a = ifelse(AGEP >= 55, "55 +", "Under 55")))



ageCount <- count(age, a, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(ageCount) 



########### Disablity

disability <- (workforce %>%
                       mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                           ifelse(ESR == 3, 2, 3)))%>%
                       filter(Emp < 3) %>%
                       mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                       mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
                       filter(Earn>1)%>%
                       filter(MANU != 3) %>% #keep participants not already working in manufacturing
                       filter(AGEP < 55)%>% #Keep participants under 55
                       mutate(d = ifelse(DIS == 1, "With a disablity", "Without a disability")))

dis <- count(disability, d, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(dis)


########### English


english <- (workforce %>%
                    mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                        ifelse(ESR == 3, 2, 3)))%>%
                    filter(Emp < 3) %>%
                    mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
                    mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
                    filter(Earn>1)%>%
                    filter(MANU != 3) %>% #keep participants not already working in manufacturing
                    filter(AGEP < 55)%>% #Keep participants under 55
                    filter(DIS == 2)%>%
                    filter(ENG >=1 )%>%
                    mutate(e = ifelse(ENG == 1 | ENG == 2, "Speaks enlish well",
                                      ifelse(ENG == 3 | ENG == 4, "Does not speak english well", 3 ))))

eng <- count(english, e, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(eng)

############################ FEMALE ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  
##with less than a bachlor's degree, not working in manufacturing,
## + percent male/female 

sx <- (workforce %>%
               mutate(Emp = ifelse(ESR == 1|ESR == 2, 1, 
                                   ifelse(ESR == 3, 2, 3)))%>%
               filter(Emp < 3) %>%
               mutate(Earn = ifelse(WAGP > 28974.4, 1, 2))%>%
               mutate(Edu = ifelse(SCHL >= 21, 1, 2))%>%
               filter(Earn>1)%>%
               filter(MANU != 3) %>% #keep participants not already working in manufacturing
               filter(DIS == 2)%>%
               filter(ENG >=3)%>%
               mutate(s = ifelse(SEX == 1, "Male", 
                                 ifelse(SEX == 2, "Female", "NA")))
)



sex <- count(sx, s, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(sex)

########### Female (take away 3/4)
t = sex[1,2]/4 ##1/4 of females
print(t)
