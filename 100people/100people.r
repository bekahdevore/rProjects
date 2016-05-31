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

#countWeight <- function(y, t){
#  count(y, t, wt = PWGTP, sort = FALSE)
#   }



############################ MAIN DATA FILTER ##################################
################################################################################

## vector to approximate Louisville MSA with full PUMA (State + Puma Code, excludes Washington, IN)
pums14 <- c(2101701, 2101702, 2101703, 2101704, 2101705, 2101800, 1803400, 1803300)

workforce <- (kyIn %>%
                      filter(FPUMA %in% pums14) %>% ## filter to Louisville MSA (approximate)
                      filter(AGEP >= 16) %>% ## filter to pop 16 and over
                      select(FPUMA, ESR, AGEP, PWGTP, WAGP, SCHL, SCHG, WKL, JWTR, MANU, SEX, AGEP, DIS, ENG, NATIVITY)) ## Select variables of interest 

dd <- is.na(workforce)
workforce[dd] <- 0 ##change NA to 0, move to top
####################################################################################
############################ 100 PEOPLE COUNTDOWN BEGINS ###########################
####################################################################################

startingPopulation <- count(workforce, wt=PWGTP)  
print(startingPopulation)

############################ Labor Force ###########################

laborforce <- (workforce %>%
                       mutate(Emp =  ifelse(ESR == 1|ESR == 2 |ESR == 3, "In Labor Force", 
                                            ifelse(ESR == 6, "Not in Labor Force", 3)))%>% 
                       filter(Emp != 3)) 


x <- count(laborforce, Emp, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(x)



############################ OVER 18 ###########################       

over18 <- (laborforce %>% 
                   filter(Emp == "In Labor Force")%>%
                   mutate(young = ifelse(AGEP == 16 | AGEP == 17, "Under 18", 
                                         ifelse(AGEP > 17, "Over 18", 3))))


x <- count(over18, young, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(x)



############################ Employed ##############################
## in Labor Force - Employed 
employed <- (over18 %>%
                     filter(young == "Over 18")%>%
                     mutate(Emp = ifelse(ESR == 1|ESR == 2, "Employed", 
                                         ifelse(ESR == 3, "Unemployed", 3)))%>%
                     filter(Emp != 3))


e <- count(employed, Emp, wt=PWGTP, sort = FALSE)
percent(e)


############################ Employed Earning more than $28,974 ##############################
##Note*  $28,974 represents (25th pct wage production + install,maint,repair occupations), simulating 
##wage for entry level jobs in specific areas of manufacturing 
## employed earning less than x 

earning <- (employed %>%
                    
                    mutate(Earn = ifelse(WAGP > 28974.4, "Earns more", "Earns less than or equal to $28,974")))

earns <- count(earning, Earn, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(earns)


############################ EDUCATIONAL ATTAINMENT ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  with less than a bachlor's degree

education <- (earning %>%
                      
                      mutate(Edu = ifelse(SCHL >= 21, "Bachelors or above", "Less than Bachelors"))%>%
                      filter(Earn == "Earns less than or equal to $28,974"))

ed <- count(education, Edu, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(ed)


dd <- is.na(education$SCHG)
education$SCHG[dd] <- 0 ##change NA to 0, move to top

###########IN SCHOOL  
inSchool <- (education %>%
                     filter(Edu == "Less than Bachelors")%>%
                     mutate(school = ifelse(SCHG == 13 | SCHG == 14 | SCHG == 15 | SCHG == 16, "Still in  School",
                                            "Not in school" 
                                            
                     )))

enrolled <- count(inSchool, school, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(enrolled)




############################ MANUFACTURING ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  
##with less than a bachlor's degree, not working in manufacturing

manufacturing <- (inSchool %>%
                          filter(school == "Not in school")%>%
                          filter(MANU < 50)%>%
                          mutate(manu = ifelse(MANU == 3, "In Manufacturing", 
                                               "Not in Manufacturing")))

Manufa <- count(manufacturing, manu, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(Manufa)




########### Age                    
age <- (manufacturing %>%
                
                filter(manu == "Not in Manufacturing") %>% #keep participants not already working in manufacturing
                mutate(a = ifelse(AGEP >= 55, "55 +", "Under 55")))

ageCount <- count(age, a, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(ageCount) 




dd <- is.na(disability$JWTR)
disability$JWTR[dd] <- 0 ##change NA to 0, move to top

###Disability | Haven't worked in 5 years
disability <- (age %>%
                       
                       filter(a == "Under 55") %>% 
                       mutate(noWork = ifelse(WKL == 3 | DIS == 1, "Disability or no work 5 years", "Without disabilty, have worked")))

dis <- count(disability, noWork, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(dis)


###Transporation
transportation <- (disability %>%
                           
                           filter(noWork == "Without disabilty, have worked") %>% 
                           mutate(transport = ifelse(JWTR == 2 | JWTR == 3 | JWTR == 10, "Bus,Trolley,Walk", "Other transportation")))

transpo <- count(transportation, transport, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(transpo)



########### English



foreignborn <- (transportation %>%
                        
                        filter(transport == "Other transportation")%>%
                        mutate(f = ifelse(NATIVITY == 1, "Native",
                                          ifelse(NATIVITY == 2, "Foreign Born", 3))))

fb <- count(foreignborn, f, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(fb)











############################ FEMALE ##############################
## in Labor market, employed and unemployed, earning less than $28,974,  
##with less than a bachelor's degree, not working in manufacturing,
## + percent male/female 

sx <- (foreignborn %>%
               
               filter(f == "Native")%>%
               mutate(s = ifelse(SEX == 1, "Male", 
                                 ifelse(SEX == 2, "Female", "NA")))
)


sex <- count(sx, s, wt= PWGTP, sort = FALSE) #weight by PWGTP
percent(sex)

########### Female (take away 3/4)
##t = sex[1,2]/4 ##1/4 of females
##print(t)












