# loads libraries
library(haven)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(SOEPhelp)
library(plm)
library(stargazer)
library(dplyr)


##### selection of variables (to avoid loading whole gsoep)

# selects relevant variables from pathl section of gsoep

pathl_variables <- c("pid", 
                     "syear", 
                     "sex", 
                     "gebjahr", 
                     "migback")

# loads relevant variables from p-gen section of gsoep
pgen_variables <- c("cid", 
                    "pid", 
                    "syear",
                    "pgpartnr", 
                    "pglfs") 

# loads relevant variables from pequiv section of gsoep
pequiv_variables <- c("cid", 
                      "pid", 
                      "hid", 
                      "syear", 
                      "d11101", # Age
                      "d11109", # Years in Education
                      "i11102", # HH Post-Government Income
                      'l11101', # Bundesland
                      "l11102", # East/West-Germany
                      "w11105")


##### loads data 
soep_long<- read.csv("soep_for_jose.csv")

soep_long_pathl <- read_dta("ppathl.dta", 
                            encoding ="latin1", 
                            col_select = pathl_variables)

soep_long_pgen <- read_dta("pgen.dta", 
                           encoding ="latin1", 
                           col_select = pgen_variables)

soep_long_pequiv <- read_dta("pequiv.dta",
                             encoding ="latin1", 
                             col_select = pequiv_variables) 


##### merges data

# creates list of dataframes
df_list <- list(soep_long, 
                soep_long_pathl, 
                soep_long_pgen, 
                soep_long_pequiv)

# left joins all data sets
merge <- df_list %>%  reduce(left_join, by=c("pid", "syear"))


##### filters according to relevant variables 

# removes NAs in migration variables
merge_1 <- merge %>% filter(migback>0)

# removes NAs in gender variable
merge_2 <- merge_1 %>% filter(sex>0)

# removes NAs in age variable 
merge_3 <- merge_2 %>% filter(d11101>=18 &d11101<=79 )

# removes NAs in income variable 
merge_4 <- merge_3 %>% filter(i11102>0)

# removes NAs in education variable 
merge_5 <- merge_4 %>% filter(d11109>0)

# removes NAs in partner identifier variable 
merge_6 <- merge_5 %>% filter(pid>0) 

# removes NAs in year of survey variable 
merge_7 <- merge_6 %>% filter(syear>0)

# removes NAs in unemployment variable
merge_8 <- merge_7 %>% filter(pglfs>0)

##### Creates new variables by recoding existing ones

# recodes political interest variable
merge_8$plh0007 <- car::recode(merge_8$plh0007, "1 = 4; 2 = 3; 3=2; 4=1")

# creates new political interest variable 
merge_8 <- merge_8 %>% mutate(political_interest = ifelse(plh0007<1|plh0007>4,NA, plh0007))

# creates new political interest variable 
merge_8 <- merge_8 %>% mutate(gender= ifelse(sex==1, "1-Male", "2-Female"))

# creates new migration background variable 
merge_8 <- merge_8 %>% mutate(migration_background= ifelse(migback==1, "1-Native", "2-Migration Background"))

# creates new employment variable 
merge_8 <- merge_8 %>% mutate(employment_status= ifelse(pglfs%in%c(11,12), "1-Employment", 
                                                        ifelse(pglfs==3, "2-Education",
                                                               ifelse(pglfs==6, "3-Registered Unemployed", "4-Not Working"))))
# creates new age variable 
merge_8 <- merge_8 %>% mutate(age = d11101)                                             

# creates new age variable 
merge_8 <- merge_8 %>% mutate(age_group = ifelse(d11101>=18 & d11101<=29, "18-29", 
                                                 ifelse(d11101>=30 & d11101<=49, "30-49", 
                                                        ifelse(d11101>=50 & d11101<=79, "50-79", NA ))))

# creates new education variable 
merge_8 <- merge_8 %>% mutate(edu_c = ifelse(d11109 < 11, "1-Low Education", 
                                             ifelse(d11109 >= 11 & d11109 < 13, "2-Mid Education", 
                                                    ifelse(d11109>=13, "3-High Education", NA ))))


##### creates two dataframes one with respondent info and the other with info for respondent's partners. joins both data sets

# creates respondent df 
left_side <- merge_8 %>% 
  select(pid, syear, gender, political_interest, pgpartnr, i11102, age_group, edu_c, employment_status, migration_background, age) %>% 
  rename(income=i11102) 

# creates partner df by joining respondent info with list of partners 
partner_info <- merge_8  %>% 
  select(pid, syear, political_interest, gender ) %>% 
  rename(sex_partner=gender, pol_interest_partner=political_interest)

partner_list <- merge %>% select(pgpartnr, syear) %>% 
  filter(pgpartnr>0) %>% rename(partner_id=pgpartnr) 

right_side <- merge(partner_list, partner_info, by.x=c("partner_id","syear"), by.y=c("pid", "syear"))

# merges respondent df with partner df 
both_sides <- merge(left_side, right_side, by.x=c("pgpartnr", "syear"), by.y=c("partner_id", "syear"), all.x=T)

both_sides <- both_sides %>% filter(!is.na(political_interest))

# creates new variable that classifies respondents according to their partner's political interest 
both_sides <- both_sides %>% mutate(pol_interest_partner=ifelse (pol_interest_partner > 0, pol_interest_partner, NA))

both_sides <- both_sides %>% mutate(partner_situation = ifelse(pol_interest_partner%in%c(1,2), "2-Low Interest Partner", 
                                                               ifelse(pol_interest_partner%in%c(3,4), "3-High Interest Partner", 
                                                                      ifelse(pgpartnr!=-2 & is.na(pol_interest_partner),"4-Partner But No Info", 
                                                                             ifelse(pgpartnr==-2, "1-No Partner", NA)))))