# loads libraries
library(haven)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(SOEPhelp)
library(plm)
library(stargazer)
library(dplyr)

# uses ols, taking a cross-sectional approach estimate the association between a respondent's level of political interest and their partner's 
model_1<-lm(as.numeric(political_interest)~partner_situation+as.factor(syear)+edu_c+gender+age_group+migration_background+employment_status,data=both_sides)

# uses a fixed-effect regression to estimate how entering and exiting relationship impacts a respodent's 
model_2<-plm(as.numeric(political_interest) ~ partner_situation + edu_c+age_group +employment_status+as.factor(syear), model = "within", index=c("pid","syear"), data=both_sides)


# displays regression results 
stargazer(model_1, model_2, 
          type="text",
          column.labels = c("OLS Model", "FE Model"), 
          dep.var.labels = "Political Interest (1-4, low to high)", 
          model.numbers = F, 
          covariate.labels = c("Low Interest Partner (Ref. No Partner)", "High Interest Partner", "Partner But No Info", "Mid Education (Ref. Low)", "High Education",  "Female (Ref. Male)", "Age 30-49 (Ref. 18-29)", "Age 50-79", "Migration Background (Ref. Native)", "Employment Status: In Education (Ref. Employed)", "Registered Unemployment", "Not Working"), 
          single.row = TRUE, 
          column.sep.width = "1pt", 
          omit = c("syear"),
          omit.labels = c("Wave"), 
          notes=c("Yearly survey waves were included in both models as a variable"))


# splits the data according to gender
male<- both_sides %>% filter(gender=="1-Male")

female<- both_sides %>% filter(gender=="2-Female")

# these models are identical to model 1 but run on the gendered datasets. 
model_3 <- lm(political_interest ~ partner_situation+edu_c+age_group+employment_status+syear+migration_background, data=male)

model_4 <- lm(political_interest ~ partner_situation+edu_c+age_group+employment_status+syear+migration_background, data=female)

# displays the results for the gendered linear regressions 
stargazer(model_3,model_4, 
          type="text",
          column.labels = c("Male Only", "Female Only"), 
          dep.var.labels = "Political Interest (1-4, low to high)", 
          model.numbers = F, 
          covariate.labels = c("Low Interest Partner (Ref. No Partner)", "High Interest Partner", "Partner But No Info", "Mid Education (Ref. Low)", "High Education", "Age 30-49 (Ref. 18-29)", "Age 50-79", "Employment Status: In Education (Ref. Employed)", "Registered Unemployment", "Not Working", "Migration Background (Ref. Native)"), 
          single.row = TRUE, 
          column.sep.width = "1pt", 
          omit = c("syear"),
          omit.labels = c("Wave"), 
          notes=c("Yearly survey waves were included in both models as a variable"))

# these models are identical to model 2 but run on gendered data sets 

model_5<-plm(as.numeric(political_interest) ~ partner_situation + edu_c+age_group +employment_status+syear, model = "within", index=c("pid","syear"), data=male)

model_6<-plm(as.numeric(political_interest) ~ partner_situation + edu_c+age_group+employment_status +syear, model = "within", index=c("pid","syear"), data=female)

# displays the results for the gendered fixed-effects regressions 

stargazer(model_5,model_6, 
          type="text",
          column.labels = c("Male Only", "Female Only"), 
          dep.var.labels = "Political Interest (1-4, low to high)", 
          model.numbers = F, 
          covariate.labels = c("Low Interest Partner (Ref. No Partner)", "High Interest Partner", "Partner But No Info", "Mid Education (Ref. Low)", "High Education", "Age 30-49 (Ref. 18-29)", "Age 50-79", "Employment Status: In Education (Ref. Employed)", "Registered Unemployment", "Not Working"), 
          single.row = TRUE, 
          column.sep.width = "1pt", 
          omit = c("syear"),
          omit.labels = c("Wave"), 
          notes=c("Yearly survey waves were included in both models as a variable"))



