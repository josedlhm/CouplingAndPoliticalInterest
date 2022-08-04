library(haven)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(SOEPhelp)
library(plm)
library(stargazer)
library(dplyr)


# collapses levels of political interest (on a 1-4 scale) to a binary variable. 
both_sides <- both_sides %>% mutate(pol_interest_factor = ifelse(political_interest%in%c(1,2),"1- Low Interest","2-High Interest"))

#creates labels for variables in summary table
table1::label(both_sides$pol_interest_factor) <- "Political Interest"
table1::label(both_sides$partner_situation) <- "Partner Political Interest"
table1::label(both_sides$age_group) <- "Age Category"
table1::label(both_sides$gender) <- "Gender"
table1::label(both_sides$edu_c) <- "Education Level"
table1::label(both_sides$migration_background) <- "Migration Background"
table1::label(both_sides$employment_status) <- "Employment Status"
table1::table1(~partner_situation + age_group + gender + edu_c+pol_interest_factor+employment_status+migration_background| partner_situation, data = both_sides)


# creates a proportion table to show the distribution of political interest by political interest of partner

table_1 <- table(both_sides$partner_situation,both_sides$political_interest) 

table_2 <- prop.table(table_1,1)

table_3 <- as.data.frame(table_2)

table_3$percent <- table_3$Freq*100


political_interest_plot <- ggplot(table_3, aes(fill=Var1, x=Var2, y=percent))+ 
  geom_bar(stat="identity", position=position_dodge(width=.5))+ 
  ylab("%")+ 
  xlab("Political Interest (from Low to High)")+ 
  labs(title="Distribution of Political Interest by Partnership Status")+
  scale_fill_discrete(name = "Partnership status", labels=c("No Partner", "Low Interest Partner", "High Interest Partner", "Partner But No Info"))+
  
  theme_bw()
# displays plot
political_interest_plot