###################################################
# Author: Fabian Kalleitner
# Title: R Intro 2 
# Purpose: Working with cross sectional data using tidy
# Version: V2.0
# Date: 24-05-2022

###################################################
#load packages
###################################################

#install.packages("ineq")

library("ineq")
library("tidyverse") 
library("panelr")
library("stargazer")

################################################
#now lets focus on incomes and wages and use dplyr and tidyR
################################################

data() #show all testdata
data("WageData") # load R testdata on wages (source package panelr)

head(WageData)
?WageData

#Wage data is a Panel we want to focus one one datapoint for the moment lets subset the data to only include data from 1976

WageData_cs <- WageData %>% filter(t==1)

#Wages are in logarithmic form (we will shortly explain next time why this approach might be adventagous for certain caclulations)  but let's focus on untransformed "normal" incomes right now 

WageData_cs <- WageData_cs %>% mutate(wage = exp(lwage))
head(WageData_cs)


############################## 
#Calculate gini coefficents,  draw Lorence curves,  make summary tables and export them to word
############################## 

ineq(WageData_cs$wage, type = c("Gini"), na.rm = TRUE)

#https://rdrr.io/cran/ineq/

Lc.w <- Lc(WageData_cs$wage)

plot(Lc.w)

table(WageData_cs$south)

WageData_cs_north <- WageData_cs %>% filter(south==0)

WageData_cs_south <- WageData_cs %>% filter(south==1)

Lc.w.n <- Lc(WageData_cs_north$wage)

Lc.w.s <- Lc(WageData_cs_south$wage)

plot(Lc.w.n, col=4)
lines(Lc.w.s, col=2)


# calculate a summary table with tidy 

exporttable <- WageData_cs %>% 
  select(c(wage,south)) %>% 
  group_by(south) %>% 
  summarise(mean = mean(wage, na.rm = TRUE),
            '10p' = quantile(wage, probs=0.1, na.rm=TRUE,names=FALSE),
            median = median(wage, na.rm = TRUE),
            '90p' = quantile(wage, probs=0.9, na.rm=TRUE,names=FALSE),
            n = n(),
            gini = ineq(wage, type = c("Gini"), na.rm = TRUE)
)

exporttable <- exporttable %>% mutate_all(., round, 2) # attention look at the differences between mutate_at mutate_if and mutate_all

#export tables into word -> stargazer

stargazer(exporttable, type = "html", summary = FALSE, rownames = TRUE, out="./output/tables/export_table1.doc")

# Draw a professional plot -> the ggplot function

ggplot(data=exporttable, aes(south, mean)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(y="mean of wages")

#export regression tables into word -> stargazer 

mysecondregression <- lm(wage ~ south, data= WageData_cs)
summary(mysecondregression)

t.test(WageData_cs$wage~ WageData_cs$south)

stargazer(mysecondregression,  type="html",no.space=TRUE,star.cutoffs = c(0.10,0.05, 0.01, 0.001),
          star.char = c("+", "*", "**","***"), notes= c("+p<0.1,*p<0.05,**p<0.01,***p<0.001"),
          notes.append = FALSE, out="regression_table_1.doc")