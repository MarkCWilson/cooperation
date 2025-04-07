##### still porting to R 2025-04-06

##### Load libraries and helper functions

library(tidyverse)
install.packages("tidymodels")
tidymodels_prefer()


########### import data

setwd("/Users/mwilson/Documents/GitHub/cooperation/data")
df<- read.csv("ICS.csv")

########## preliminary data exploration

colnames(df)
df %>% count(q_gender)
df %>% count(q_race)
df %>% count(q_age)
summary(df$q_age)

########## feature selection

cor(as.numeric(df2$q_age), as.numeric(df2$crt), use = "complete.obs")

cols = c("q_gender","q_age","q_race","crt") 
df2 <- df %>% select(all_of(cols))


######### linear regression

linear_reg() %>% set_engine("lm")

