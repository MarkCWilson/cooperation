##### still porting to R 2025-04-06

##### Load libraries and helper functions
library(tidyverse)
library(tidymodels)
#tidymodels_prefer()
library(purrr)


########### import data

setwd("/Users/mwilson/Documents/GitHub/cooperation/data")
df<- read.csv("ICS.csv", stringsAsFactors=FALSE)
df2<- df

########### clean data

# rename variables

df <- df %>% rename(p11=coop_mexotx, p10=coop_mexoty, p01=coop_meyotx, p00=coop_meyoty,
              p1=coop2, C0=coop1)

# remove derived variables
df <- df %>% select(!Strategy)

# our new derived variables
df <- df %>% mutate(C = (p1+p11+p01+p10+p00)/500, R = (abs(p10-p11) + abs(p00-p01))/200)

# race format fix
f = function(v) {return (as.numeric(strsplit(v,',')[[1]]))}
df <- df %>% mutate (q_race = q_race %>% map(f))

# deal with multiple races
# white only vs at least one nonwhite
g = function(v) {if(length(v)==0) {return(0)} else if (length(v)==1 && 1 %in% v) {return(v)} else {return(2)}} 
df <- df %>% mutate (q_race = q_race %>% map(g))

# must pass attention checks
df <- df %>% filter(pass1==1 & pass2==1)
  
# many NA

########## preliminary data exploration

print(colnames(df))
print(df %>% count(q_gender))
print(df %>% count(q_race))
#print(df %>% count(q_age))
print(summary(df$q_age))

########## feature selection

# find correlations - not finished
cor(as.numeric(df$q_age), as.numeric(df$crt), use = "complete.obs")

cols = c("p1", "p11", "p01", "p10", "p00", "q_gender","q_age","q_race","crt","trust","C","R") 
df2 <- df %>% select(all_of(cols))


######### linear regression

linear_reg() %>% set_engine("lm")

