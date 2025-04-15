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

# rename and rescale variables

df <- df %>% rename(p11=coop_mexotx, p10=coop_mexoty, p01=coop_meyotx, p00=coop_meyoty,
              p1=coop2, C0=coop1, race = q_race, gender = q_gender, age = q_age) %>% mutate (p11=p11/100, p10=p10/100,p01=p01/100,p00=p00/100,p1=p1/100,C0=C0/100)

# remove derived variables
df <- df %>% select(!Strategy)

# our new derived variables
df <- df %>% mutate(C = (p1+p11+p01+p10+p00)/5, R = (abs(p10-p11) + abs(p00-p01))/2)

# race format fix
f = function(v) {return (as.numeric(strsplit(v,',')[[1]]))}
df <- df %>% mutate (race = race %>% map(f))

# deal with multiple races
# white only vs at least one nonwhite
g = function(v) {if(length(v)==0) {return(0)} else if (length(v)==1 && 1 %in% v) {return(v)} else {return(2)}} 
df <- df %>% mutate (race = race %>% map(g))

# must pass attention checks
df <- df %>% filter(pass1==1 & pass2==1)
  
# still many NA, deal with them in each analysis

########## preliminary data exploration

print(colnames(df))
print(df %>% count(gender))
print(df %>% count(race))
print(summary(df$age))
print(df %>% group_by(gender) %>% summarize(avg_C0 = mean(C0), avg_C = mean(C), avg_R = mean(R), avg_forg = mean(p10)))
print(df %>% group_by(race) %>% summarize(avg_C0 = mean(C0), avg_C = mean(C), avg_R = mean(R), avg_forg = mean(p10)))


########## regroup data
df <- df %>% mutate(gender = ifelse(gender>2,3,gender))

########## feature selection

# find correlations - not finished
corr <- cor(as.numeric(df$age), as.numeric(df$crt), use = "complete.obs")

cols = c("p1", "p11", "p01", "p10", "p00", "gender","age","race","crt","trust","C0", "C","R") 
df2 <- df %>% select(all_of(cols))

########## linear regression

lm_model <- linear_reg() %>% set_engine("lm")
lm_form_fit <- lm_model %>% fit(C0 ~ gender + age + crt + trust, data = df)
out1 <- lm_form_fit %>% extract_fit_engine() %>% summary()

lm_form_fit <- lm_model %>% fit(C~crt, data = df)
out2 <- lm_form_fit %>% extract_fit_engine() %>% summary()

########## breakdown by groups

p1<- ggplot(df2, aes(x=as.factor(gender), y=C)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("gender") + geom_boxplot(fill="pink", alpha=0.3)

p2<- ggplot(df2, aes(x=as.factor(gender), y=R)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("gender") + geom_boxplot(fill="pink", alpha=0.3)

p3<- ggplot(df2, aes(x=as.factor(race), y=C)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("race") + geom_boxplot(fill="pink", alpha=0.3)

p4<- ggplot(df2, aes(x=as.factor(race), y=R)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("race") + geom_boxplot(fill="pink", alpha=0.3)

p5<- ggplot(df2, aes(x=as.factor(crt), y=C)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("CRT") + geom_boxplot(fill="pink", alpha=0.3)

p6<- ggplot(df2, aes(x=as.factor(crt), y=R)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("CRT") + geom_boxplot(fill="pink", alpha=0.3)


