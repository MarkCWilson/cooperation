##### still porting to R 2025-04-06

##### Load libraries and helper functions
library(tidyverse)
library(tidymodels)
#tidymodels_prefer()
library(purrr)

##### import data

setwd("/Users/mwilson/Documents/GitHub/cooperation/")
df<- read.csv("data/ICS.csv", stringsAsFactors=FALSE)
df2<- df

##### clean data

# rename and rescale variables
colnames(df)

df <- df %>% rename(p11=coop_mexotx, p10=coop_mexoty, p01=coop_meyotx, p00=coop_meyoty,
              p1=coop2, C0=coop1, race = q_race, gender = q_gender, age = q_age, disc = ddt_log_k) %>% 
        mutate (p11=p11/100, p10=p10/100,p01=p01/100,p00=p00/100,p1=p1/100,C0=C0/100)

df <- as_tibble(df)

# remove derived variables
df <- df %>% select(!Strategy)

# our new derived variables
df <- df %>% mutate(C = (p1+p11+p01+p10+p00)/5, R = (abs(p10-p11) + abs(p00-p01))/2)

# fix carit and flanker missing data (each subject did only one of these)

df <- df %>% mutate(IC = if_else(!is.na(flanker_correct), flanker_correct, carit_correct))

# race format fix
f = function(v) {return (as.integer(strsplit(v,',')[[1]]))}
df <- df %>% mutate (race = race %>% map(f)) 

# deal with multiple races
# no answer vs white only vs at least one nonwhite
#g = function(v) {if(length(v)==0) {return(0)} else if (length(v)==1 && 1 %in% v) {return(1)} else {return(2)}} 
# easier version
h = function(v) {if (1 %in% v) {return(1)} else {return(2)}} 
df <- df %>% rowwise %>% mutate (race = h(race))

# must pass attention checks
df <- df %>% filter(pass1==1 & pass2==1)
  
# still many NA, deal with them individually in each analysis

##### preliminary data exploration

print(colnames(df))
print(df %>% count(gender))
print(df %>% count(race))
print(summary(df$age))
print(df %>% group_by(gender) %>% summarize(avg_C0 = mean(C0), avg_C = mean(C), avg_R = mean(R), avg_forg = mean(p10)))
print(df %>% group_by(race) %>% summarize(avg_C0 = mean(C0), avg_C = mean(C), avg_R = mean(R), avg_forg = mean(p10)))

#####regroup data

df <- df %>% mutate(gender = ifelse(gender>2,3,gender))

##### feature selection - do in more detail later, uses domain knowledge and data

cols = c("p1", "p11", "p01", "p10", "p00", "gender","age","race", "disc", "crt", 
         "IC", "nfc", "trust","C0", "C","R") 
df2 <- df %>% select(all_of(cols))

##### breakdown by groups

p1<- df2 %>% drop_na(gender) %>% ggplot(aes(x=as.factor(gender), y=C)) + 
  geom_violin(fill="slateblue", alpha=0.6) + 
  xlab("gender") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_gender_C.png", width = 10, height = 10)

p2<- df2 %>% drop_na(gender) %>% ggplot(aes(x=as.factor(gender), y=R))  +
  geom_violin(fill="slateblue", alpha=0.6) +
  xlab("gender") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_gender_R.png", width = 10, height = 10)

p3<- df2 %>% drop_na(race) %>% ggplot(aes(x=as.factor(race), y=C)) +
  geom_violin(fill="slateblue", alpha=0.6)  +
  xlab("race") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_race_C.png", width = 10, height = 10)

p4<- df2 %>% drop_na(race) %>% ggplot(aes(x=as.factor(race), y=R))  +
  geom_violin(fill="slateblue", alpha=0.6)  +
  xlab("race") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_race_R.png", width = 10, height = 10)

p5<- df2 %>% drop_na(crt) %>% ggplot(aes(x=as.factor(crt), y=C))  +
  geom_violin(fill="slateblue", alpha=0.6)  +
  xlab("CRT") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_crt_C.png", width = 10, height = 10)

p6<- df2 %>% drop_na(crt) %>% ggplot(aes(x=as.factor(crt), y=R)) +
  geom_violin(fill="slateblue", alpha=0.6) +
  xlab("CRT") + geom_boxplot(fill="pink", alpha=0.3)

s <- ggsave("output/figures/violin_crt_R.png", width = 10, height = 10)

p7<- df2 %>% drop_na(gender) %>% group_by(gender) %>% ggplot(aes(y=C, color=factor(gender))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_gender_C.png", width = 10, height = 10)

p8<- df2 %>% drop_na(gender) %>% group_by(gender) %>% ggplot(aes(y=R, color=factor(gender))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_gender_R.png", width = 10, height = 10)

p9<- df2 %>% drop_na(gender) %>% group_by(gender) %>% ggplot(aes(y=crt, color=factor(gender))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_gender_crt.png", width = 10, height = 10)

p10<- df2 %>% drop_na(race) %>% group_by(race) %>% ggplot(aes(y=C, color=factor(race))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_race_C.png", width = 10, height = 10)

p11<- df2 %>% drop_na(race) %>% group_by(race) %>% ggplot(aes(y=R, color=factor(race))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_race_R.png", width = 10, height = 10)

p12<- df2 %>% drop_na(race) %>% group_by(race) %>% ggplot(aes(y=crt, color=factor(race))) + stat_ecdf(geom = "step")
s <- ggsave("output/figures/ecdf_race_crt.png", width = 10, height = 10)


############ can we predict anything interesting?

# may need to exclude genders other than 1 & 2
# looks like many other NA to remove, coming in prediction of race, why?

df2 <- df2 %>% drop_na(gender) %>%
  mutate(race = as.factor(race), gender = as.factor(gender)) %>%
  filter(gender == 1 | gender == 2) %>% droplevels()

df2 <- df2 %>% drop_na(race) %>%
  filter(race == 1 | race == 2) %>%droplevels()

set.seed(222)
# split into training and testing 
data_split <- initial_split(df2, prop = 3/4)

# create data frames for the two sets
train_data <- training(data_split)
test_data  <- testing(data_split)

gender_rec <- 
  recipe(gender ~ ., data = train_data) %>% 
  update_role(age,race,disc,new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

race_rec <- 
  recipe(race ~ ., data = train_data) %>% 
  update_role(age,gender,disc,new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

mod <- function(m) {
  
  if(m=="LogR") 
  {
    meth <- 
      logistic_reg(
        mode = "classification",
        engine = "glm",
        penalty = NULL,
        mixture = NULL
      )
  }
  else if(m=="XG")
  {
    meth <- boost_tree(
      mode = "classification",
      engine = "xgboost",
      mtry = NULL,
      trees = NULL,
      min_n = NULL,
      tree_depth = NULL,
      learn_rate = NULL,
      loss_reduction = NULL,
      sample_size = NULL,
      stop_iter = NULL
    )
  }
  else if (m=="RF")
  {
    meth <- rand_forest(mtry = 2, trees = 1000) %>% 
      set_engine("ranger") %>% 
      set_mode("classification")
  }
  
  return(meth)
}

pred <- function(m) {
  
  # make workflow
  gender_wf <- 
    workflow() %>% 
    add_model(mod(m)) %>% 
    add_recipe(gender_rec)
  
  race_wf <- 
    workflow() %>% 
    add_model(mod(m)) %>% 
    add_recipe(race_rec)
  
  # fit model on training data
  gender_fit <- 
   gender_wf %>% 
    fit(data = train_data)
  
  race_fit <- 
    race_wf %>% 
    fit(data = train_data)

  #  vote_fit_dem %>% 
  #    extract_fit_parsnip() %>% 
  #    tidy() 
  
  # predict and evaluate
  gender_aug <- 
    augment(gender_fit, test_data)
  race_aug <- 
    augment(race_fit, test_data)

 return(cbind(gender_aug %>% select(gender, pred_gender = .pred_class), 
          race_aug %>% select(race, pred_race = .pred_class)))
}

############ classify gender from crt, C, R

p <- pred("LogR")
metrics <- metric_set(accuracy, precision, recall)
resultsg <- p %>% metrics(gender, estimate = pred_gender)
cmg <- conf_mat(p, gender, pred_gender) %>% tidy()
show(resultsg)
show(cmg)

############ classify race from crt, C, R


metrics <- metric_set(accuracy, precision, recall)
resultsr <- p %>% metrics(race, estimate = pred_race)
cmr <- conf_mat(p, race, pred_race) %>% tidy()
show(resultsr)
show(cmr)