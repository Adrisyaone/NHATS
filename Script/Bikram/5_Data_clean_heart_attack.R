# clear environment
rm(list=ls())

# load packages and functions
source("Script/1 Load pkgs and functions.R")

# load processed data
cdt<-readRDS("Dataset/Processed/cdt_cleaned.RDS")

cdt<- cdt %>% 
  # working on cohort 1 only
  filter(enrolled_wave==1) %>% 
  group_by(spid) %>% 
  arrange(spid, Wave) %>% 
  mutate(year_in_study = Wave-Wave[1],
         start=Wave,
         stop=lead(Wave),
         event = Dementia_event
         
         
  ) %>% 
  ungroup()

# exclude with impairment at the baseline 
cdt_new <- cdt %>% 
  group_by(spid) %>% 
  mutate(flag= ifelse(Wave==1 & first(Dementia_event)==1, 1,0),
         flag2=sum(flag, na.rm=T)) %>%  # flag participant with dementia_event at first
  filter(flag2!=1) %>% 
  filter(cumsum(Dementia_event == 1) <= 1) # remove all other rows once event is observed)


# remove NA
cdt_surv_clean <- cdt_new %>%
  mutate(
    start = as.numeric(start),
    stop = as.numeric(stop),
    event = as.numeric(event),
    id_sp=spid
  ) %>% 
  filter(!is.na(event) & !is.na(start) & !is.na(stop) & start < stop) %>% 
  filter(!is.na(rd2intvrage))




# Baseline characteristics of the study participants
cdt %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level","BMI_new", "pa","pain_cat", 'heartattack', "heartdisease", "heart_conditions"))


# incidence of cognitive decline
incidence(data = cdt_surv_clean, strata = age)



library(survival)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heart_conditions + age + sex + race + income_level + BMI_new + pa+educat,,
  data = cdt_surv_clean
)


summary(cox_model)







cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~ 
    heart_conditions + age + sex + race + income_level + BMI_new + pa+educat,
  design = design
)



summary(cox_model)





km_fit <- svykm(
  Surv(start, stop, Dementia_event) ~ heart_conditions,
  design = design,
  se = TRUE
)
