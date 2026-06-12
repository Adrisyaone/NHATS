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
         start=lag(Wave),
         start=ifelse(is.na(start), 0, start),
         stop=Wave,
         event = Dementia_event,
         baseline_memory=first(cgratememry),
         baseline_depression= first(depressionSym)
  ) %>%
  filter(age %in% c("1 - 65-69", "2 - 70-74", "3 - 75-79")) %>% 
  filter(baseline_memory !=" 5 POOR" ) %>% 
  ungroup()





# clean wt
cdt<-cdt |> 
  group_by(spid) |> 
  # Ensure the data is in chronological order for each person
  arrange(Wave, .by_group = TRUE) %>% 
  # Apply the 'first' function to the main weight and all 56 replicates
  mutate(across(starts_with("trbswgt"), ~ first(.))) %>%
  ungroup()

# exclude with impairment at the baseline 
cdt_new <- cdt %>% 
  group_by(spid) %>% 
  mutate(flag= ifelse(Wave==1 & first(Dementia_event)==1, 1,0),
         flag2=sum(flag, na.rm=T)) %>%  # flag participant with dementia_event at first
  filter(flag2!=1) %>% 
  mutate(
    event_cum = cumsum(Dementia_event == 1),
    flag3 = case_when(
      event_cum == 0 ~ 0,                         # before event
      event_cum == 1 & Dementia_event == 1 ~ 1,   # event row
      event_cum >= 1 & Dementia_event == 0 ~ 2    # after event
    )
  ) |> 
  filter(flag3<=1)





# remove NA
cdt_surv_clean <- cdt_new %>%
  mutate(
    start = as.numeric(start),
    stop = as.numeric(stop),
    event = as.numeric(event),
    id_sp=spid
  )




# Baseline characteristics of the study participants
cdt %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "pa", "overall_comorbid_cat", "sleep_diff", "depressionSym", "anxSym", "dep_anx", "phq2", "gad2"))





library(survival)


# wt adjusted
design <- svrepdesign(
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_surv_clean,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)


# Model 3
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  age + sex + race+ income_level +educat+pa+sleep_diff+dep_anx ,
  design = design
)
summary(cox_model)

