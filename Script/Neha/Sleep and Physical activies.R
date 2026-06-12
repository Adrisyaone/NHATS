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
         cog_impair = Dementia_event,
         baseline_heartdisease=heartdisease[1],
         baseline_pa=pa[1],
         baseline_sleep=sleep_diff[1],
         sleep_lag = lag(sleep_diff)
  ) %>% 
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

# wt adjusted
design <- svrepdesign(
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)


# Model 1 (total effect)
cox_model1 <- svycoxph(
  Surv(start, stop, cog_impair) ~  pa+age + sex + race+ income_level + BMI_new +educat+overall_comorbid_cat,
  design = design
)
summary(cox_model1)

# Model 2 (direct effect)
cox_mode2 <- svycoxph(
  Surv(start, stop, cog_impair) ~  pa+sleep_diff+age + sex + race+ income_level + BMI_new +educat+overall_comorbid_cat,
  design = design
)
summary(cox_mode2)


# Model 2 (direct effect)
cox_mode2 <- svycoxph(
  Surv(start, stop, cog_impair) ~  pa+ sleep_lag+age + sex + race+ income_level + BMI_new +educat+overall_comorbid_cat,
  design = design
)
summary(cox_mode2)



library(survival)
surv_fit <- survfit(Surv(start, stop, cog_impair) ~ sleep_diff+pa, data=cdt_new)
plot(surv_fit, col=c("blue","red"), lty=1:2)
