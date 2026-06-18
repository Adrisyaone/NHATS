# clear environment
rm(list=ls())

# load packages and functions
source("Script/1 Load pkgs.R")
source("Script/Functions/my_functions.R")
# load processed data
cdt<-read_encrypt_data("Dataset/Processed/cdt_v2_encrypted.RDS")


# generate start, stop and event
cdt<- cdt %>% 
  # working on cohort 1 only
  filter(enrolled_wave==1) %>% 
  group_by(spid) %>% 
  arrange(spid, Wave) %>% 
  mutate(year_in_study = Wave-Wave[1],

         start = lag(rdintvwrage),
         stop  = rdintvwrage,
         start = ifelse(is.na(start), stop, start),
         event = Dementia_event, # define event
         baseline_age = first(rdintvwrage),
         baseline_heartdisease=heartdisease[1],
         heart_disease_ever=ifelse(any(heartdisease==1), 1,0)
  ) %>% 
  mutate(start = ifelse(start == stop, start - 1, start)) |> 
  ungroup()




# clean baseline weight and add to the dataset
cdt<-cdt |> 
  group_by(spid) |> 
  # Ensure the data is in chronological order for each person
  arrange(Wave, .by_group = TRUE) %>% 
  # Apply the 'first' function to the main weight and all 56 replicates
  mutate(across(starts_with("trbswgt"), ~ first(.))) %>%
  ungroup()




# exclude participants with impairment at the baseline 
cdt_new <- cdt %>% 
  group_by(spid) %>% 
  mutate(flag= ifelse(Wave==1 & first(Dementia_event)==1, 1,0),
         flag2=sum(flag, na.rm=T)) %>%  # flag participant with dementia_event at first
  filter(flag2!=1)
  
  
  
  
  
# determine dementia diagnosed wave, death wave, time to event and event
cdt_new <- cdt_new %>%
  group_by(spid) %>%
  mutate(
    dem_time = if (any(Dementia_event == 1, na.rm = TRUE)) 
      min(rdintvwrage[Dementia_event == 1], na.rm = TRUE) 
    else NA_real_,
    
    death_time = if (any(died == 1, na.rm = TRUE)) 
      min(rdintvwrage[died == 1], na.rm = TRUE) 
    else NA_real_,
    
    last_time = max(rdintvwrage, na.rm=T)) %>% 
  mutate(
    ftime = case_when(
      !is.na(dem_time) & (is.na(death_time) | dem_time < death_time) ~ dem_time,
      !is.na(death_time) & (is.na(dem_time) | death_time < dem_time) ~ death_time,
      TRUE ~ last_time
    ),
    fstatus = case_when(
      !is.na(dem_time) & (is.na(death_time) | dem_time < death_time) ~ 1,  # dementia
      !is.na(death_time) & (is.na(dem_time) | death_time < dem_time) ~ 2,  # death
      TRUE ~ 0  # censored
    )

  )




# exclude rows after event occurs
cdt_new<- cdt_new %>% 
  mutate(
    event_cum = cumsum(Dementia_event == 1),
    flag3 = case_when(
      event_cum == 0 ~ 0,                         # before event
      event_cum == 1 & Dementia_event == 1 ~ 1,   # event row
      event_cum >= 1 & Dementia_event == 0 ~ 2    # after event
    )
  ) |> 
  filter(flag3<=1)



#Add person time to data
cdt<-cdt %>% 
  mutate(pt=stop-start)


# Baseline characteristics before filtering 
k1<-cdt |> 
    select(spid, start, stop,  heart_conditions, heartdisease, heartattack,Dementia_event, age,rdintvwrage, sex ,race, income_level, BMI_new , pa, educat, sleep_diff, dep_anx, depressionSym,dep_anx, Wave)


k1 %>% 
    filter(Wave==1) %>% 
    tbl_summary(include = c("sex", "age","race","income_level","BMI_new", "pa", "Dementia_event", "heartdisease", "heartattack"))
  
  


# Baseline characteristics after filtering 
k2<-cdt_new |> 
  select(spid, start, stop, ftime, fstatus, dem_time, death_time, last_time, heart_conditions, heartdisease, heartattack, baseline_heartdisease, heart_disease_ever, Dementia_event, age,rdintvwrage, sex ,race, income_level, BMI_new , pa, educat, sleep_diff, dep_anx, depressionSym,dep_anx, Wave)


write_dta(k2 |> filter(Wave==1), "Dataset/SAS/dt_to_dta_forFG.dta")

k2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","rdintvwrage","race","income_level","educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack"))




# Create design for weighted analysis
# wt adjusted (final wt)
design2 <- svrepdesign(
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

# wt adjusted (basewt)
design <-svydesign(
  id=~spid,
  weights = ~trbswgt0,
  data = cdt_new,
  type = "Fay",
  rho=0.3,
)




# ----------------------------------------------------------
#                   Baseline characteristics               -
#-----------------------------------------------------------
# Unweighted

# Baseline characteristics after filtering 
k2<-cdt_new |> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff)


k2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design |>
 subset(Wave==1) |> 
  tbl_svysummary(include =c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))








# ----------------------------------------------------------
#                          Heart disease                   -
#-----------------------------------------------------------
# Unweighted

# Model 1
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease,
  data = cdt_new
)

summary(cox_model)



# Model 2
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+age + sex + race ,
  data = cdt_new
)

summary(cox_model)


# Model 3
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+age + sex + race+educat+income_level,
  data = cdt_new
)
summary(cox_model)


# Model 4
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+age + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new
)

summary(cox_model)






# ----------------------------------------------------------
#                          Heart disease                   -
#-----------------------------------------------------------
# Weighted

# Model 1
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease,
  design = design
)

summary(cox_model)



# Model 2
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+age + sex + race ,
  design = design
)

summary(cox_model)


# Model 3
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+age + sex + race+income_level+educat ,
  design = design
)
summary(cox_model)


# Model 4
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)

summary(cox_model)








# ----------------------------------------------------------
#                          Heart attack                   -
#-----------------------------------------------------------
# UnWeighted

# Model 1
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack,
  data = cdt_new
)

summary(cox_model)



# Model 2
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack+age + sex + race ,
  data = cdt_new
)

summary(cox_model)


# Model 3
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack+age + sex + race+income_level+educat ,
  data = cdt_new
)
summary(cox_model)


# Model 4
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack+age + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new
)

summary(cox_model)




# ----------------------------------------------------------
#                          Heart attack                   -
#-----------------------------------------------------------
# Weighted



# Model 1
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack,
  design = design
)

summary(cox_model)



# Model 2
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+age + sex + race ,
  design = design
)

summary(cox_model)


# Model 3
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~ heartattack+age + sex + race+income_level+educat ,
  design = design
)
summary(cox_model)


# Model 4
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+age + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)

summary(cox_model)



# --------------------- SENSitivity analysis----------------------------
# ----------------------------------------------------------
#                          Sensitivity analysis           -
#-----------------------------------------------------------
# incident heart disease and cognitive impariment
cdt_new2 <- cdt_new%>% 
  group_by(spid) %>% 
  mutate(flag_M= ifelse(Wave==1 & first(heartdisease)==1, 1,0),
         flag_M2=sum(flag_M, na.rm=T)) %>%  # flag participant with dementia_event at first
  filter(flag_M2!=1)

cdt_new2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))

# ----------------------------------------------------------
#                          Heart disease                   -
#-----------------------------------------------------------
# Unweighted

# Model 1
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease,
  data = cdt_new2
)

summary(cox_model)



# Model 2
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race ,
  data = cdt_new2
)

summary(cox_model)


# Model 3
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+ sex + race+educat+income_level,
  data = cdt_new2
)
summary(cox_model)


# Model 4
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+baseline_age + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new2
)

summary(cox_model)


# Model 5 (interaction)
cox_modelHD2 <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease*income_level+ sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new2
)


dtHA7<-data.frame(extract_interaction_hr(model = cox_modelHD2,exposure = "heartdisease", modifier = "income_level"))
dtHA7$exposure <-"Heart Disease"
dtHA7$model <-"Model2"
dtHA7$wt <-"Unweighted"



# Model 6 (interaction)
#1
cox_modelHD1 <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease*income_level + sex + race+educat+income_level,
  data = cdt_new2
)

summary(cox_modelHD1)


dtHA8<-data.frame(extract_interaction_hr(model = cox_modelHD1,exposure = "heartdisease", modifier = "income_level"))
dtHA8$exposure <-"Heart Disease"
dtHA8$model <-"Model1"
dtHA8$wt <-"Unweighted"



# ----------------------------------------------------------
#                          Heart disease                   -
#-----------------------------------------------------------
# Weighted
# wt adjusted (final wt)
design2 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new2,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

# wt adjusted (basewt)
design <-svydesign(
  id=~spid,
  weights = ~trbswgt0,
  data = cdt_new2,
  type = "Fay",
  rho=0.3,
)


# Model 1
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease,
  design = design
)

summary(cox_model)



# Model 2
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race ,
  design = design
)

summary(cox_model)


# Model 3
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race+income_level+educat ,
  design = design
)
summary(cox_model)


# Model 4
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)

summary(cox_model)

# Model 5 (interaction)
cox_modelHD2 <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease*income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)

dtHA5<-data.frame(extract_interaction_hr(model = cox_modelHD2,exposure = "heartdisease", modifier = "income_level"))
dtHA5$exposure <-"Heart Disease"
dtHA5$model <-"Model2"
dtHA5$wt <-"Weighted"



# Model 6 (interaction)
#1
cox_modelHD1 <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease*income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)
summary(cox_modelHD1)


dtHA6<-data.frame(extract_interaction_hr(model = cox_modelHD1,exposure = "heartdisease", modifier = "income_level"))
dtHA6$exposure <-"Heart Disease"
dtHA6$model <-"Model1"
dtHA6$wt <-"Weighted"







# ----------------------------------------------------------
#                          Heart attack                   -
#-----------------------------------------------------------
cdt_new3 <- cdt_new%>% 
  group_by(spid) %>% 
  mutate(flag_M= ifelse(Wave==1 & first(heartattack)==1, 1,0),
         flag_M2=sum(flag_M, na.rm=T)) %>%  # flag participant with dementia_event at first
  filter(flag_M2!=1)


cdt_new3 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


# UnWeighted

# Model 1
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack,
  data = cdt_new3
)

summary(cox_model)



# Model 2
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race ,
  data = cdt_new3
)

summary(cox_model)


# Model 3
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race+income_level+educat ,
  data = cdt_new3
)
summary(cox_model)


# Model 4
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new3
)

summary(cox_model)


# Model 5 (interaction)
cox_modelHA2 <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack*income_level+ sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new3
)


dtHA3<-data.frame(extract_interaction_hr(model = cox_modelHA2,exposure = "heartattack", modifier = "income_level"))
dtHA3$exposure <-"Heart attack"
dtHA3$model <-"Model2"
dtHA3$wt <-"Unweighted"

# Model 6 (interaction)
#1
cox_modelHA1 <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack*income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  data = cdt_new3
)
summary(cox_modelHA1)


dtHA4<-data.frame(extract_interaction_hr(model = cox_modelHA1,exposure = "heartattack", modifier = "income_level"))
dtHA4$exposure <-"Heart attack"
dtHA4$model <-"Model1"
dtHA4$wt <-"Unweighted"


 
# ----------------------------------------------------------
#                          Heart attack                   -
#-----------------------------------------------------------
# Weighted

# wt adjusted (final wt)
design2 <- svrepdesign(
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new3,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

# wt adjusted (basewt)
design <-svydesign(
  id=~spid,
  weights = ~trbswgt0,
  data = cdt_new3,
  type = "Fay",
  rho=0.3,
)


# Model 1
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack,
  design = design
)

summary(cox_model)



# Model 2
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race ,
  design = design
)

summary(cox_model)


# Model 3
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~ heartattack + sex + race+income_level+educat ,
  design = design
)
summary(cox_model)


# Model 4
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design
)
summary(cox_model)

# Model 5 (interaction)
cox_modelHA2 <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack*income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design
)


dtHA2<-data.frame(extract_interaction_hr(model = cox_modelHA2,exposure = "heartattack", modifier = "income_level"))
dtHA2$exposure <-"Heart attack"
dtHA2$model <-"Model2"
dtHA2$wt <-"Weighted"

# Model 6 (interaction)
#1
cox_modelHA1 <- svycoxph(
  Surv(start, stop, Dementia_event) ~ heartattack*income_level + sex + race+income_level+educat ,
  design = design
)
summary(cox_modelHA1)


dtHA1<-data.frame(extract_interaction_hr(model = cox_modelHA1,exposure = "heartattack", modifier = "income_level"))
dtHA1$exposure <-"Heart attack"
dtHA1$model <-"Model1"
dtHA1$wt <-"Weighted"




# ----------------------------------------------------------
#                          Sensitivity analysis 2           -
#-----------------------------------------------------------
k2<-cdt_new |> 
  ungroup() |> 
  select(spid, start, stop, ftime, fstatus, dem_time, death_time, last_time, heart_conditions, heartdisease, heartattack, baseline_heartdisease, heart_disease_ever, Dementia_event, age, sex ,race, income_level, BMI_new , pa, educat, sleep_diff, dep_anx, depressionSym,dep_anx, Wave) |> 
  filter(Wave==1)


library("pseudo")
library(survival)
t_star <- 2
pseudo_data <- pseudosurv(
  time   = k2$ftime,
  event  = (k2$fstatus == 1),  # dementia = 1
  tmax   = t_star
)



k2$cif <- pseudo_data$pseudo




fg_tv_model <-glm(
  cif ~ heart_disease_ever + age + sex + race +
    income_level + educat + BMI_new + pa + sleep_diff,
 data =k2
)



summary(fg_tv_model)





# Interaction between heart disease and income (plot)
df<-bind_rows(dtHA1, dtHA2, dtHA3, dtHA4, dtHA5, dtHA6, dtHA7, dtHA8)

df <- df%>%
  filter(wt == "Weighted") %>%
  mutate(
    income = Group,   # rename for clarity
    income = factor(income,
                    levels = c("Reference (baseline modifier)",
                               "income_level2Moderate",
                               "income_level3High")),
    
    exposure = factor(exposure),
    model = factor(model),
    wt = factor(wt)
  )

df$income <- recode(df$income,
                     "Reference (baseline modifier)" = "Low (Ref)",
                     "income_level2Moderate" = "Moderate",
                     "income_level3High" = "High"
)

ggplot(df, aes(x = income, y = HR,
                 color = exposure,
                 group = exposure)) +
  
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    position = position_dodge(width = 0.6),
    width = 0.15
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  facet_wrap(~model) +
  
  labs(
    title = "Cognitive impariment Risk by time varying cardiovascular Exposure",
    subtitle = "Weighted Cox models: Effect modification by income",
    x = "Income Level",
    y = "Hazard Ratio (HR)",
    color = "Exposure"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )








# sensitivity analysis stratified by age (heart disease)
# Weighted
# wt adjusted (final wt)

cdt_new2_1<-cdt_new2 %>%
  filter(baseline_age<75)


# Baseline characteristics after filtering 
# <75 years
cdt_new2_1|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_1 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new2_1,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new2_1
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_1
)
summary(cox_model)




# >=75 years
cdt_new2_2<-cdt_new2 %>%
  filter(baseline_age>=75)

cdt_new2_2|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_2 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new2_2,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new2_2
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_2
)
summary(cox_model)





# sensitivity analysis stratified by age (heart attack)
# Weighted
# wt adjusted (final wt)

cdt_new3_1<-cdt_new3 %>%
  filter(baseline_age<75)


# Baseline characteristics after filtering 
# <75 years
cdt_new3_1|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_22 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new3_1,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new3_1
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+income_level + sex + race+income_level+educat+BMI_new + pa+educat+sleep_diff ,
  design = design_22
)
summary(cox_model)




# >=75 years
cdt_new3_2<-cdt_new3 %>%
  filter(baseline_age>=75)

cdt_new3_2|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_222 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new3_2,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new3_2
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+income_level+ race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_222
)
summary(cox_model)







# by sex (heart attack)
# Weighted
# wt adjusted (final wt)

cdt_new3_1<-cdt_new3 %>%
  filter(sex=="1 MALE")

# Male
cdt_new3_1|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_22 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new3_1,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack  + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new3_1
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+income_level + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_22
)
summary(cox_model)




# Female
cdt_new3_2<-cdt_new3 %>%
  filter(sex=="2 FEMALE")

cdt_new3_2|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_222 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new3_2,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartattack + sex + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new3_2
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartattack+income_level + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_222
)
summary(cox_model)





# by sex by heart disease
cdt_new2_1<-cdt_new2 %>%
  filter(sex=="1 MALE")


# Baseline characteristics after filtering 
# Male
cdt_new2_1|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_22 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new2_1,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease  + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new2_1
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+income_level + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_22
)
summary(cox_model)




# Female
cdt_new2_2<-cdt_new3 %>%
  filter(sex=="2 FEMALE")

cdt_new2_2|> 
  select(spid, start, stop, Wave, heart_conditions, heartdisease, heartattack, Dementia_event, age,  sex ,race, income_level, educat, BMI_new , pa, sleep_diff) %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level", "educat","BMI_new", "pa","sleep_diff", "Dementia_event", "heartdisease", "heartattack", "heart_conditions"))


design_222 <- svrepdesign(
  
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_new2_2,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~  heartdisease + race+educat+income_level+BMI_new + pa+educat+sleep_diff,
  data = cdt_new2_2
)
summary(cox_model)


cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  heartdisease+income_level + race+income_level+educat+BMI_new + pa+educat+sleep_diff+Wave ,
  design = design_222
)
summary(cox_model)





# KM plot
# Fit Cox model with time-varying exposure
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~ heartattack,
  data = cdt_new3
)

# Reference dataset — two groups
newdata_ref <- data.frame(heartattack = c(0, 1))

# Predicted survival
cox_surv <- survfit(cox_model, newdata = newdata_ref)

# Convert to dataframe
surv_df <- data.frame(
  time  = rep(cox_surv$time, 2),
  surv  = c(cox_surv$surv[,1], cox_surv$surv[,2]),
  lower = c(cox_surv$lower[,1], cox_surv$lower[,2]),
  upper = c(cox_surv$upper[,1], cox_surv$upper[,2]),
  group = rep(
    c("No Heart Attack", "Heart Attack"),
    each = length(cox_surv$time)
  )
)

# Median survival
median_surv <- surv_df %>%
  group_by(group) %>%
  summarise(
    median_time = time[which.min(abs(surv - 0.5))],
    .groups = "drop"
  )

# Plot
ggplot(surv_df, aes(x = time, y = surv,
                    color = group, fill = group)) +
  geom_step(linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15, color = NA) +
  
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values  = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  coord_cartesian(xlim = c(65, 90)) +
  labs(
    title    = "Predicted Cognitive Impairment-free Survival",
    subtitle = "Adjusted for time-varying heart attack exposure (Cox model)",
    x        = "Age (years)",
    y        = "Survival Probability",
    color    = "Heart Attack Status",
    fill     = "Heart Attack Status"
  ) +
    theme_light(base_size = 12) +
  theme(legend.position = "top")




# Fit Cox model with time-varying exposure
cox_model <- coxph(
  Surv(start, stop, Dementia_event) ~ heartdisease,
  data = cdt_new2
)

# Reference dataset — two groups
newdata_ref <- data.frame(heartdisease = c(0, 1))

# Predicted survival
cox_surv <- survfit(cox_model, newdata = newdata_ref)

# Convert to dataframe
surv_df <- data.frame(
  time  = rep(cox_surv$time, 2),
  surv  = c(cox_surv$surv[,1], cox_surv$surv[,2]),
  lower = c(cox_surv$lower[,1], cox_surv$lower[,2]),
  upper = c(cox_surv$upper[,1], cox_surv$upper[,2]),
  group = rep(
    c("No Heart Disease", "Heart Disease"),
    each = length(cox_surv$time)
  )
)

# Median survival
median_surv <- surv_df %>%
  group_by(group) %>%
  summarise(
    median_time = time[which.min(abs(surv - 0.5))],
    .groups = "drop"
  )

# Plot
ggplot(surv_df, aes(x = time, y = surv,
                    color = group, fill = group)) +
  geom_step(linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15, color = NA) +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values  = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  coord_cartesian(xlim = c(65, 90)) +
  labs(
    title    = "Predicted Cognitive Impairment-free Survival",
    subtitle = "Adjusted for time-varying heart disease exposure (Cox model)",
    x        = "Age (years)",
    y        = "Survival Probability",
    color    = "Heart Disease Status",
    fill     = "Heart Disease Status"
  ) +
  theme_light(base_size = 12) +
  theme(legend.position = "top")






# Heart disease to cognitive impairment
cdt4 <- cdt_new2 %>% 
  group_by(spid) %>% 
  mutate(age_heart_disease =  first(rdintvwrage[heartdisease == 1]),
         
         age_cog_impair =  first(rdintvwrage[Dementia_event == 1]),
         ) %>% 
  ungroup() %>% 
  mutate(age_heart_disease=ifelse(is.na(age_heart_disease), 0, age_heart_disease),
         age_cog_impair=ifelse(is.na(age_cog_impair), 0, age_cog_impair),
         time_fromHearttocog=age_cog_impair-age_heart_disease)
          


# incidence
incidence(data=cdt_new, strata = "s",eventk = Dementia_event)

incidence(data=cdt_new, strata = sex,eventk = Dementia_event)
incidence(data=cdt_new, strata = income_level,eventk = Dementia_event)


# incidence
incidence(data=cdt_new2, strata = "",eventk = heartdisease)

incidence(data=cdt_new2, strata = sex,eventk = heartdisease)
incidence(data=cdt_new2, strata = income_level,eventk = heartdisease)

# incidence
incidence(data=cdt_new3, strata = "",eventk = heartdisease)

incidence(data=cdt_new3, strata = sex,eventk = heartdisease)
incidence(data=cdt_new3, strata = income_level,eventk = heartdisease)














#
same_wave <- cdt_new3 %>%
  filter(heartattack == 1 & Dementia_event == 1)

nrow(same_wave)

# What proportion of all events is this?
nrow(same_wave) / sum(cdt_new3$Dementia_event)


same_wave <- cdt_new3 %>%
  filter(heartdisease == 1 & Dementia_event == 1)

nrow(same_wave)

# What proportion of all events is this?
nrow(same_wave) / sum(cdt_new3$Dementia_event)
