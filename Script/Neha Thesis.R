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

cdt$dep_sleep_cat <- with(cdt,
                                     ifelse(dep_anx == "Yes" & sleep_diff == 1, "Dep_Yes_Sleep_Yes",
                                            ifelse(dep_anx == "No"  & sleep_diff == 1, "Dep_No_Sleep_Yes",
                                                   ifelse(dep_anx == "Yes" & sleep_diff == 0, "Dep_Yes_Sleep_No",
                                                          "Dep_No_Sleep_No")))
)


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

# incidence of cognitive decline
incidence(data = cdt_surv_clean, strata = age)


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


# Model 1
cox_model <- svycoxph(
  Surv(start, stop, Dementia_event) ~  age + sex + race+ income_level +educat+pa+sleep_diff+dep_anx ,
  design = design
)
summary(cox_model)



table(cdt_surv_clean$dep_sleep_cat)
cdt_surv_clean$dep_sleep_cat <- factor(
  cdt_surv_clean$dep_sleep_cat,
  levels = c("Dep_No_Sleep_No",
             "Dep_No_Sleep_Yes",
             "Dep_Yes_Sleep_No",
             "Dep_Yes_Sleep_Yes")
)

design <- svrepdesign(
  id=~spid,
  weights = ~trbswgt0,
  
  repweights = "trbswgt[1-56]",
  data = cdt_surv_clean,
  type = "Fay",
  rho=0.3,
  combined.weights = TRUE
)

#Model 3

design_male <- subset(design, sex=="1 MALE")
design_female <- subset(design, sex=="2 FEMALE")

cox_model_male <- svycoxph(
  Surv(start, stop, Dementia_event) ~ dep_sleep_cat + pa + age + race + income_level + educat,
  design = design_male
)
summary(cox_model_male)

cox_model_female <- svycoxph(
  Surv(start, stop, Dementia_event) ~ dep_sleep_cat + pa + age + race + income_level + educat,
  design = design_female
)
summary(cox_model_female)

#Model 2
cox_model2 <- svycoxph(
  Surv(start, stop, Dementia_event) ~  dep_sleep_cat + pa + age + sex + race+ income_level +educat ,
  design = design
)
summary(cox_model2)

table(cdt_surv_clean$sex)


library(survey)
library(survival)
install.packages("survminer")
library(survminer)

cdt_surv_clean$time <- cdt_surv_clean$stop - cdt_surv_clean$start

km_fit <- svykm(
  Surv(start, stop, Dementia_event) ~ dep_sleep_cat,
  design = design,
  se = TRUE
)
km_fit <- svykm(
  Surv(time, Dementia_event) ~ dep_sleep_cat,
  design = design,
  se = FALSE
)

plot(km_fit,
     col = 1:length(unique(cdt_surv_clean$dep_sleep_cat)),
     lty = 1,
     xlab = "Time",
     ylab = "Survival Probability",
     main = "Survey-weighted KM Curve by Depression-Sleep Category")

legend("bottomleft",
       legend = levels(as.factor(cdt_surv_clean$dep_sleep_cat)),
       col = 1:length(levels(as.factor(cdt_surv_clean$dep_sleep_cat))),
       lty = 1)



# traj package
# install.packages("traj")
library(traj)


cdt2 <- cdt %>%
  filter(enrolled_wave==1) %>% 
  select(spid, Wave,orientation_score,memory_score, executive_score, cog_score) 



install.packages("gbmt")
library(gbmt)



cdt2<-cdt2 %>% 
  group_by(spid) %>% 
  mutate(count=n()) %>% 
  ungroup() %>% 
  filter(count>=3)

cdt2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("cog_score"))


k <- gbmt(
  x.names = c("cog_score"),
  unit = "spid",
  time = "Wave",
  d = 1,
  ng = 2,
  scaling = 0,
  data = data.frame(cdt2)
)

plot(
  k,
  xlab = "NHATS Follow-up Waves",
  ylab = "Mean Cognitive Score",
)


m<-rownames_to_column(data.frame(k$assign))

m$rowname<- as.numeric(m$rowname)

cdt_l<-cdt %>% 
  left_join(m, by=c("spid"="rowname"))

k3<-gbmt(x.names=c( "cog_score"),
         unit="spid", time="Wave", d=1, ng=3, scaling=0, data=data.frame(cdt2))


plot(k3)

m3 <- rownames_to_column(data.frame(k3$assign))

m3$rowname <- as.numeric(m3$rowname)


cdt_2<-cdt_l |> 
  filter(Wave==1)

cdt_2$dep_sleep_cat <- with(cdt_2,
                          ifelse(dep_anx == "Yes" & sleep_diff == 1, "Dep_Yes_Sleep_Yes",
                                 ifelse(dep_anx == "No"  & sleep_diff == 1, "Dep_No_Sleep_Yes",
                                        ifelse(dep_anx == "Yes" & sleep_diff == 0, "Dep_Yes_Sleep_No",
                                               "Dep_No_Sleep_No")))
)

cdt_2$traj <- factor(cdt_2$k.assign, levels=c(1,2), labels=c("Constant", "Decreasing"))


cdt_2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("traj", "age", "sex", "race", "income_level", "educat", "pa", "sleep_diff", "dep_anx" , "dep_sleep_cat"))

summary(cdt_2$traj)

model1<-glm(formula = traj~age + sex + race+ income_level +educat+pa+dep_sleep_cat, family = "binomial", data = cdt_2)

sjPlot::tab_model(model1)

table(cdt_2$pa)
cdt_active <- subset(cdt_2, pa == "Active")
cdt_inactive <- subset(cdt_2, pa == "Inactive")
model_active <- glm(
  traj ~ age + sex + race + income_level + educat + dep_sleep_cat,
  family = "binomial",
  data = cdt_active
)
model_inactive <- glm(
  traj ~ age + sex + race + income_level + educat + dep_sleep_cat,
  family = "binomial",
  data = cdt_inactive
)
sjPlot::tab_model(
  model_active,
  model_inactive,
  dv.labels = c("Physically Active", "Physically Inactive")
)


#Paper 3, SEM

library(tidyverse)

sem_data <- cdt %>%
  filter(Wave %in% c(1,2,3)) %>%
  select(
    spid,
    Wave,
    cog_score,
    dep_anx,
    sleep_diff,
    pa,
    age,
    sex,
    race,
    income_level,
    educat
  ) %>%
  pivot_wider(
    id_cols = c(
      spid,
      age,
      sex,
      race,
      income_level,
      educat
    ),
    names_from = Wave,
    values_from = c(
      cog_score,
      dep_anx,
      sleep_diff,
      pa
    )
  )
str(sem_data$dep_anx_1)
str(sem_data$pa_1)
sem_data$dep_num_1 <- ifelse(sem_data$dep_anx_1 == "Yes", 1, 0)
sem_data$dep_num_2 <- ifelse(sem_data$dep_anx_2 == "Yes", 1, 0)
sem_data$pa_num_1 <- ifelse(sem_data$pa_1 == "Inactive", 1, 0)
sem_data$pa_num_2 <- ifelse(sem_data$pa_2 == "Inactive", 1, 0)
sem_data <- sem_data %>%
  mutate(
    dep_pa_1 = dep_num_1 * pa_num_1,
    dep_pa_2 = dep_num_2 * pa_num_2
  )

table(sem_data$dep_num_1)
table(sem_data$pa_num_1)

summary(sem_data$dep_pa_1)
names(sem_data)

str(sem_data)

class(sem_data$dep_anx_1)
class(sem_data$pa_1)
install.packages("lavaan")
install.packages("semPlot")

str(sem_data[, c("dep_anx_1", "dep_anx_2", "pa_1", "pa_2")])

sem_data <- sem_data %>%
  mutate(
    dep_num_1 = ifelse(dep_anx_1 == "Yes", 1, 0),
    dep_num_2 = ifelse(dep_anx_2 == "Yes", 1, 0),
    dep_num_3 = ifelse(dep_anx_3 == "Yes", 1, 0),
    
    pa_num_1 = ifelse(pa_1 == "Inactive", 1, 0),
    pa_num_2 = ifelse(pa_2 == "Inactive", 1, 0), pa_num_3 = ifelse(pa_3 == "Inactive", 1, 0)
  )


sem_data <- sem_data %>%
  mutate(
    dep_pa_1 = dep_num_1 * pa_num_1,
    dep_pa_2 = dep_num_2 * pa_num_2,
    dep_pa_3 = dep_num_3 * pa_num_3
  )
library(lavaan)
library(semPlot)

#Model A
#Basic Cross-Lagged Panel Model

#Tests:
  
 # Depression → Cognition
#Cognition → Depression

clpm_model <- '

################################################
# Autoregressive paths
################################################

dep_num_2 ~ a1*dep_num_1
dep_num_3 ~ a2*dep_num_2

cog_score_2 ~ b1*cog_score_1
cog_score_3 ~ b2*cog_score_2

################################################
# Cross-lagged paths
################################################

cog_score_2 ~ c1*dep_num_1
cog_score_3 ~ c2*dep_num_2

dep_num_2 ~ d1*cog_score_1
dep_num_3 ~ d2*cog_score_2

################################################
# Covariates
################################################

dep_num_2 ~ age + sex + race + income_level + educat
dep_num_3 ~ age + sex + race + income_level + educat

cog_score_2 ~ age + sex + race + income_level + educat
cog_score_3 ~ age + sex + race + income_level + educat

'
fit_clpm <- sem(
  clpm_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml"
)

summary(
  fit_clpm,
  standardized = TRUE,
  fit.measures = TRUE
)

#Model B
#Add Sleep as Mediator

mediation_model <- '

################################################
# Stability
################################################

dep_num_2 ~ dep_num_1
dep_num_3 ~ dep_num_2

sleep_diff_2 ~ sleep_diff_1
sleep_diff_3 ~ sleep_diff_2

cog_score_2 ~ cog_score_1
cog_score_3 ~ cog_score_2

################################################
# Mediation pathway
################################################

sleep_diff_2 ~ a*dep_num_1
sleep_diff_3 ~ a*dep_num_2

cog_score_2 ~ b*sleep_diff_1
cog_score_3 ~ b*sleep_diff_2

################################################
# Direct pathway
################################################

cog_score_2 ~ c*dep_num_1
cog_score_3 ~ c*dep_num_2

################################################
# Indirect effect
################################################

indirect := a*b

'
fit_med <- sem(
  mediation_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml"
)

summary(
  fit_med,
  standardized = TRUE,
  fit.measures = TRUE
)

#Model C
#Add Physical Activity Moderation

moderation_model <- '

################################################
# Stability
################################################

cog_score_2 ~ cog_score_1
cog_score_3 ~ cog_score_2

################################################
# Main effects
################################################

cog_score_2 ~ dep_num_1 +
              pa_num_1 +
              dep_pa_1

cog_score_3 ~ dep_num_2 +
              pa_num_2 +
              dep_pa_2

'
fit_mod <- sem(
  moderation_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml"
)

summary(
  fit_mod,
  standardized = TRUE,
  fit.measures = TRUE
)

#Model D
#Full Model (Cross-Lagged + Mediation + Moderation)

full_model <- '

################################################
# Depression stability
################################################

dep_num_2 ~ dep_num_1
dep_num_3 ~ dep_num_2

################################################
# Sleep stability
################################################

sleep_diff_2 ~ sleep_diff_1
sleep_diff_3 ~ sleep_diff_2

################################################
# Cognition stability
################################################

cog_score_2 ~ cog_score_1
cog_score_3 ~ cog_score_2

################################################
# Depression -> Sleep
################################################

sleep_diff_2 ~ a*dep_num_1
sleep_diff_3 ~ a*dep_num_2

################################################
# Sleep -> Cognition
################################################

cog_score_2 ~ b*sleep_diff_1
cog_score_3 ~ b*sleep_diff_2

################################################
# Depression -> Cognition
################################################

cog_score_2 ~ c*dep_num_1
cog_score_3 ~ c*dep_num_2

################################################
# Cognition -> Depression
################################################

dep_num_2 ~ d*cog_score_1
dep_num_3 ~ d*cog_score_2

################################################
# Physical activity moderation
################################################

cog_score_2 ~ pa_num_1 + dep_pa_1
cog_score_3 ~ pa_num_2 + dep_pa_2

################################################
# Covariates
################################################

cog_score_2 ~ age + sex + race +
              income_level + educat

cog_score_3 ~ age + sex + race +
              income_level + educat

################################################
# Indirect effect
################################################

indirect := a*b

'
fit_full <- sem(
  full_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml"
)

summary(
  fit_full,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)

#Step 6. Examine Model Fit
fitMeasures(
  fit_full,
  c(
    "cfi",
    "tli",
    "rmsea",
    "srmr"
  )
)

#Fit Diagram
semPaths(
  fit_full,
  what = "std",
  layout = "tree",
  edge.label.cex = 1,
  residuals = FALSE,
  intercepts = FALSE
)

library(tidySEM)

semPlot::semPaths(
  fit_full,
  what = "std",
  layout = "tree",
  residuals = FALSE,
  intercepts = FALSE
)



library(semPlot)

semPaths(
  fit_full,
  what = "std",
  whatLabels = "std",
  style = "lisrel",
  layout = "tree2",        # IMPORTANT upgrade from "tree"
  rotation = 2,            # left → right flow
  intercepts = FALSE,
  residuals = FALSE,
  nCharNodes = 0,
  sizeMan = 6,
  edge.label.cex = 0.8,
  fade = FALSE
)

library(dplyr)

cdt_long <- cdt %>%
  arrange(spid, Wave) %>%
  group_by(spid) %>%
  mutate(
    
    cog_prev   = lag(cog_score),
    
    dep_prev = lag(
      ifelse(dep_anx=="Yes",1,0)
    ),
    
    sleep_prev = lag(sleep_diff),
    
    pa_prev = lag(
      ifelse(pa=="Inactive",1,0)
    )
    
  ) %>%
  ungroup()
cdt_long <- cdt_long %>%
  mutate(
    dep_pa_prev = dep_prev * pa_prev
  )
library(lme4)

model_long <- lmer(
  cog_score ~
    cog_prev +
    dep_prev +
    sleep_prev +
    pa_prev +
    dep_pa_prev +
    age +
    sex +
    race +
    educat +
    income_level +
    (1|spid),
  data = cdt_long
)

summary(model_long)

library(lmerTest)

model_long <- lmer(
  cog_score ~
    cog_prev +
    dep_prev +
    sleep_prev +
    pa_prev +
    dep_pa_prev +
    age +
    sex +
    race +
    educat +
    income_level +
    (1 | spid),
  data = cdt_long
)

summary(model_long)

library(gtsummary)
library(dplyr)

baseline <- cdt %>%
  filter(Wave == 1)

tbl1 <- baseline %>%
  select(
    age,
    sex,
    race,
    income_level,
    educat,
    pa,
    dep_anx,
    sleep_diff,
    cog_score
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

tbl1
