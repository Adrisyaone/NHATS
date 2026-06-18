# clear environment
rm(list=ls())

# load packages and functions
source("Script/1 Load pkgs.R")

# load processed data
cdt<-readRDS("Dataset/Processed/cdt_V2.RDS")



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
