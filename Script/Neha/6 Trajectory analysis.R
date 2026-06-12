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
  mutate(year_in_study = Wave-first(Wave),
         baseline_memory=first(cgratememry),
         baseline_depression= first(depressionSym),
         baseline_age= first(rdintvwrage),
         baseline_cog_score= first(cog_score)
  ) %>% 
  filter(baseline_age<80) %>% 
  filter(baseline_memory !=" 5 POOR" ) %>% 
  filter(baseline_cog_score>5) |> 
  ungroup()


# Baseline characteristics of the study participants
cdt %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("sex", "age","race","income_level","BMI_new", "pa", "cog_score"))




# Mixed model
model_lme <- lmer(cog_score ~ year_in_study +age + sex + race+ income_level + BMI_new + pa+educat +sleep_diff+ (year_in_study | spid), data = cdt,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

summary(model_lme)

sjPlot::tab_model(model_lme)





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


k<-gbmt(x.names=c( "cog_score"),
     unit="spid", time="Wave", d=2, ng=2, scaling=1, data=data.frame(cdt2))


plot(k)
m<-rownames_to_column(data.frame(k$assign))

m$rowname<- as.numeric(m$rowname)

cdt_l<-cdt %>% 
  left_join(m, by=c("spid"="rowname"))



cdt_2<-cdt_l |> 
  filter(Wave==1)



cdt_2$traj <- factor(cdt_2$k.assign, levels=c(1,2), labels=c("Constant", "Decreasing"))


cdt_2 %>% 
  filter(Wave==1) %>% 
  tbl_summary(include = c("traj", "age", "sex", "race", "income_level", "educat", "pa", "sleep_diff", "dep_anx"))



model1<-glm(formula = traj~age + sex + race+ income_level +educat+pa+sleep_diff+dep_anx, family = "binomial", data = cdt_2)

sjPlot::tab_model(model1)

