# clear environment
rm(list=ls())

# load packages and functions
source("Script/1 Load pkgs.R")
source("Script/Functions/my_functions.R")

# load processed data
cdt<-read_encrypt_data("Dataset/Processed/cdt_V1_encrypted.RDS")


# arrange variables
cdt<-cdt %>% 
  mutate(spid=spid,
         Wave=Wave.x,
         income=as.numeric(as.character(iatoincim1)),
         education=el1higstschl) %>% 
  select (spid, Wave, id, 1:190, spid) %>% 
  arrange(spid, Wave) %>% 
  group_by(spid) %>% 
  fill(income, .direction="down") %>% 
  fill(education, .direction="down") %>% 
  mutate(race=first(rldracehisp),
         race=ifelse(race==" 5 more than one DKRF primary"|race==" 6 DKRF", NA, as.character(race)),
         smoke_age = first(sdagesrtsmk),
         sex= first(rdgender),
         
         age = ifelse(rd2intvrage=="5 - 85-89"| rd2intvrage=="6 - 90 +","5 - 85+", as.character(rd2intvrage) ),
         
         hh_member=ifelse(hhdhshldnum=="0", NA, as.numeric(as.character(hhdhshldnum))),
         FPL=fpl(household_count = hh_member, income_level = income),
         income_level= case_when(FPL<138 ~"1Low",
                                 FPL>=138 & FPL< 400 ~ "2Moderate",
                                 FPL >=400 ~"3High",
                                 TRUE~NA),
         wt = (as.numeric(as.character(hwcurrweigh)))*0.4535924,
         ht=(as.numeric(as.character(hwhowtallin)))*0.0254 +0.3048*(as.numeric(as.character(hwhowtallft))),
         BMI=wt/(ht^2),
         BMI_new= ifelse(BMI>100, NA, BMI),
         
         educat= case_when(education %in% c(" 1 NO SCHOOLING COMPLETED", " 2 1ST-8TH GRADE"," 3 9TH-12TH GRADE (NO DIPLOMA)" )~"Level 1: Less than High",
                           education %in% c(" 4 HIGH SCHOOL GRADUATE (HIGH SCHOOL DIPLOMA OR EQUIVALENT)")~"Level 2: High school",
                           education %in% c(" 5 VOCATIONAL, TECHNICAL, BUSINESS, OR TRADE SCHOOL CERTIFICATE OR DIPLOMA (BEYOND HIGH SCHOOL LEVEL)", " 6 SOME COLLEGE BUT NO DEGREE"," 7 ASSOCIATE'S DEGREE")~"Level 3: Some college/technical",
                           education %in% c(" 8 BACHELOR'S DEGREE"," 9 MASTER'S, PROFESSIONAL, OR DOCTORAL DEGREE")~"Level 4: Bachelor’s or higher",
                           TRUE~NA),
         # depression
         dep1= case_when(hcdepresan1==" 1 NOT AT ALL"~ 0, 
                         hcdepresan1==" 2 SEVERAL DAYS"~1,
                         hcdepresan1==" 3 MORE THAN HALF THE DAYS"~2,
                         hcdepresan1==" 4 NEARLY EVERY DAY"~3,
                         ),
         dep2= case_when(hcdepresan2==" 1 NOT AT ALL"~ 0, 
                         hcdepresan2==" 2 SEVERAL DAYS"~1,
                         hcdepresan2==" 3 MORE THAN HALF THE DAYS"~2,
                         hcdepresan2==" 4 NEARLY EVERY DAY"~3,
         ),
         dep3= case_when(hcdepresan3==" 1 NOT AT ALL"~ 0, 
                         hcdepresan3==" 2 SEVERAL DAYS"~1,
                         hcdepresan3==" 3 MORE THAN HALF THE DAYS"~2,
                         hcdepresan3==" 4 NEARLY EVERY DAY"~3,
         ),
         dep4= case_when(hcdepresan4==" 1 NOT AT ALL"~ 0, 
                         hcdepresan4==" 2 SEVERAL DAYS"~1,
                         hcdepresan4==" 3 MORE THAN HALF THE DAYS"~2,
                         hcdepresan4==" 4 NEARLY EVERY DAY"~3,
         ),
         
         phq2=dep1+dep2,
         gad2=dep3+dep4,
         
         depressionSym= ifelse(phq2<3, "No/Low","Possible depression"),
         anxSym= ifelse(gad2<3, "No/Low","Possible anxiety"),
         dep_anx=ifelse(depressionSym=="Possible depression"|anxSym=="Possible anxiety", "Yes", "No"),
         
         #physical activity
         pa=ifelse(
           # Either walking or vigorous activity is YES
           paevrgowalk == " 1 YES" | pavigoractv == " 1 YES", "Active","Inactive"
           
           
         ),
         
         pain_cat =cut(as.numeric(as.character(pnpainscale)),
                         breaks = c(-1, 3, 6, 10),  # 0–3 Low, 4–6 Moderate, 7–10 Severe
                         labels = c("Low", "Moderate", "Severe")
         ),
         
         heartattack= ifelse(hcdisescn1==" 2 NO", 0,1),
         heartdisease=ifelse(hcdisescn2==" 2 NO", 0,1),
         highbp=ifelse(hcdisescn3==" 2 NO", 0,1),
         arthritis=ifelse(hcdisescn4==" 2 NO", 0,1),
         osteoporosis=ifelse(hcdisescn5==" 2 NO", 0,1),
         diabetes=ifelse(hcdisescn6==" 2 NO", 0,1),
         lungdisease=ifelse(hcdisescn7==" 2 NO", 0,1),
         stroke=ifelse(hcdisescn8==" 2 NO", 0,1),
         cancer=ifelse(hcdisescn10==" 2 NO", 0,1),
         dementia=ifelse(hcdisescn9==" 2 NO", 0,1),
         heart_conditions = ifelse(heartattack==1 | heartdisease==1, 1,0),
         
         overall_comorbid = sum(heartdisease, arthritis,osteoporosis, diabetes,lungdisease,cancer,highbp,stroke, na.rm = T),
         overall_comorbid_cat = ifelse(overall_comorbid==0, "Zero",ifelse(overall_comorbid==1, "1 condition", ">1 conditions")),
         comorbid= rowSums(cbind(arthritis,diabetes,lungdisease,cancer,highbp,stroke, na.rm = T)),
         comorbid_cat=ifelse(comorbid>1, ">1", "Zero"),
         
         # sleep difficulties
         slp1 = ifelse(hctrbfalbck==" 1 EVERY NIGHT" |hctrbfalbck== " 2 MOST NIGHTS", 1, 0),
         slp2 = ifelse(hcaslep30mn==" 1 EVERY NIGHT" |hcaslep30mn== " 2 MOST NIGHTS", 1, 0),
         slp3 = ifelse(hcsleepmed==" 1 EVERY NIGHT" |hcsleepmed== " 2 MOST NIGHTS", 1, 0),
         
         sleep_diff= ifelse(slp1==1 |slp2==1 |slp3==1, 1,0),
         
         spid2=spid
         )






# ad8 items
ad8_items <- paste0("cp","chgthink", 1:8)

# data cleaning
# select data of participants enrolled in wave 1;
cdt<-cdt %>%
  mutate(
    orient1=ifelse(cgtodaydat1==" 2 NO/DON'T KNOW", 0,1),
    orient2=ifelse(cgtodaydat2==" 2 NO/DON'T KNOW", 0,1),
    orient3=ifelse(cgtodaydat3==" 2 NO/DON'T KNOW", 0,1),
    orient4=ifelse(cgpresidna1==" 2 NO/DON'T KNOW", 0,1),
    orient5=ifelse(cgpresidna3==" 2 NO/DON'T KNOW", 0,1),
    orient6=ifelse(cgvpname1==" 2 NO/DON'T KNOW", 0,1),
    orient7=ifelse(cgvpname3==" 2 NO/DON'T KNOW", 0,1),
    
    orientation_score =orient1+orient2+orient3+orient4+orient5+orient6+orient7,
    memory_score=as.numeric(as.character(cgdwrdimmrc))+ as.numeric(as.character(cgdwrddlyrc)),
    executive_score=as.numeric(cgdclkdraw)-1,
    cog_score = memory_score+orientation_score+executive_score,
    cog_score_z=as.vector(scale(cog_score)),
    orientation_score_z=as.vector(scale(orientation_score)),
    executive_score_z=as.vector(scale(executive_score)),
    memory_score_z=as.vector(scale(memory_score)),
    overall_cog_score = rowMeans(
      cbind(orientation_score_z, memory_score_z, executive_score_z),
      na.rm = TRUE
    )
    ) %>% 
  
  mutate(across(all_of(ad8_items), ~case_when(
    . %in% c(" 1 YES, A CHANGE"," 3 DEMENTIA/ALZHEIMER'S REPORTED BY PROXY") ~ 1,
    . == " 2 NO, NO CHANGE"        ~ 0,
    TRUE          ~ NA_real_
  ))) %>%
  rowwise() %>%
  mutate(ad8_score = sum(c_across(all_of(ad8_items)))) %>%
  ungroup()


# domain wise cognitive impairment measurement
memory_mean <- mean(cdt$memory_score, na.rm=TRUE)
memory_sd   <- sd(cdt$memory_score, na.rm=TRUE)

orientation_mean <- mean(cdt$orientation_score, na.rm=TRUE)
orientation_sd   <- sd(cdt$orientation_score, na.rm=TRUE)

clock_mean <- mean(cdt$executive_score, na.rm=TRUE)
clock_sd   <- sd(cdt$executive_score, na.rm=TRUE)
    

cdt <- cdt %>%
  mutate(
    # Domain impairments (preserve NA if score is NA)
    memory_impair = case_when(
      is.na(memory_score) ~ NA_real_,
      memory_score <= memory_mean - 1.5*memory_sd ~ 1,
      TRUE ~ 0
    ),
    
    orientation_impair = case_when(
      is.na(orientation_score) ~ NA_real_,
      orientation_score <= orientation_mean - 1.5*orientation_sd ~ 1,
      TRUE ~ 0
    ),
    
    clock_impair = case_when(
      is.na(executive_score) ~ NA_real_,
      executive_score <= clock_mean - 1.5*clock_sd ~ 1,
      TRUE ~ 0
    ),
    
    # Count impairments (ignore NA but track all-missing)
    domains_impair = rowSums(
      cbind(memory_impair, orientation_impair, clock_impair),
      na.rm = TRUE
    ),
    
    domains_all_na = if_all(
      c(memory_impair, orientation_impair, clock_impair),
      is.na
    ),
    
    # Doctor diagnosis
    doctor_dementia = case_when(
      is.na(hcdisescn9) ~ NA_real_,
      hcdisescn9 == " 2 NO" ~ 0,
      TRUE ~ 1
    ),
    
    # Probable dementia
    probable_dementia = case_when(
      # ALL relevant variables missing → NA
      domains_all_na & is.na(ad8_score) & is.na(doctor_dementia) ~ NA_real_,
      
      # Otherwise classify using available info
      doctor_dementia == 1 | ad8_score >= 2 | domains_impair >= 2 ~ 1,
      TRUE ~ 0
    ),
    
    # Possible dementia
    possible_dementia = case_when(
      is.na(probable_dementia) ~ NA_real_,
      probable_dementia == 0 & domains_impair == 1 ~ 1,
      probable_dementia == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # No dementia
    no_dementia = case_when(
      is.na(probable_dementia) & is.na(possible_dementia) ~ NA_real_,
      probable_dementia == 0 & possible_dementia == 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Final binary
    Dementia = case_when(
      is.na(no_dementia) ~ NA_real_,
      no_dementia == 0 ~ 1,
      no_dementia == 1 ~ 0
    ),
    
    # Final classification
    dementia_status = case_when(
      is.na(probable_dementia) & is.na(possible_dementia) ~ NA_character_,
      probable_dementia == 1 ~ "Probable",
      possible_dementia == 1 ~ "Possible",
      no_dementia == 1 ~ "No dementia"
    ),
    
    Dementia_event =ifelse(no_dementia==0, 1, 0),
    
    died = ifelse(rstatus==62, 1, 0)
  )



cdt<-cdt %>% 
  group_by(spid) %>%
  arrange(Wave,spid) %>% 
  mutate(died_all = ifelse(any(died == 1), 1, 0)) %>%
  ungroup() %>% 
  mutate(rdintvwrage= ifelse(spid=="10009274" & Wave==1, 85,ifelse(spid=="10009274" & Wave==2, 86,rdintvwrage ) ),
         rdintvwrage= ifelse(spid=="10012378" & Wave==1, 89,ifelse(spid=="10012378" & Wave==2, 90,rdintvwrage ) ),
         enrolled_wave=ifelse(yearsample==2011, 1, ifelse(yearsample==2015, 2, ifelse(yearsample==2022, 3, ifelse(yearsample==2023, 4, NA)))))





# save data (open)
saveRDS(cdt, "Dataset/Processed/cdt_V2.RDS")  

# encrypted (protected)
save_encrypted_data(file = cdt, path ="Dataset/Processed", name = "cdt_v2_encrypted" )
  