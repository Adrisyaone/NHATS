# Step-0: Clear environment
rm(list=ls())

# Step-1.1: Install and load library
source("Script/1 Load pkgs.R")

# Step-1.2:load functions
source("Script/!!!Functions/my_functions.R")


# Step-2: Set your working directory to unzip and select all required files
dir.create("Dataset/raw data", recursive = TRUE, showWarnings = FALSE)
dir.create("Dataset/raw data full", recursive = TRUE, showWarnings = FALSE)
dir.create("Dataset/raw data/Sensitive data/", recursive = TRUE, showWarnings = FALSE) # for sensitive data
dir.create("Dataset/raw data full/Sensitive data", recursive = TRUE, showWarnings = FALSE) # for sensitive data


# Step-3 Get all zip files of unsenstitive data
zf <- list.files(path = "Dataset/raw data",pattern = "\\.zip$", full.names = T)


# Step-4 Unzip all files
for (z in zf) {
  unzip(zipfile = z, exdir = "Dataset/raw data full", overwrite = T)
}



# Step-5 Get all zip files for sensititve data
sen_data <- list.files(path = "Dataset/raw data/Sensitive data/",pattern = "\\.zip$", full.names = T)

# Step-6 Unzip all  sensitive files
for (z in sen_data) {
  unzip(zipfile = z, exdir = "Dataset/raw data full/Sensitive data", overwrite = T)
}



# 3. List all extracted files
all_files <- list.files(recursive = TRUE, full.names = TRUE)


# sp data
data_sp <- list.files(pattern="SP_File.*\\.dta$", 
                      full.names = T, recursive = T,
                      ignore.case = TRUE)

# op data
data_op <-list.files(pattern="OP_File.*\\.dta$", 
                     full.names = T, recursive = T,
                     ignore.case = TRUE)

# tracker data
data_tracking <-list.files(pattern="Tracker.*\\.dta$", 
                           full.names = T, recursive = T,
                           ignore.case = TRUE)

# sensitive data
data_sen_sp <- list.files(pattern="SP_Sen_Dem_File.*\\.dta$", 
                          full.names = T, recursive = T,
                          ignore.case = TRUE)

# List out the variables name
# Function to generate variable names for a given wave
get_vars_wave <-  function(data_file=c("TR", "SP", "OP", "SEN"), wave, wt){
  wave=wave
  wt=wt
  if(data_file=="SP"){
    k<-c(
      paste0("cg", wave, "dwrdimmrc"), # Immediate Word Recall Score
      paste0("cg", wave, "dwrddlyrc"), # Delayed Word Recall Score
      paste0("cg", wave, "todaydat1"), # Today's Date Correct Month
      paste0("cg", wave, "todaydat2"), # Today's Date Correct Day
      paste0("cg", wave, "todaydat3"), # Today's Date Correct Year
      paste0("cg", wave, "presidna1"), # President Last Name Correct
      paste0("cg", wave, "presidna3"), # President First Name Correct
      paste0("cg", wave, "vpname1"), # Vice President Last Name Correct
      paste0("cg", wave, "vpname3"), # Vice President First Name Correct
      paste0("cg", wave, "dclkdraw"), # SCORE OF CLOCK DRAWING TEST
      paste0("cg", wave, "atdrwclc"), #ATTEMPT CLOCK DRAWI
      paste0("cg", wave, "ratememry"), # Self-Rated Memory
      paste0("cg", wave, "memcom1yr"), # Memory Compared to 1 Year Ago
      paste0("cp", wave, "chgthink7"), # Problems with Judgment
      paste0("cp", wave, "chgthink5"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink8"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink6"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink4"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink3"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink2"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "chgthink1"), # Daily Problems with Thinking/Memory
      paste0("cp", wave, "memcogpr1"), # LOST IN FAMILIAR ENVIRO
      paste0("cp", wave, "memcogpr4"), #SP HEARS SEES THNGS
      paste0("hc", wave, "aslep30mn"), # In the last month, how often did it take you more than 30 minutes to fall asleep at night?
      paste0("hc", wave, "trbfalbck"), # Trouble Falling Back Asleep
      paste0("hc", wave, "sleepmed"), # Sleep Medication Use
      paste0("hc", wave, "depresan1"), # Little Interest or Pleasure
      paste0("hc", wave, "depresan2"), # Felt Down, Depressed, or Hopeless
      paste0("hc", wave, "depresan3"), # Felt Nervous or Anxious
      paste0("hc", wave, "depresan4"), # Unable to Stop or Control Worrying
      paste0("r", wave, "d2intvrage"), # age
      paste0("r", wave, "dintvwrage"),
      paste0("rl", wave, "dracehisp"), # race
      if (wave == 1) "r1dgender" else NULL, # Gender
      paste0("ia", wave, "totinc"),
      paste0("ia", wave, "toincim1"),
      paste0("ia", wave, "toincesjt"),
      paste0("el", wave, "eincimj1"),
      
      paste0("hh", wave, "dhshldnum"),
      paste0("hw", wave, "currweigh"),
      paste0("hw", wave, "howtallft"),
      paste0("hw", wave, "howtallin"),
      paste0("pn", wave, "painscale"),
      paste0("pa", wave, "evrgowalk"),
      paste0("pa", wave, "vigoractv"),
      
      
      paste0("el", wave, "higstschl"),
      
      paste0("sd", wave, "smokesnow"),
      paste0("sd", wave, "numcigday"),
      paste0("sd", wave, "agesrtsmk"),
      
      
      paste0("hc", wave, "disescn1"), # Heart attack
      paste0("hc", wave, "disescn2"), # Heart Disease
      paste0("hc", wave, "disescn3"), # High BP
      paste0("hc", wave, "disescn4"), # arthritis
      paste0("hc", wave, "disescn5"), # osteoporosis
      paste0("hc", wave, "disescn6"), # Diabetes
      paste0("hc", wave, "disescn7"), # Lung disease
      paste0("hc", wave, "disescn8"), # stroke
      paste0("hc", wave, "disescn10"), # Cancer
      paste0("hc", wave, "disescn9"), # Dementia or Alzheimer’s Disease
      paste0("hc", wave, "dementage"), # Age told had dementia
      
      paste0 ("hc", wave, "health"), # overall health ratig
      paste0 ("fl", wave, "newsample"), # new sample added
      paste0 ("r", wave, "dcontnew"), # continued or new sample
      paste0 ("r", wave, "status"), # continued or new sample
      
      paste0 ("cp", wave, "dad8dem"), # continued or new sample
      
      "spid"
    )
  }
  else if(data_file=="TR"){
    k<- c(paste0 ("w", wave, "varstrat"), # w1varstrat
          paste0 ("w", wave, "varunit"), # w1varunit
          paste0 ("r", wave, "status"), # w1varunit
          paste0 ("r", wave, "casestdtmt"), # w1varunit
          paste0 ("r", wave, "casestdtyr"), # w1varunit
          paste0 ("w", wave, "trbswgt",wt ), # w1varunit
          paste0 ("w", wave, "trfinwgt",wt ), # w1varunit
          
          "yearsample",
          
          "spid"
    )
  }
  
  else if(data_file=="SEN"){
    k<-c(
      paste0("r", wave, "dintvwrage"), # Immediate Word Recall Score
      
      "spid"
    )
    
  }
  else {
    k<-NULL
  }
}



# Pull sp data

sp_data<-pull_data(data_sp, wave_range=1:14,wt_range = 0:56, pattern="^(hc|cg|fl|r|cp|rd|ia|sd|hh|hw|rl|pn|pa|wr)[0-9]+", tracker = "SP")

tr_data<-pull_data(data_tracking, wave_range=1:14,wt_range = 0:56, pattern="^(hc|cg|fl|r|cp|rd|ia|sd|hh|hw|rl|pn|pa|wr)[0-9]+", tracker = "TR")

sen_data <-pull_data(data_sen_sp, wave_range=1:14,wt_range = 0:56, pattern="^(hc|cg|fl|r|cp|rd|ia|sd|hh|hw|rl|pn|pa|wr)[0-9]+", tracker = "SEN")


# final_data
cdt<-sp_data |> 
  left_join(tr_data , by="id") %>% 
  mutate(spid=spid.y,
         spid.y=spid.x=NULL) %>% 
  left_join(sen_data, by="id") %>% 
  mutate(spid=spid.y,
         spid.y=spid.x=NULL)


# save data (open)
saveRDS(cdt, "Dataset/Processed/cdt_V1.RDS")

# encrypted
save_encrypted_data(file = cdt, path ="Dataset/Processed", name = "cdt_v1_encrypted" )
