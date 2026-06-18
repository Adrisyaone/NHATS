# secured data save
save_encrypted_data <-function(file, path, name){
  # save encrypted data
  key <- cyphr::key_sodium(sodium::sha256(charToRaw(getPass::getPass("Create a secure data password: "))))
  
  # Encrypt and save the file
  cyphr::encrypt(
    saveRDS(file, paste0(path, "/",name, ".RDS")), 
    key
  )
  
}

# secured data read
read_encrypt_data<-function(path){
  # save encrypted data
  key <- cyphr::key_sodium(sodium::sha256(charToRaw(getPass::getPass("Enter password: "))))
  
  # Encrypt and save the file
  cyphr::decrypt(
    readRDS(path), 
    key
  )
  
}


# Function 0
pull_data<-function(data_list, wave_range=1:14,wt_range=0:56, pattern="^(hc|cg|fl|r|cp|rd|ia|sd|hh|hw|rl|pn|pa|wr)[0-9]+",tracker=c("SP", "OP", "TR", "SEN")){
  # import data from list
  data_list_imported <- lapply(data_list, read_dta)
  
  
  # Extract round/wave numbers from file names
  round_numbers <- str_extract(data_list, "Round_\\d+") |> 
    str_extract("\\d+")
  
  # Name the list
  names(data_list_imported) <- paste0("Round", round_numbers)
  
  # select variables for each waves 
  wave = wave_range
  wt = wt_range
  
  
  # Apply variable selection to all datasets in the list
  data_selected <- lapply(names(data_list_imported), function(w) {
    wave_num <- as.numeric(str_extract(w, "\\d+"))
    vars <- get_vars_wave(data_file = tracker, wave_num, wt)
    
    # Keep only existing variables to avoid errors if some are missing
    vars <- vars[vars %in% names(data_list_imported[[w]])]
    
    data_list_imported[[w]][, vars]
  })
  
  names(data_selected) <- names(data_list_imported)
  
  
  # Add wave number column to each dataset
  data_selected <- lapply(names(data_selected), function(w) {
    
    wave_num <- as.numeric(str_extract(w, "\\d+"))  # extract wave number from name
    
    data_selected[[w]] %>%
      mutate(Wave = wave_num)  # add new column 'Wave'
  })
  names(data_selected) <- names(data_list_imported)
  
  
  # remove prefix from the names of the 
  mydt <- lapply(data_selected, function(x) {
    names(x) <- sub(pattern, "\\1", names(x))
    x
  })
  
  
  # convert data to long from 1 to wave 4
  combined_data <- do.call(bind_rows, mydt)
  
  
  # remove negative codings
  combined_data [combined_data<0] <-NA
  
  # convert to factor
  final_data<-as_factor(combined_data, only_labelled = T)
 
  # generate unique id
  final_data$id<- paste0(final_data$spid, "-", final_data$Wave)
  
  # return the final data
  return(final_data)
}






# Function 1
# function to determine income status
fpl <- function(household_count, income_level){
  case_when(
    household_count == 1 ~ income_level / 15650 * 100,
    household_count == 2 ~ income_level / 21150 * 100,
    household_count == 3 ~ income_level / 26650 * 100,
    household_count == 4 ~ income_level / 32150 * 100,
    household_count == 5 ~ income_level / 37650 * 100,
    household_count == 6 ~ income_level / 43150 * 100,
    household_count == 7 ~ income_level / 48650 * 100,
    household_count == 8 ~ income_level / 54150 * 100,
    household_count > 8  ~ income_level / (54150 + 5500 * (household_count - 8)) * 100,
    TRUE ~ NA_real_ 
  )
}


# Function 2
# Function to calculate incidence
incidence <- function(data, strata, eventk) {
  library(dplyr)
  data %>%
    mutate(time = stop - start) %>%
    group_by({{ strata }}) %>%
    summarise(
      events       = sum({{ eventk }}, na.rm = TRUE),   # BUG 1 FIX: embrace eventk
      person_years = sum(time, na.rm = TRUE),
      
      ir_per_1000  = (events / person_years) * 1000,
      
      # BUG 2 FIX: use Poisson-based CI (sqrt(events)/person_years)
      # not (sqrt(events)/person_years) applied to events±1.96*sqrt(events)
      lower_ci = (events / person_years) * exp(-1.96 / sqrt(events)) * 1000,
      upper_ci = (events / person_years) * exp( 1.96 / sqrt(events)) * 1000
    ) %>%
    ungroup() %>% 
    
    # BUG 3 FIX: round for readability and prevent negative CIs
    mutate(across(c(ir_per_1000, lower_ci, upper_ci), ~ round(., 2)),
           lower_ci = pmax(lower_ci, 0))   # prevents negative lower CI
}


# Function
# function to handle interaction
extract_interaction_hr <- function(model,
                                   exposure,
                                   modifier) {
  
  library(survey)
  
  coefs <- coef(model)
  vc <- vcov(model)
  
  # ---- identify terms ----
  exposure_term <- exposure
  
  interaction_terms <- grep(
    paste0(exposure, ":", modifier),
    names(coefs),
    value = TRUE
  )
  
  # ---- reference group (baseline modifier) ----
  beta_ref <- coefs[exposure_term]
  se_ref <- sqrt(vc[exposure_term, exposure_term])
  
  results <- data.frame(
    Group = "Reference (baseline modifier)",
    HR = exp(beta_ref),
    CI_low = exp(beta_ref - 1.96 * se_ref),
    CI_high = exp(beta_ref + 1.96 * se_ref)
  )
  
  # ---- loop through modifier levels ----
  for (int_term in interaction_terms) {
    
    beta <- coefs[exposure_term] + coefs[int_term]
    
    var <- vc[exposure_term, exposure_term] +
      vc[int_term, int_term] +
      2 * vc[exposure_term, int_term]
    
    se <- sqrt(var)
    
    # clean group name
    group_name <- gsub(paste0(exposure, ":"), "", int_term)
    
    results <- rbind(results, data.frame(
      Group = group_name,
      HR = exp(beta),
      CI_low = exp(beta - 1.96 * se),
      CI_high = exp(beta + 1.96 * se)
    ))
  }
  
  rownames(results) <- NULL
  return(results)
}