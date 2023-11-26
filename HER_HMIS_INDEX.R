# Load required packages -----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Read data ------------------------------------------------------------------------------
data_path <- "./input/HER_2023_Q4-2023_PATIENT_VERIFICATION_ANNOTATED_PMT Revised.xlsx"


# Pre-Process -------------------------------------------------------------
data <- read_excel(data_path, sheet = "data", guess_max = 5000000) %>% 
  mutate(
    `HF Category` = case_when(
      HF_Type_based_on_sample %in% c("CHC", "CHC+", "BHC", "SHC", "DH") ~ "BPHS",
      TRUE ~ "EPHS"
    ),
    did_not_lacate_household = case_when(
      Did_you_locate_the_household_of_the_patient %in% "No" & 
        Please_verify_with_the_community_elder_Or_CHW_Or_Mullah_Or_Other_person_why_you_could_not_locate_the_household %in% "No one knows about the HH/it never existed" ~ "No",
      !is.na(Please_verify_with_the_community_elder_Or_CHW_Or_Mullah_Or_Other_person_why_you_could_not_locate_the_household) ~ Please_verify_with_the_community_elder_Or_CHW_Or_Mullah_Or_Other_person_why_you_could_not_locate_the_household, 
      TRUE ~ "Yes"
    ), 
    date_verified = case_when(
      Did_you_visit_the_health_facility_to_receive_the_mentioned_service_3 == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    service_verified = case_when(
      Did_you_visit_the_health_facility_to_receive_the_mentioned_service_2 == 1 ~ "Yes",
      TRUE ~ "NO"
    ),
    excluded = case_when(
      Please_verify_with_the_community_elder_Or_CHW_Or_Mullah_Or_Other_person_why_you_could_not_locate_the_household %in% "No one knows about the HH/it never existed" ~ "No",
      Can_we_speak_with_sampled_patient != "No one in the household give consent for the interview" & Consent %in% "Yes" ~ "No",
      TRUE ~ "Yes"
    ),
    `Service Provider` = SP_Name_based_on_sample
  )
  
# Filter excluded data
data_located <- data %>% filter(excluded == "No")

###########################################################################
###########################################################################
# MA Accuracy Score -------------------------------------------------------

# Accuracy score HF category
accuracy_score_MA_hf_cat <- data_located %>% 
  group_by(Province, `Service Provider`, Service_Type_Sample, `HF Category`) %>% 
  summarise(
    score = sum(service_verified == "Yes") / n()
  ) %>% ungroup() %>% 
  pivot_wider(names_from = Service_Type_Sample, values_from = score) %>% 
  rowwise() %>% 
  mutate(
    Overall = mean(c_across(is.numeric), na.rm = T)
  ) %>% ungroup() %>% 
  select(Province, `Service Provider`, `HF Category`, 
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`, Overall
         )


# Split BPHS and EPHS results
accuracy_score_MA_bphs <- accuracy_score_MA_hf_cat %>% filter(`HF Category` == "BPHS")
accuracy_score_MA_ephs <- accuracy_score_MA_hf_cat %>% filter(`HF Category` == "EPHS")

# Accuracy score Overall
accuracy_score_MA_overall <- data_located %>% 
  group_by(Province, `Service Provider`, Service_Type_Sample) %>% 
  summarise(
    score = sum(service_verified == "Yes") / n()
  ) %>% ungroup() %>% 
  pivot_wider(names_from = Service_Type_Sample, values_from = score) %>% 
  rowwise() %>% 
  mutate(
    Overall = mean(c_across(is.numeric), na.rm = T)
  ) %>% ungroup() %>% 
  select(Province, `Service Provider`, 
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`, Overall
  )

# list of outputs
accuracy_score_list <- list(
  accuracy_score_MA_bphs = accuracy_score_MA_bphs,
  accuracy_score_MA_ephs = accuracy_score_MA_ephs,
  accuracy_score_MA_overall = accuracy_score_MA_overall
)

openxlsx::write.xlsx(accuracy_score_list, "output/accuracy_score.xlsx")

# Question: Should we use the average of verified service and verified data in accuracy calculation - Below is a sample script

#### accuracy score Service and Date mean
# accuracy_score_MA_hf_cat <- data_located %>%
#   group_by(Province, `Service Provider`, Service_Type_Sample, `HF Category`) %>%
#   summarise(
#     service = sum(service_verified == "Yes") / n(),
#     date = sum(date_verified == "Yes") / n(),
#   ) %>% rowwise() %>%
#   mutate(
#     score = mean(service, date) * 100
#     # score = sum(service_verified == "Yes") / n() * 100
#   ) %>% ungroup() %>%
#   select(-c(service, date)) %>%
#   pivot_wider(names_from = Service_Type_Sample, values_from = score) %>% 
#   ungroup() %>% 
#   select(Province, `Service Provider`, 
#          `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
#          `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
#          `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
#          `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
#          `Under 5 Morbidities`, `C-section` , `Major surgery`
#   )


###########################################################################
###########################################################################
# Household Located -------------------------------------------------------

# HF Category
households_located_MA_hf_cat <- data %>% 
  group_by(Province, `Service Provider`, Service_Type_Sample, `HF Category`) %>% 
  summarise(
    score = sum(did_not_lacate_household == "Yes") / n()
  ) %>% 
  pivot_wider(names_from = Service_Type_Sample, values_from = score) %>% 
  select(Province, `Service Provider`,  `HF Category`,
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`
  )

# Split BPHS and EPHS results
households_located_MA_bphs <- households_located_MA_hf_cat %>% filter(`HF Category` == "BPHS")
households_located_MA_ephs <- households_located_MA_hf_cat %>% filter(`HF Category` == "EPHS")


# Overall 
households_located_MA_overall <- data %>% 
  group_by(Province, `Service Provider`, Service_Type_Sample) %>% 
  summarise(
    score = sum(did_not_lacate_household == "Yes") / n()
  ) %>% 
  pivot_wider(names_from = Service_Type_Sample, values_from = score) %>% 
  select(Province, `Service Provider`,
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`
  )


# list of outputs
hh_located_list <- list(
  households_located_MA_bphs = households_located_MA_bphs,
  households_located_MA_ephs = households_located_MA_ephs,
  households_located_MA_overall = households_located_MA_overall
)


# Export the result
openxlsx::write.xlsx(hh_located_list, "output/located_households.xlsx")


###########################################################################
###########################################################################
# Particip Accuracy Score - Data Not Available ----------------------------------


###########################################################################
###########################################################################
# Consistency Score -------------------------------------------------------

# Question: For calculation of the consistency score, should we use the sum of the 3 months or the average? 
# and if we use the average is it better to take the average of the counts or average of score of the 3 months?

# Note: Register not Available in Qol Hir SHC (2772) - the facility doesn't provide the service. Excluded from the analysis 


### variables used:
# Number_Visits_In_The_Health_Register_Month1 - April 2023
# Number_Visits_In_The_Health_Register_Month2 - May 2023
# Number_Visits_In_The_Health_Register_Month3 - June 2023
# Total_Verified_Visits
# 
# Number_Visits_Miar_Hmir_Month1 - April 2023
# Number_Visits_Miar_Hmir_Month2 - May 2023
# Number_Visits_Miar_Hmir_Month3 - June 2023
# 
# Register_Available
# This_Facility_Provide

# Read Data
sample_ver_data <- read_excel("input/HER_Service_Assessment_and_Sampling_Verification_cleaned_approved.xlsx", sheet = "rep_service")


# Pre-process data
sample_ver_data <- sample_ver_data %>% 
  rowwise() %>% 
  mutate(
    total_Miar_Hmir_visits = sum(Number_Visits_Miar_Hmir_Month1, Number_Visits_Miar_Hmir_Month2, Number_Visits_Miar_Hmir_Month3, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    `Service Provider` = SP_Name_based_on_sample,
    
    `HF Category` = case_when(
      HF_Type_based_on_sample %in% c("CHC", "CHC+", "BHC", "SHC", "DH") ~ "BPHS",
      TRUE ~ "EPHS"
    )
  ) %>% 
  filter(Register_Available %in% "Yes")

# Consistency score HF Category
# formula = If HF register cases are higher: HMIS cases / HF register cases
#           If HMIS cases are higher: HF register cases / HMIS cases
consistency_score_MA_hf_cat <- sample_ver_data %>% 
  group_by(Province, `Service Provider`, Type_of_service_name, `HF Category`) %>% 
  summarise(
    hf_total = sum(Total_Verified_Visits, na.rm = T),
    hmis_total = sum(total_Miar_Hmir_visits, na.rm = T),
    
    score = if(hf_total > hmis_total) {
      sum(total_Miar_Hmir_visits, na.rm = T) / sum(Total_Verified_Visits, na.rm = T)
    }
      else{
       sum(Total_Verified_Visits, na.rm = T) / sum(total_Miar_Hmir_visits, na.rm = T)
      }
    
  ) %>% ungroup() %>% 
  select(-c(hf_total, hmis_total)) %>% 
  pivot_wider(names_from = Type_of_service_name, values_from = score) %>% 
  rowwise() %>% 
  mutate(
    Overall = mean(c_across(is.numeric), na.rm = T)
  ) %>% 
  select(Province, `Service Provider`, `HF Category`, 
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`, Overall
  )

# Split BPHS and EPHS results
consistency_score_MA_bphs <- consistency_score_MA_hf_cat %>% filter(`HF Category` == "BPHS")
consistency_score_MA_ephs <- consistency_score_MA_hf_cat %>% filter(`HF Category` == "EPHS")


# Consistency score Overall
consistency_score_MA_Overall <- sample_ver_data %>% 
  group_by(Province, `Service Provider`, Type_of_service_name) %>% 
  summarise(
    hf_total = sum(Total_Verified_Visits, na.rm = T),
    hmis_total = sum(total_Miar_Hmir_visits, na.rm = T),
    
    score = if(hf_total > hmis_total) {
      sum(total_Miar_Hmir_visits, na.rm = T) / sum(Total_Verified_Visits, na.rm = T)
    }
    else{
      sum(Total_Verified_Visits, na.rm = T) / sum(total_Miar_Hmir_visits, na.rm = T)
    }
    
  ) %>% ungroup() %>% 
  select(-c(hf_total, hmis_total)) %>% 
  pivot_wider(names_from = Type_of_service_name, values_from = score) %>% 
  rowwise() %>% 
  mutate(
    Overall = mean(c_across(is.numeric), na.rm = T)
  ) %>% 
  select(Province, `Service Provider`, 
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`, Overall
  )



# Output list 
consistency_score_list <- list(
  consistency_score_MA_bphs = consistency_score_MA_bphs,
  consistency_score_MA_ephs = consistency_score_MA_ephs,
  consistency_score_MA_Overall = consistency_score_MA_Overall
)

# Export the result
openxlsx::write.xlsx(consistency_score_list, "output/consistency_score.xlsx")

# Consistency score HF Category prev formula
consistency_score_MA_hf_cat_formula2 <- sample_ver_data %>% 
  group_by(Province, `Service Provider`, Type_of_service_name, `HF Category`) %>% 
  summarise(
    hf_total = sum(Total_Verified_Visits, na.rm = T),
    hmis_total = sum(total_Miar_Hmir_visits, na.rm = T),
    
    score = if(hf_total > hmis_total) {
      sum(total_Miar_Hmir_visits, na.rm = T) / sum(Total_Verified_Visits, na.rm = T)
    }
    else{
      sum(Total_Verified_Visits, na.rm = T) / sum(total_Miar_Hmir_visits, na.rm = T)
    }
    
  ) %>% ungroup() %>% 
  select(-c(hf_total, hmis_total)) %>% 
  pivot_wider(names_from = Type_of_service_name, values_from = score) %>% 
  rowwise() %>% 
  mutate(
    Overall = mean(c_across(is.numeric), na.rm = T)
  ) %>% 
  select(Province, `Service Provider`, `HF Category`, 
         `Ante-natal care (ANC)`, `Post-natal care (PNC)`,
         `Institutional Delivery`, `Couple-year protection (CYP)/Family Planning`,
         `Pentavalent vaccine`, `Toxoid Tetanus (TT+) vaccine`,
         `Tuberculosis exams (TB smear+ or GeneXpert)`, `Growth monitoring of children below 2 years old`,
         `Under 5 Morbidities`, `C-section` , `Major surgery`, Overall
  )

# Split BPHS and EPHS results
consistency_score_MA_bphs_formula2 <- consistency_score_MA_hf_cat_formula2 %>% filter(`HF Category` == "BPHS")
consistency_score_MA_ephs_formula2 <- consistency_score_MA_hf_cat_formula2 %>% filter(`HF Category` == "EPHS")




# HMIS INDEX --------------------------------------------------------------

# HMIS Index HF category 
index_hf_cat_list <- list()
for (col in names(consistency_score_MA_hf_cat)) {
  
  if(is.character(consistency_score_MA_hf_cat[[col]])){
    index_hf_cat_list[[col]] <- consistency_score_MA_hf_cat[[col]]
  }
  
  if (is.numeric(consistency_score_MA_hf_cat[[col]])) {
    index_hf_cat_list[[col]] <- consistency_score_MA_hf_cat[[col]] * accuracy_score_MA_hf_cat[[col]]
  }
}

# Convert list to dataframe
hmis_index_ma_hf_cat <- do.call(cbind, index_hf_cat_list) %>% as.data.frame()

# Split BPHS and EPHS results
hmis_index_ma_MA_bphs <- hmis_index_ma_hf_cat %>% filter(`HF Category` == "BPHS")
hmis_index_ma_ephs <- hmis_index_ma_hf_cat %>% filter(`HF Category` == "EPHS")



# HMIS Index overall
index_overall_list <- list()
for (col in names(consistency_score_MA_Overall)) {
  
  if(is.character(consistency_score_MA_Overall[[col]])){
    index_overall_list[[col]] <- consistency_score_MA_Overall[[col]]
  }
  
  if (is.numeric(consistency_score_MA_Overall[[col]])) {
    index_overall_list[[col]] <- consistency_score_MA_Overall[[col]] * accuracy_score_MA_overall[[col]]
  }
}

# Convert list to dataframe
hmis_index_ma_overall <- do.call(cbind, index_overall_list) %>% as.data.frame()


# list of outputs
hmis_index_ma_output_list <- list(
  hmis_index_ma_MA_bphs = hmis_index_ma_MA_bphs,
  hmis_index_ma_ephs = hmis_index_ma_ephs,
  hmis_index_ma_overall = hmis_index_ma_overall
)

openxlsx::write.xlsx(hmis_index_ma_output_list, "output/hmis_index_ma.xlsx")
