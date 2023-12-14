# Load required packages -----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Read data ------------------------------------------------------------------------------
data_path <- "./input/HER_Re_QQC_cleaned_approved.xlsx"
data <- read_excel(data_path, sheet = "data", guess_max = 5000000) %>% 
  filter(HF_Type_based_on_sample != "SHC") %>% 
  filter(Interviewee_Respondent_Type %in% "Observation Section")

# Pre-Process -------------------------------------------------------------
data_processed <- data %>% 
  mutate(
    # 1. Gen_Management
    q1_1_gen_mgmnt = case_when(
      q1_1_1 == 1 & q1_1_2 == 1 & q1_1_3 == 1 & q1_1_4 == 1 & q1_1_5 ~ 0.5,
      TRUE ~ 0
    ),
    q1_2_1_gen_mgmnt = case_when(
      q1_2_1 %in% "YES (critieria met]" ~ 0.5,
      TRUE ~ 0
    ),
    q1_2_2_gen_mgmnt = case_when(
      q1_2_2 %in% "YES (critieria met]" ~ 0.5,
      TRUE ~ 0
    ),
    q1_2_3_gen_mgmnt = case_when(
      q1_2_3 %in% "YES (critieria met]" ~ 0.5,
      TRUE ~ 0
    ),
    q1_2_4_gen_mgmnt = case_when(
      q1_2_4 %in% "YES (critieria met]" ~ 0.5,
      TRUE ~ 0
    ),
    q1_2_5_gen_mgmnt = case_when(
      q1_2_5 %in% "YES (critieria met]" ~ 0.5,
      TRUE ~ 0
    ),
    q1_3_gen_mgmnt = case_when(
      q1_3 %in% "YES (critieria met]" ~ 1,
      TRUE ~ 0
    ),
    q1_4_gen_mgmnt = case_when(
      q1_4 %in% "Four Complete Meeting Minutes" ~ 4,
      q1_4 %in% "Three Complete Meeting Minutes" ~ 3,
      q1_4 %in% "Two Complete Meeting Minutes" ~ 2,
      q1_4 %in% "One Complete Meeting Minutes" ~ 1,
      TRUE ~ 0
    ),
    q1_5_gen_mgmnt = case_when(
      q1_5 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    q1_6_gen_mgmnt = case_when(
      q1_6 %in% "YES (all criteria met)" ~ 1,
      TRUE ~ 0
    ),
    q1_7_gen_mgmnt = case_when(
      q1_7 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    q1_8_gen_mgmnt = case_when(
      q1_8 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    q1_9_gen_mgmnt = case_when(
      q1_9 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    # 2. Hygiene
    q2_1_hygiene  = case_when(
      q2_1 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q2_2_hygiene  = case_when(
      q2_2 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q2_3_1_hygiene  = case_when(
      q2_3_1_1 == 1 & q2_3_1_2 == 1 & q2_3_1_3 == 1 & q2_3_1_4 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q2_3_2_hygiene  = case_when(
      q2_3_2 %in% "Yes" ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_3_3_hygiene  = case_when(
      q2_3_3 %in% "Yes" ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_3_4_hygiene  = case_when(
      q2_3_4_1 == 1 & q2_3_4_2 == 1 & q2_3_4_3 == 1 & q2_3_4_4 == 1 ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_3_5_hygiene  = case_when(
      q2_3_5_1 == 1 & q2_3_5_2 == 1 & q2_3_5_3 == 1 ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_4_1_hygiene  = case_when(
      q2_4_1_1 == 1 & q2_4_1_2 == 1 & q2_4_1_3 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q2_4_2_hygiene  = case_when(
      q2_4_2 %in% "Yes" ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_4_3_hygiene  = case_when(
      q2_4_3 %in% "Yes" ~ 0.5,
      TRUE ~ 0
    ),
    
    q2_5_hygiene  = case_when(
      q2_5_1 == 1 & q2_5_2 == 1 & q2_5_3 == 1 & q2_5_4 == 1 ~ 12,
      TRUE ~ 0
    ),
    
    q2_6_hygiene  = case_when(
      q2_6 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q2_7_hygiene  = case_when(
      q2_7_1 == 1 & q2_7_2 == 1 & q2_7_3 == 1 & q2_7_4 == 1 & q2_7_5 == 1 ~ 3,
      TRUE ~ 0
    ),
    
    q2_8_hygiene  = case_when(
      q2_8_1 == 1 & q2_8_2 == 1 & q2_8_3 == 1 & q2_8_4 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q2_9_hygiene  = case_when(
      q2_9_1 == 1 & q2_9_2 == 1 & q2_9_3 == 1 & q2_9_4 == 1 ~ 6,
      TRUE ~ 0
    ),
    
    q2_10_hygiene  = case_when(
      q2_10_1 == 1 & q2_10_2 == 1 & q2_10_3 == 1 ~ 3,
      TRUE ~ 0
    ),
    
    
    # 3. OPD
    q3_1_opd  = case_when(
      q3_1_1 == 1 & q3_1_2 == 1 & q3_1_3 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q3_2_opd  = case_when(
      q3_2 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_3_opd  = case_when(
      q3_3 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_4_1_opd  = case_when(
      q3_4_1_1 == 1 & q3_4_1_2 == 1 & q3_4_1_3 == 1 & q3_4_1_4 == 1 & q3_4_1_5 == 1 ~ 5,
      TRUE ~ 0
    ),
    
    q3_4_2_opd  = case_when(
      q3_4_2 %in% "YES, there are two consultation rooms, and the room for men has all five criteria met" ~ 1,
      TRUE ~ 0
    ),
    
    q3_5_opd  = case_when(
      q3_5 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_6_opd  = case_when(
      q3_6 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q3_7_1_opd  = case_when(
      q3_7_1 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_7_2_opd  = case_when(
      q3_7_2 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_8_opd  = case_when(
      q3_8 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_9_opd  = case_when(
      q3_9 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q3_10_opd  = case_when(
      q3_10_1 == 1 & q3_10_2 == 1 & q3_10_3 == 1 & q3_10_4 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q3_11_opd  = case_when(
      q3_11_1 == 1 & q3_11_2 == 1 & q3_11_3 == 1 & q3_11_4 == 1 & q3_11_5 == 1 & q3_11_6 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q3_12_opd  = case_when(
      q3_12 %in% "YES, ALL last five ARI cases have been treated according to the ARI protocol" ~ 1,
      TRUE ~ 0
    ),
    
    q3_13_2_opd  = case_when(
      q3_13_2 == "YES, both criteria have been met" ~ 2,
      TRUE ~ 0
    ),
    
    q3_14_opd  = case_when(
      q3_14 == "YES, ALL last five Diarrhea cases have been treated according to the Diarrhea protocol" ~ 1, # Added on 12.12.23
      TRUE ~ 0
    ),
    
    q3_15_opd  = case_when(
      q3_15 %in% "Yes" ~ 4,
      TRUE ~ 0
    ),
    
    q3_16_opd  = case_when(
      q3_16 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q3_17_opd  = case_when(
      q3_17_1 == 1 & q3_17_2 == 1 & q3_17_3 == 1 & q3_17_4 == 1  ~ 2,
      TRUE ~ 0
    ),
    
    q3_18_opd  = case_when(
      q3_18 %in% "Yes, available and functional" ~ 0.5,
      TRUE ~ 0
    ),
    
    q3_19_opd  = case_when(
      q3_19 %in% "Yes, available and functional" ~ 0.5,
      TRUE ~ 0
    ),
    
    q3_20_opd  = case_when(
      q3_20 %in% "Yes, available and functional" ~ 0.5,
      TRUE ~ 0
    ),
    
    q3_21_opd  = case_when(
      q3_21 %in% "Yes, available and functional" ~ 1,
      TRUE ~ 0
    ),
    
    q3_22_opd  = case_when(
      q3_22 %in% "Yes, available and functional" ~ 0.5,
      TRUE ~ 0
    ),
    
    q3_23_opd  = case_when(
      q3_23 %in% "YES, both criteria have been met" ~ 1,
      TRUE ~ 0
    ),
    
    q3_24_1_opd  = case_when(
      q3_24_1 %in% "YES, all five cases sampled have had their nutritional status recorded" ~ 4,
      TRUE ~ 0
    ),
    
    q3_24_2_opd  = case_when(
      q3_24_2 %in% "YES, the mothers of all five cases sampled have had their nutritional status recorded" ~ 2,
      TRUE ~ 0
    ),
    
    q3_24_3_opd  = case_when(
      q3_24_3 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    # 4. FP
    
    q4_1_fp  = case_when(
      q4_1 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q4_2_fp  = case_when(
      q4_2 %in% "Yes" ~ 4,
      TRUE ~ 0
    ),
    
    q4_3_fp  = case_when(
      q4_3_1 == 1 & q4_3_2 == 1 & q4_3_3 == 1   ~ 7,
      TRUE ~ 0
    ),
    
    q4_4_fp  = case_when(
      q4_4_1 == 1 & q4_4_2 == 1    ~ 3,
      TRUE ~ 0
    ),
    
    q4_5_fp  = case_when(
      q4_5_1 == 1 & q4_5_2 == 1    ~ 3,
      TRUE ~ 0
    ),
    
    q4_6_fp  = case_when(
      q4_6 %in% "Yes" ~ 3,
      TRUE ~ 0
    ),
    
    q4_7_fp  = case_when(
      q4_7 %in% "YES, all five cards have all five elements" ~ 2,
      TRUE ~ 0
    ),
    
    # 5. Laboratory
    
    q5_1_lab  = case_when(
      q5_1 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q5_2_lab  = case_when(
      q5_2 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q5_3_lab  = case_when(
      q5_3 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q5_4_lab  = case_when(
      q5_4 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q5_5_lab  = case_when(
      q5_5 %in% "YES all three criteria met" ~ 1,
      TRUE ~ 0
    ),
    
    q5_6_lab  = case_when(
      q5_6 %in% "Yes, all six elements are available and functional/not expired" ~ 2,
      TRUE ~ 0
    ),
    
    q5_8_lab  = case_when(
      q5_8 %in% "Yes, available and functional" ~ 1,  # FIXED on 12.12.2023: corrected the choice name
      TRUE ~ 0
    ),
    
    q5_9_lab  = case_when( # FIXED on 12.12.2023: added the suffix 'lab'
      q5_9_1 == 1 & q5_9_2 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q5_10_lab  = case_when(
      q5_10 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q5_11_lab  = case_when( # FIXED on 12.12.2023: added the suffix 'lab'
      q5_11_1 == 1 & q5_11_2 == 1 & q5_11_3 == 1 & q5_11_4 == 1 & q5_11_5 == 1 & q5_11_6 == 1 & q5_11_7 == 1 & q5_11_8 == 1 ~ 3,
      TRUE ~ 0
    ),
    
    # 6. EDM
    q6_1_edm = case_when(
      q6_1 == "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q6_1_1_edm = case_when(
      q6_1_1_1 == 1 & q6_1_1_2 == 1 ~ 6,
      TRUE ~ 0
    ),
    
    q6_2_edm = case_when(
      q6_2_1 == 1 & q6_2_2 == 1 ~ 6,
      TRUE ~ 0
    ),
    
    q6_3_1_edm = case_when(
      q6_3_1 == "No" ~ 4,
      TRUE ~ 0
    ),
    
    # 6_3_2 is missing in the tool and data set
    
    q6_3_3_edm = case_when(
      q6_3_3_1 == 1 & q6_3_3_2 == 1 & q6_3_3_3 == 1 & q6_3_3_4 == 1 ~ 4,
      TRUE ~ 0
    ),
    
    q6_3_4_edm = case_when(
      q6_3_4 == "YES, correctly mentioned" ~ 4,
      TRUE ~ 0
    ),
    
    q6_3_5_edm = case_when(
      q6_3_5_1 == 1 & q6_3_5_2 == 1 & q6_3_5_3 == 1 & q6_3_5_4 == 1 & q6_3_5_5 == 1 ~ 4,
      TRUE ~ 0
    ),
    
    q6_3_6_edm = case_when(
      (q6_3_6_1 + q6_3_6_2 + q6_3_6_3 + q6_3_6_4 + q6_3_6_5) >= 4 ~ 4,
      TRUE ~ 0
    ),
    
    q6_4_edm = case_when(
      q6_4_1 == 1 & q6_4_2 == 1 ~ 4,
      TRUE ~ 0
    ),
    
    q6_5_edm = case_when(
      q6_5_1 == 1 & q6_5_2 == 1 & q6_5_3 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q6_6_1_edm = case_when(
      q6_6_1 == "Yes" ~ 4,
      TRUE ~ 0
    ),
    
    q6_6_2_edm = case_when(
      q6_6_2 == "Yes" ~ 4,
      TRUE ~ 0
    ),
    
    # 7 TracerRx
    q7_1_tracerrx = case_when(
      q7_1 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_2_tracerrx = case_when(
      q7_2 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_3_tracerrx = case_when(
      q7_3 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_4_tracerrx = case_when(
      q7_4 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_5_tracerrx = case_when(
      q7_5 == "Yes, available with the amount of two months’ worth of AMC for 50k, 100k, and 200k IU Vit A" ~ 2, # FIXED on 12.12.23: replaced the correct choice
      TRUE ~ 0
    ),
    
    q7_6_tracerrx = case_when(
      q7_6 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_7_tracerrx = case_when(
      q7_7 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_8_tracerrx = case_when(
      q7_8 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_9_tracerrx = case_when(
      q7_9 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_10_tracerrx = case_when(
      q7_10 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_11_tracerrx = case_when(
      q7_11 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_12_tracerrx = case_when(
      q7_12 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_13_tracerrx = case_when(
      q7_13 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_14_tracerrx = case_when(
      q7_14 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_15_tracerrx = case_when(
      q7_15 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_16_tracerrx = case_when(
      q7_16 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_17_tracerrx = case_when(
      q7_17 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_18_tracerrx = case_when(
      q7_18 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_19_tracerrx = case_when(
      q7_19 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_20_tracerrx = case_when(
      q7_20 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_21_tracerrx = case_when(
      q7_21 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_22_tracerrx = case_when(
      q7_22 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_23_tracerrx = case_when(
      q7_23 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_24_tracerrx = case_when(
      q7_24 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_25_tracerrx = case_when(
      q7_25 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_26_tracerrx = case_when(
      q7_26 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_27_tracerrx = case_when(
      q7_27 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_28_tracerrx = case_when(
      q7_28 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_29_tracerrx = case_when(
      q7_29 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_30_tracerrx = case_when(
      q7_30 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_31_tracerrx = case_when(
      q7_31 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_32_tracerrx = case_when(
      q7_32 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_33_tracerrx = case_when(
      q7_33 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_34_tracerrx = case_when(
      q7_34 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_35_tracerrx = case_when(
      q7_35 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_36_tracerrx = case_when(
      q7_36 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_37_tracerrx = case_when(
      q7_37 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_38_tracerrx = case_when(
      q7_38 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_39_tracerrx = case_when(
      q7_39 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_40_tracerrx = case_when(
      q7_40 == "Yes, available with the amount of two months’ worth of AMC" ~ 2, # FIXED on 12.12.23 based on long format of scoring methodology
      TRUE ~ 0
    ),
    
    q7_41_tracerrx = case_when(
      q7_41 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_42_tracerrx = case_when(
      q7_42 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_43_tracerrx = case_when(
      q7_43 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_44_tracerrx = case_when(
      q7_44 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_45_tracerrx = case_when(
      q7_45 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    q7_46_tracerrx = case_when(
      q7_46 == "Yes, available with the amount of two months’ worth of AMC" ~ 2,
      TRUE ~ 0
    ),
    
    # 8. Maternity
    q8_1_maternity = case_when(
      q8_1_1 == 1 & q8_1_2 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q8_2_maternity = case_when(
      q8_2_1 == 1 & q8_2_2 == 1 ~ 1,
      TRUE ~ 0
    ),  
    
    q8_3_maternity = case_when(
      q8_3_1 == 1 & q8_3_2 == 1 & q8_3_3 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q8_4_1_maternity = case_when(
      q8_4_1 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_4_2_maternity = case_when(
      q8_4_2 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_4_3_maternity = case_when(
      q8_4_3_1 == 1 & q8_4_3_2 == 1 & q8_4_3_3 == 1 & q8_4_3_4 == 1 & q8_4_3_5 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q8_4_4_maternity = case_when(
      q8_4_4 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_4_5_maternity = case_when(
      q8_4_5 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_4_6_maternity = case_when(
      q8_4_6 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_5_maternity = case_when(
      q8_5 == "YES, both criteria have been met" ~ 2,
      TRUE ~ 0
    ),
    
    q8_6_1_maternity = case_when(
      q8_6_1 == "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q8_6_2_maternity = case_when(
      q8_6_2 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_1_maternity = case_when(
      q8_7_1 == "Yes, available and functional" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_2_maternity = case_when(
      q8_7_2 == "Yes, available and functional" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_3_maternity = case_when(
      q8_7_3 == "Yes, available and functional" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_4_maternity = case_when(
      q8_7_4 == "Yes, available and not expired" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_5_maternity = case_when(
      q8_7_5 == "Yes, available and functional" ~ 2,
      TRUE ~ 0
    ),
    
    q8_7_6_maternity = case_when(
      q8_7_6 == "Yes, available and functional" ~ 1,
      TRUE ~ 0
    ),
    
    q8_7_7_maternity = case_when(
      q8_7_7 == "Yes, available and not expired" ~ 1, # FIXED on 12.12.23: added the correct choice name
      TRUE ~ 0
    ),
    
    q8_8_maternity = case_when(
      q8_8_1 == 1 & q8_8_2 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q8_9_maternity = case_when(
      q8_9_1 == 1 & q8_9_2 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q8_10_maternity = case_when(
      q8_10_1 == 1 & q8_10_2 == 1 & q8_10_3 == 1 & q8_10_4 == 1 & q8_10_5 == 1 & q8_10_6 == 1 & q8_10_7 == 1 & q8_10_8 == 1 & q8_10_9 == 1 ~ 2, 
      TRUE ~ 0
    ),
    
    q8_11_maternity = case_when(
      q8_11_1 == 1 & q8_11_2 == 1 & q8_11_3 == 1 ~ 1, 
      TRUE ~ 0
    ),
    
    
    # 9. EPI
    
    q9_1_epi  = case_when(
      q9_1_1 == 1 & q9_1_2 == 1 & q9_1_3 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q9_2_epi  = case_when(
      q9_2_1 == 1 & q9_2_2 == 1 & q9_2_3 == 1 & q9_2_4 == 1 & q9_2_5 == 1 & q9_2_6 == 1 ~ 8,
      TRUE ~ 0
    ),
    
    # The question numbers are out of order starting from Q9_3
    q9_3_epi  = case_when(
      q9_3 %in% "Yes" & q9_4_1 == 1 & q9_4_2 == 1 & q9_4_3 == 1 & q9_4_4 == 1 & q9_4_5 == 1  ~ 5,
      TRUE ~ 0
    ),

    q9_4_epi = case_when(
      q9_5_1 == 1 & q9_5_2 == 1 & q9_5_3 == 1 & q9_5_4 == 1 ~ 1,
      TRUE  ~ 0
    ),
    
    q9_5_epi  = case_when(
      q9_6 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
     
    q9_6_epi = case_when(
      q9_7_1 == 1 & q9_7_2 == 1 & q9_7_3 == 1 ~ 1,
      TRUE  ~ 0
    ),
       
    q9_7_epi  = case_when(
      q9_8 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q9_8_epi  = case_when(
      q9_9 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q9_9_epi  = case_when(
      q9_10 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q9_10_epi = case_when(
      q9_11_1 == 1 & q9_11_2 == 1 & q9_11_3 == 1 & q9_11_4 == 1 ~ 1,
      TRUE  ~ 0
    ),
    
    q9_11_epi  = case_when(
      q9_12 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q9_12_epi  = case_when(
      q9_13 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q9_13_epi  = case_when(
      q9_14_1 == 1 & q9_14_2 == 1  ~ 1,
      TRUE ~ 0
    ),
    
    q9_14_epi  = case_when(
      q9_15 %in% "YES, both criteria have been met" ~ 1,
      TRUE ~ 0
    ),
    # 9.15 - Existence of a system to recover drop-outs - Missing
    
    # 10. ANC
    q10_1_anc = case_when(
      q10_1_1 == 1 & q10_1_2 == 1 ~ 0.5,
      TRUE ~ 0
    ),
    
    q10_2_anc = case_when(
      q10_2_1 == 1 & q10_2_2 == 1 & q10_2_3 == 1 & q10_2_4 == 1 & q10_2_5 == 1 & q10_2_6 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    q10_3_anc = case_when(
      q10_3_1 == 1 & q10_3_2 == 1 & q10_3_3 == 1 & q10_3_4 == 1 & q10_3_5 == 1 ~ 3,
      TRUE ~ 0
    ),
    
    q10_4_anc = case_when(
      q10_4_1 == 1 & q10_4_2 == 1 & q10_4_3 == 1 & q10_4_4 == 1 & q10_4_5 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q10_5_anc = case_when(
      q10_5 %in% "Yes" ~ 0.5,
      TRUE ~ 0
    ),
    
    q10_6_anc = case_when(
      q10_6 %in% "YES, both criteria have been met" ~ 3,
      TRUE ~ 0
    ),
    
    q10_7_anc = case_when(
      q10_7 %in% "Yes" ~ 2,
      TRUE ~ 0
    ),
    
    q10_8_anc = case_when(
      q10_8 %in% "YES, both criteria have been met" ~ 1,
      TRUE ~ 0
    )

  )


# Aggregate the points ----------------------------------------------------

data_processed_indicators <- data_processed %>% 
  rowwise() %>% 
  mutate(
    General_Management = sum(c_across(ends_with("_gen_mgmnt")), na.rm = T),
    Hygiene = sum(c_across(ends_with("_hygiene")), na.rm = T),
    OPD = sum(c_across(ends_with("_opd")), na.rm = T),
    Family_Planning = sum(c_across(ends_with("_fp")), na.rm = T),
    Laboratory = sum(c_across(ends_with("_lab")), na.rm = T),
    Essential_Drugs_Management = sum(c_across(ends_with("_edm")), na.rm = T),
    Tracer_Drugs = sum(c_across(ends_with("_tracerrx")), na.rm = T),
    Maternity = sum(c_across(ends_with("_maternity")), na.rm = T),
    EPI = sum(c_across(ends_with("_epi")), na.rm = T),
    ANC = sum(c_across(ends_with("_anc")), na.rm = T),
  ) %>% ungroup() %>% 
  mutate(
    General_Management_score = General_Management / 14,
    Hygiene_score = Hygiene / 34,
    OPD_score = OPD / 45, #Fixed on 12.12.23 based on the scoring methodology
    Family_Planning_score = Family_Planning / 23,
    Laboratory_score = Laboratory / 15,
    Essential_Drugs_Management_score = Essential_Drugs_Management / 48, # Since 6.3.2 is missing max score has been reduced to 48 from 52 (6.3.1 gets 4 points)
    Tracer_Drugs_score = Tracer_Drugs / 90,
    Maternity_score = Maternity / 29,
    EPI_score = EPI / 25, # 9.15 "Existence of a system to recover drop-outs" is missing, The max score is adjusted, reduced to 25 from 27 (9.15 gets 2 points)
    ANC_score = ANC / 13,
  )


# Analysis ----------------------------------------------------------------

# Changing 0 to NA in cells that were supposed to be empty for correct calculation of mean
data_for_analysis <- data_processed_indicators %>% 
  mutate(
    General_Management_score = case_when(
      Interview_Type_SV %in% "General Management" ~ General_Management_score,
      TRUE ~ NA_real_
    ),
    Hygiene_score = case_when(
      Interview_Type_SV %in% "Hygiene" ~ Hygiene_score,
      TRUE ~ NA_real_
    ),
    OPD_score = case_when(
      Interview_Type_SV %in% "OPD" ~ OPD_score,
      TRUE ~ NA_real_
    ),
    Family_Planning_score = case_when(
      Interview_Type_SV %in% "Family Planning" ~ Family_Planning_score,
      TRUE ~ NA_real_
    ),
    Laboratory_score = case_when(
      Interview_Type_SV %in% "Laboratory" ~ Laboratory_score,
      TRUE ~ NA_real_
    ),
    Essential_Drugs_Management_score = case_when(
      Interview_Type_SV %in% "Essential Drugs Management" ~ Essential_Drugs_Management_score,
      TRUE ~ NA_real_
    ),
    Tracer_Drugs_score = case_when(
      Interview_Type_SV %in% "Tracer Drugs" ~ Tracer_Drugs_score,
      TRUE ~ NA_real_
    ),
    Maternity_score = case_when(
      Interview_Type_SV %in% "Maternity" ~ Maternity_score,
      TRUE ~ NA_real_
    ),
    EPI_score = case_when(
      Interview_Type_SV %in% "EPI" ~ EPI_score,
      TRUE ~ NA_real_
    ),
    ANC_score = case_when(
      Interview_Type_SV %in% "ANC" ~ ANC_score,
      TRUE ~ NA_real_
    ),
  )


# Aggregation

overall <- data_for_analysis %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )
  

by_region <- data_for_analysis %>% 
  group_by(Region) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )


by_hf_type <- data_for_analysis %>% 
  group_by(HF_Type_based_on_sample) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )

by_province <- data_for_analysis %>% 
  group_by(Province) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )

by_sp <- data_for_analysis %>% 
  group_by(SP_Name_based_on_sample) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )



by_Site_Visit_ID <- data_for_analysis %>% 
  group_by(Site_Visit_ID) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )

by_Site_Visit_ID <- data_for_analysis %>% 
  group_by(Site_Visit_ID) %>% 
  summarise(
    General_Management_score = mean(General_Management_score, na.rm = T),
    Hygiene_score = mean(Hygiene_score, na.rm = T),
    OPD_score = mean(OPD_score, na.rm = T),
    Family_Planning_score = mean(Family_Planning_score, na.rm = T),
    Laboratory_score = mean(Laboratory_score, na.rm = T),
    Essential_Drugs_Management_score = mean(Essential_Drugs_Management_score, na.rm = T),
    Tracer_Drugs_score = mean(Tracer_Drugs_score, na.rm = T),
    Maternity_score = mean(Maternity_score, na.rm = T),
    EPI_score = mean(EPI_score, na.rm = T),
    ANC_score = mean(ANC_score, na.rm = T),
  )


openxlsx::write.xlsx(by_Site_Visit_ID, "QQC_TEST_By_Site_Visit_ID.xlsx")


output_list <- list(
  overall = overall,
  by_region = by_region,
  by_hf_type = by_hf_type,
  by_province = by_province,
  by_sp = by_sp
)

openxlsx::write.xlsx(output_list, "output/QQC_Indicators_analysis_28112023.xlsx")



# data subset
sub_set <- data_processed_indicators %>% 
  select(
    Province:HF_Type_based_on_sample, SP_Name_based_on_sample, Interview_Type_SV,
    General_Management:ANC_score,
    ends_with(c("_gen_mgmnt", "_hygiene", "_opd", "_fp", "_lab", "_edm", "_tracerrx", "_maternity", "_epi", "_anc")),
    
  )

openxlsx::write.xlsx(sub_set, "output/QQC_indicators_calculations_TEST.xlsx")

