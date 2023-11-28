# Load required packages -----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Read data ------------------------------------------------------------------------------
data_path <- "./input/HER_Re_QQC_cleaned_approved.xlsx"


# Pre-Process -------------------------------------------------------------
data <- read_excel(data_path, sheet = "data", guess_max = 5000000)

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
      q1_7 %in% "YES (all criteria met)" ~ 1,
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
    
    q3_13_1_opd  = case_when(
      q3_13_1_1 == 1 & q3_13_1_2 == 1 & q3_13_1_3 == 1 & q3_13_1_4 == 1 & q3_13_1_5 == 1 & q3_13_1_6 == 1 ~ 2,
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
    
    q4_3_opd  = case_when(
      q4_3_1 == 1 & q4_3_2 == 1 & q4_3_3 == 1   ~ 7,
      TRUE ~ 0
    ),
    
    q4_4_opd  = case_when(
      q4_4_1 == 1 & q4_4_2 == 1    ~ 3,
      TRUE ~ 0
    ),
    
    q4_5_opd  = case_when(
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
      q5_8 %in% "YES all three criteria met" ~ 1,
      TRUE ~ 0
    ),
    
    q5_9_opd  = case_when(
      q5_9_1 == 1 & q5_9_2 == 1 ~ 2,
      TRUE ~ 0
    ),
    
    q5_10_lab  = case_when(
      q5_10 %in% "Yes" ~ 1,
      TRUE ~ 0
    ),
    
    q5_11_opd  = case_when(
      q5_11_1 == 1 & q5_11_2 == 1 & q5_11_3 == 1 & q5_11_4 == 1 & q5_11_5 == 1 & q5_11_6 == 1 & q5_11_7 == 1 & q5_11_8 == 1 ~ 3,
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
    
  )


table(data_processed$q9_14_epi)
table(is.na(data_processed$q9_2))

