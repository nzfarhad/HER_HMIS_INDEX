# Load required packages -----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Read data ------------------------------------------------------------------------------
data_path <- "./input/HER_Re_QQC_cleaned_approved.xlsx"


# Pre-Process -------------------------------------------------------------
data <- read_excel(data_path, sheet = "data", guess_max = 5000000)

data_processed <- data %>% 
  mutate(
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
  )


table(data_processed$q1_9)
table(is.na(data_processed$q1_5))

