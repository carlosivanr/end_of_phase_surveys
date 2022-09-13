#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus. Dept. of Family Medicine
# Prepare EOP data set

# From Wendy Christensen:
# The data sets contain data from each end-of-phase (EOP) survey, which is a 
# required survey for all medical students at the end of every academic year. 
# Some years do not include clerkships and clinical work started changing in 
# 2020 with the "Hybrid" graduating class of 2024. 
# Prior to 2020, the "clerkship year" was Phase 3 (EOP3), which is why the EOP3 
# survey has an additional file with clerkship grades.

# The CO in each data file indicates "Class Of". 
# Students who completed the EOP1 survey started med school in AY 20-21 and, 
# if they stay on-cycle, will graduate in 2024. Students can and do go 
# # off-cycle, but most students end up graduating on-time.

# CO = Class Of
# AY = Academic Year
# EOP = End of Phase
# SDAC = student data advisory committee

# EOP1 is at the end of Year1, expected graduation in 2024
# EOP2 is at the end of Year2, expected graduation in 2023
# EOP3 is at the end of Year3, expected graduation in 2022
# EOP4 is at the end of Year4, expected graduation in 2021

# Survey Questions ProfID 1:9 tap into professional identification via a likert
# style responses with a range from 1:5 Strongly disagree, disagree, neutral, 
# agree, strongly agree
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacman::p_load(tidyverse,
               openxlsx,
               Hmisc,
               magrittr,
               here)

# Load the data and the corresponding code book --------------------------------
# List all the CO20 files in the data directory
file_paths <- str_c("./data/", dir("./data", pattern = "CO20"))

# Load the files into the list data
eop_files <- map(file_paths, read.xlsx)
file_names <- sub("./data/", "", file_paths)
file_names <- sub("_for.*", "", file_names)
names(eop_files) <- file_names

# Load codebook and rename the code books according to the the sheet name
code_path <-  str_c("./data/", dir("./data", pattern = "Codebook"))
code_sheets <- getSheetNames(code_path)
eop_code_books <- map(code_sheets, read.xlsx, xlsxFile = code_path)
names(eop_code_books) <- code_sheets

# Source the process_eop function ---------------------------------------------
source("./functions/process_eop.R")

# Apply the function ----------------------------------------------------------
# eop4 maps from eop_files[[CO2021_EOP4]] and eop_code_books[["CO 2021 EOP 4"]]
# eop3 maps from eop_files[[CO2022_EOP3]] and eop_code_books[["CO 2022 EOP 3"]]
# eop2 maps from eop_files[[CO2023_EOP2]] and eop_code_books[["CO 2023 EOP 2"]]
# eop1 maps from eop_files[[CO2024_EOP1]] and eop_code_books[["CO 2024 EOP 1"]]

# EOP 4
eop4 <- process_eop(eop_files[["CO2021_EOP4"]], eop_code_books[["CO 2021 EOP 4"]])

# EOP 3
eop3 <- process_eop(eop_files[["CO2022_EOP3"]], eop_code_books[["CO 2022 EOP 3"]])

# EOP 2
eop2 <- process_eop(eop_files[["CO2023_EOP2"]], eop_code_books[["CO 2023 EOP 2"]])

# EOP 1
eop1 <- process_eop(eop_files[["CO2024_EOP1"]], eop_code_books[["CO 2024 EOP 1"]])

# EOP 3 Grades
eop3_grades <- process_eop(eop_files[["CO2022_EOP3_grades_schmerling.xlsx"]], 
                           eop_code_books[["CO 2022 EOP + Clerkship Grades"]])

# Ensure all grades are previous to 2021 Fall semester, takes care of the student
# with both LIC and Traditional clerkship
eop3_grades %<>%
  filter(TERM_SD != "2021 Fall")

# Only one student took a W, filtering that out to obtain a consistent grade for
# each student. Equates to taking the most frequent grade which was decided on
# by the research group. 
eop3_grades %<>%
  filter(CRSE_GRADE_OFF != "W")

# Get the most recent clerkship grade. When W filtered out, equatest to taking
# the most frequent, as every other student has the same grade for each semester.
eop3_grades %<>%
  group_by(ResearchID) %>%
  arrange(TERM_SD) %>%
  slice_tail() %>%
  ungroup()
  
# Process all ProfId columns, by converting to numeric and reverse scoring 
# questions 3, 4, and 5.
source(here("functions", "process_ProfId.R"))

# Set vector of data frames that have columns to convert dates
dfs <- c("eop1", "eop2", "eop3", 
         "eop4", "eop3_grades")

# Apply convert_dates() to all dfs
walk(dfs, ~ proc_ProfId(.x))

# Process eop3 grades columns
eop3_grades %<>% 
  mutate(
    across(CRSE_GRADE_OFF, 
           ~ as.numeric(factor(.x, levels = c("W", "P", "HP", "H")))
           )
    )      







              
## Test tables --------------------------------------------------
# # Test race_ethnicity variables
# eop4 %>% 
#   select(Race_1:Race_8) %>%
#   tbl_summary()
# 
# # Test the survey questions
# eop4 %>%
#   select(ProfID_1) %>%
#   tbl_summary()
# 
# # Test creating a survey_design object
# eop_design <- survey::svydesign(data = eop4, id = ~1)
# 
# # Frequency tables by one variable
# svytable(~ProfID_1, design = eop_design)
# 
# # Frequency tables by two variables
# as.data.frame(svytable(~ProfID_1 + Rural, design = eop_design))
# 
# # Survey table from the gtsummary package for one question
# tbl_svysummary(
#   eop_design,
#   by = Rural,
#   include = ProfID_1
# )
# 
# # Survey table from the gtsummary package for two questions
# tbl_svysummary(
#   eop_design,
#   by = Rural,
#   include = c(ProfID_1, ProfID_2)
# )
# 
# # tbl_summary can do the exact same thing. The default will conduct a Fisher's
# # exact test, the proportion of var1, var2, var3, etc across the by variable
# eop4 %>%
#   select(Rural, ProfID_1, ProfID_2) %>%
#   tbl_summary(
#     by = Rural
#   ) %>%
#   add_p()
#   

# Test the difference between rural and non-rural on a question?
# Could use a chi_square to test the differnce in proportion
# Could run an ANOVA on the numerical values

