#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus. Dept. of Family Medicine
# Process EOP survey data

# This function will processes the following: 
# 1. All NULL values converted to NA
# 2. Renames gender, rural1nonrural0 columns to Gender and Rural
# 3. Renames age_matriculation to Age, but consider creating a new variable that
#   reflects age at survey rather than age at matriculation
# 4. ProfID questions converted to ordered factor and labeled
# 5. Multiple values in Race column are separated and converted to wide, renamed
#   and then rejoined to the main data frame.

# Requirements:
# This function requires that all eop survey data and codebooks are loaded into
# the workspace as data frame 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Process an EOP survey is a function of the EOP data frame as input
# and the corresponding codebook dataframe.
# e.g. for df the df is data[[1]] and the codebook is "CO 2021 EOP 4"
#eop4 <- process_eop(data[[1]], code_book[["CO 2021 EOP 4"]])

process_eop <-  function(eop, eop_codes){
  df <- eop
  df_codes <- eop_codes
  
  # First check to ensure the inputs are data frames
  check_df <- is.data.frame(df)
  if (check_df == FALSE){
    stop("First input must be a data frame of an EOP survey.")
  }
  
  check_code <- is.data.frame(df_codes)
  if (check_code == FALSE){
    stop("Second input must be a data frame of an EOP survey codebook.")
  }
  
  # Assign the number of rows to check with in subsequent steps
  check_nrow <- nrow(df)

  ## 1. Convert all NULL values to NA ------------------------------------------
  df[df == "NULL"] = NA
  
  ## 2. Rename variables -------------------------------------------------------
  df <- df %>% 
    rename(Gender = gender,
           Matriculation_Age = age_matriculation,
           Rural = rural1nonrural0comb)
  
  ## 3. Prepare the race & ethnicity columns  ----------------------------------
  # Ethnicity
  # Combine/recode Hispanic,Latino, or of Spanish origin to Spanish/Hispanic/Latino/Latina
  # If Not Hispanic or Latino, fill in with whatever is in the race column
  df %<>% 
    mutate(Ethnicity = dplyr::recode(Ethnicity, "Hispanic, Latino, or of Spanish origin" = "Spanish/Hispanic/Latino/Latina")) %>%
    mutate(Ethnicity = ifelse(Ethnicity == "Not Hispanic or Latino", Race, Ethnicity))
  
  # Rename Ethnicity to Race/Ethncity since it has both types of information
  df %<>% 
    rename("Race/Ethnicity" = Ethnicity)
  
  # # Separate rows according to the semicolon
  # df.race <- df %>% 
  #   separate_rows(Race, sep = ";")
  # 
  # # Trim the white space in the values after separating rows
  # df.race$Race <- str_trim(df.race$Race)
  # 
  # # Convert any empty values to NA
  # df.race %<>% mutate_all(na_if,"")
  # 
  # # Create a wide format data frame of race from the separated rows
  # race_wide <- 
  #   df.race %>%
  #   group_by(ResearchID) %>%
  #   count(Race) %>%
  #   pivot_wider(id_cols = ResearchID, 
  #               names_from = Race, 
  #               values_from = n) %>%
  #   ungroup()
  # 
  # # Check the number of rows
  # if (check_nrow != nrow(race_wide)){
  #   warning("The number of rows in race_wide does not match EOP input!")
  # }
  # 
  # # Drop the NA and Unknown columns from race_wide if they are found in the 
  # # race_wide dataframe
  # if (sum(grepl("NA", names(race_wide))) == 1){
  #  race_wide %<>% 
  #   select(-`NA`) 
  # }
  # 
  # 
  # if (sum(grepl("Unknown", names(race_wide))) == 1){
  #  race_wide %<>% 
  #   select(-Unknown) 
  # }
  # 
  # 
  # # Convert all of the NAs to 0
  # race_wide[is.na(race_wide)] <- 0
  # 
  # # Create a vector of the variable column names other than ResearchID
  # race_vars <- race_wide %>% 
  #   select(-ResearchID) %>% 
  #   names()
  # 
  # # Create a vector of column names that will be subsequently used to 
  # # rename columns, AND apply the names of the race_vars as labels
  # col_names <- ""
  # for (i in seq_along(race_vars)){
  #   label <- race_vars[i]
  #   col_names[i] <- str_c("Race_", i)
  #   label(race_wide[[race_vars[i]]]) <-  label
  # }
  # 
  # # Rename the race columns using the col_names vector created above
  # race_wide <- race_wide %>%
  #   rename_at(all_of(race_vars), ~col_names)
  # 
  # # Merge df and race_wide together by ResearchID
  # df <- left_join(df, race_wide, by = "ResearchID")
  
  
  # Convert the numerical survey questions to ordered factor and label the 
  # values
  df <- df %>%
    mutate_at(vars(starts_with("ProfID")), 
              ~ordered(.,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral",
                                  "Agree",
                                  "Strongly Agree")))
  
  ## 4. Label the survey question columns  -------------------------------------
  # The survey questions and the corresponding column name that they map onto
  # are located in the df_codes data frame. Names of the columns must be set to
  # be able to dynamically index them in a function. Should be found in the 
  # first two columns of names
  q_names <- (df_codes %>% names())[1:2]
  
  # Filter for the corresponding questions in the rows matching ProfID,
  # select the columns using q_names, transpose the data, and convert to 
  # data frame
  q_labels <- df_codes %>% 
    filter(grepl("ProfID", `Dataset.name:`)) %>%
    select(all_of(q_names)) %>% 
    t() %>%
    as.data.frame()
  
  # Use the first row of the transposed data frame to set the column names
  colnames(q_labels) <- str_trim(q_labels[1,])
  
  # Then remove the first row and the row names
  q_labels <- q_labels[2,]
  rownames(q_labels) <- NULL
  
  # Set the names of the question variables to label
  q_vars <- names(q_labels)
  
  # Loop through the ProfID columns and assign the question label after 
  # removing the un-necessary text. df needs double brackets to index the 
  # column names in a loop
  for (i in seq_along(q_vars)){
    col_label <- sub(".*: ", "", q_labels[1,i])
    label(df[[q_vars[i]]]) <-  col_label
  }
  
  return(df)
}