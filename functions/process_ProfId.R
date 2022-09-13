# Process Professional ID questions ############################################
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus. Dept. of Family Medicine
# Process Professional ID questions

# Questions 3, 4, and 5 in the professional identification instrument of the 
# end of phase survey need to be reverse-scored because higher scores indicate 
# lower levels of professional identification. In the remaining questions, 
# higher scores indicate higher levels of professional identification. Reverse-
# scoreing questions 3, 4, and 5, will ensure that all questions scores can be
# interpreted in a consistent manner.
# 
# Input is a data frame containing ProfId questions
# Output is a data frame containing ProfId questions with the scores in 
# questions 3, 4, and 5 reveresed. 
# ##############################################################################

proc_ProfId <- function(df_name){
    df <- get(df_name, envir = .GlobalEnv)
    
    # Convert ProfId questions to numeric
    df %<>% 
      mutate(
        across(
          ProfID_1:ProfID_9, ~ as.numeric(.)
          )
        )
    
    # Reverse score ProfId3:ProfId5
    df %<>%
      mutate(
        across(
          ProfID_3:ProfID_5, ~ recode(.x, 
                                      `5` = 1, 
                                      `4` = 2,
                                      `3` = 3,
                                      `2` = 4,
                                      `1` = 5)
          )
        )
    
    # Assign df back to global environment
    assign(df_name, df, envir = .GlobalEnv)
}


