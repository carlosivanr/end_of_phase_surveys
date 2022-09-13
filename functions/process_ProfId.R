# Process Professional ID questions ############################################
# Input is a data frame
# Output is a data frame
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


