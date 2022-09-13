# Reverse Score ordered survey responses #######################################
# Input is a data frame
# Output is a data frame
# ##############################################################################

reverse_score <- function(df_name){
    df <- get(df_name, envir = .GlobalEnv)
    
    # Alter questions 3,4,5
    df %>% 
      mutate(
 


  
  
  }