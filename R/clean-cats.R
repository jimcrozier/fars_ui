#' Make clean categories
#'
#' This function returns a vector that is the the categories
#' that exist in the input vector 'input_nm' ("input name"), 
#' but only if they're above a certain threshold. If not, 
#' they're thrown into the "Other" category.
#' 
#'
#' @param df the data frame
#' @param input_nm the name of the column in df we want to alter
#' @param threshold how many instances a level must have to not get replaced by 'Other'
#'
#' @return the input vector, but with the altered categories.
#'
#' @export
clean_cats = function(df, input_nm,threshold){
  # creates a new column in character form of input_nm
  df$cat = as.character(df[,input_nm])
  
  # replaces NA with "Unkown for all rows in df
  df$cat[is.na(df$cat)] = "Unknown"
  
  # df = group_by(df, cat)
  # Then create a new column in df that is percent_of_n / total rows
  df <- df %>% 
    group_by(cat) %>% 
    mutate(pct_n = n()/NROW(df))
  
  # creates new column called 'cat2' that is the same as 
  # df$cat, UNLESS df$pct_n is above threshold, then df$cat2
  # for that row is "Other"
  df$cat2 = ifelse(df$pct_n>threshold, df$cat, "Other")
  
  return(df$cat2)
}