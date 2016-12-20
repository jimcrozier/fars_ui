#' Summaries
#'
#' Creates a data frame that summarize the input df, with respect
#' to the variable input_nm.
#'
#' @param input_nm the variable name on which to summarize
#' @param df the data frame in which the data sits
#' @param todummies a boolean that if true, the data will be makde into dummy variables
#' @param todummies3 a boolean that if true, the data will be makde into dummy variables
#' @param runnames the variables selected in the model - only used in commented out dummy variable code
#' 
#'
#'
#' @examples
#' summaries('night', 
#'              df = dataIn,
#'              todummies = FALSE,
#'              todummies3 = FALSE,
#'              runnames = c("dr_drink", "roadtype", "lighting", "weathercond"))
#'
#' @export
summaries = function(input_nm, df, todummies, todummies3, runnames){
  #runnames = input$varsinmodel
  #if(todummies | todummies3){
  #  for (i in 1:NROW(runnames)){
  #    if(!runnames[i] %in% c("Gender","Age-range")) df[,runnames[i]] = ifelse(df[,runnames[i]]>1,1,0)
  #  }
  # }
  df = df %>% select_("depvar", input_nm)
     df[,input_nm]= clean_levels(df[,input_nm],50)
  

  sums = data.frame(varname = input_nm, 
                    dplyr::summarize(group_by_(df, as.name(paste(input_nm))),
                                     n = n(), 
                                     pct_n = n()/NROW(df),
                                     avg = mean(depvar)))
  names(sums) = c("varname", "value","n", "pct_n", "avg_fatality")
  #sums$pct_n = sums$n/NROW(df)
  sums$value = as.character(sums$value)
  return(sums)
}