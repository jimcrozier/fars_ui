#' Decile Accuracy Plot
#'
#' Creates the decile prediction accuracy plot on the modeling -> model scores page. 
#' This plot shows the distribution of how probable death is given an accident.
#'
#' @param predout a pretty heavily processed prediction vector
#'
#'
#' @examples
#' pred <- model_fn(...)$pred  # grab pred from model_fn
#' pred <- pred %>% dplyr::mutate(quartile = ntile(pred, 10))  # add a new column to pred called 'quartile', which is actually the corresponding decile haha
#' predout <- dplyr::summarise( dplyr::group_by(pred, quartile), pred = mean(as.numeric(depvar))) # make a new df, grouped by 'quartile', with a new column being the mean of depvar (within that decile)
#' decileAccuracyPlot(predout)
#'
#' @export
decileAccuracyPlot = function(predout){
  predout =  predout %>% filter(!is.na(predout$pred))
  barplot(predout$pred, col = "steelblue",
          main = paste("Decile Predicition Accuracy \n Power*: ", round(predout$pred[10]/mean(predout$pred,na.rm=T),2)),
          xlab = "Decile \n *Power is top decile/average",names.arg =1:NROW(predout), ylab= "Likelihood of Death | Accident")
  abline(h=mean(predout$pred,na.rm=T))
}