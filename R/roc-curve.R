#' ROC Curve
#'
#' Plots the ROC curve. This is a graph to evaluate the model.
#' The larger the area under the red curve within the square, the better the model. 
#'
#' @param pred the prediction gathered from the constrcuted model
#'
#'
#' @examples
#' # pr is a vector of the predictions, we now create a standardized prediction pbject
#' pred <- prediction(pr, test_dat$depvar)
#' roccurve(pred)
#'
#' @export
roccurve = function(pred){
  pe <- performance(pred, "tpr", "fpr")
  au <- performance(pred, "auc")@y.values[[1]]
  pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  p <- ggplot(pd, aes(x=fpr, y=tpr))
  p <- p + geom_line(colour="red")
  p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
  p <- p + ggtitle("ROC Curve, Target: Fatality Indicator")
  p <- p + theme(plot.title=element_text(size=10))
  p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
  p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                    label=paste("AUC =", round(au, 2)))
  print(p)
} 