#' Create buckets
#'
#' Makes quantiles in which to put continuous data
#'
#' @param input_vec the input vector
#' @param cuts the number of buckets desired
#'
#' @return a factor of length(input_vec), with the index that corresponds to a data point in input_vec
#'         now corresponding to that data point's bucket.
#'
#' @examples
#' v <- seq(400, 28, -3)
#' buckets <- create_buckets(v)
#'
#' @export
create_buckets = function(input_vec,cuts=5){
  # note - this function isn't used anywhere, yet.
  # question - this function throws the minimum of the vector passed in into the bucket '<NA>'. 
  #            what does that mean?
  #
  # If it's a problem let me know and I'll fix it!

  out = as.factor(cut(input_vec,quantile(input_vec,probs = seq(0, 1, 1/cuts)),labels=F))
  return(out)
}