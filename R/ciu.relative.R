#' Calculate CIU of a sub-concept/input relative to an intermediate
#' concept (or output).
#'
#' Calculate CIU of a sub-concept/input relative to an intermediate
#' concept (or output). The parameters must be of class "ciu.result" or a
#' [data.frame] with compatible columns.
#' @param sub.ciu.result ciu.result object of sub-concept/input.
#'
#' @param sup.ciu.result ciu.result object of intermediate concept/output.
#'
#' @export ciu.relative
ciu.relative <- function(sub.ciu.result, sup.ciu.result) {
  ciu.rel <- sub.ciu.result # Only CI changes, rest remains the same.
  ciu.rel$CI <- (sub.ciu.result$cmax - sub.ciu.result$cmin)/
    (sup.ciu.result$cmax - sup.ciu.result$cmin)
  return(ciu.rel)
}

