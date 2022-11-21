#' CIU result object
#'
#' Create object of class `ciu.result`, which stores results of CIU
#' calculations. The `ciu$explain` and [ciu.explain] methods return a `ciu.result` object.
#'
#' @param ci vector of CI values, one per output
#' @param cu vector of CU values, one per output
#' @param cmin vector of cmin values, one per output
#' @param cmax vector of cmax values, one per output
#' @param outval vector of black-box output values, one per output
#'
#' @return An object of class ciu.result, which is a data.frame with
#' (at least) five columns:
#' - `CI values`: one row per output of the black-box model
#' - `CU values`: one row per output of the black-box model
#' - `cmin values`: one row per output of the black-box model
#' - `cmax values`: one row per output of the black-box model
#' - `outval values`: one row per output of the black-box model
#'
#' @export
#' @author Kary Främling
ciu.result.new <- function(ci, cu, cmin, cmax, outval) {
  ciu.result <- data.frame(CI=ci, CU=as.numeric(cu),
                           cmin=cmin, cmax=cmax, outval=outval)
  if ( !is.null(names(ci)) )
    rownames(ciu.result) <- names(ci)
  class(ciu.result)<-c("ciu.result","data.frame")
  return(ciu.result)
}

