#' @details
#' This package implements the Contextual Importance and Utility (CIU)
#' concepts for Explainable AI (XAI).
#' CIU allows explaining output values of any regression or
#' classification systems, no matter if it is a "black-box" or a "white-box"
#' AI, or anything between black and white. CIU is entirely
#' model-agnostic. Contrary to most (all?) other XAI methods, CIU provides
#' explanations directly based on the observed input-output behavior without
#' building an intermediate "interpretable" model for doing it.
#'
#' CIU was developed by Kary Främling in his PhD thesis, which was presented
#' in 1996 (in French). CIU was first presented in 1995 at the International
#' Conference on Artificial Neural Networks (ICANN).
#'
#' The ciu package supports models from `caret` and at least `lda` natively,
#' but can easily be made to work with any model.
#'
#' **Main functions:**
#'
#' Use of `ciu` starts by calling the function [ciu.new] that returns an object
#' of class `CIU`. If the `CIU` object is created by `ciu <- ciu.new(...)`,
#' then different methods can be called as `ciu$explain()`,
#' `ciu$barplot.ciu()` etc. for obtaining explanations in different forms.
#'
#' `ciu` is implemented using an "old style" (?) R object orientation. However,
#' it provides object-oriented encapsulation of variables and methods of the
#' `CIU` object, which presumably helps to avoid name conflicts with other
#' packages or user code.
#'
#' Since version 0.5.0 it is also possible to use a non-object-oriented approach
#' by creating an ordinary [list] of class `ciu` by calling the function [ciu].
#' That `ciu` object is then
#' passed as the first parameter to the different functions. This parallel
#' possibility was originally developed mainly for getting support for proper
#' Roxygen functionality. However, it does also offer some interesting properties,
#' e.g. a `CIU` object takes up much more memory than a `ciu` object because it
#' creates its own environment. `CIU` objects can be converted to `ciu` objects
#' and vice versa at any time by the `<CIU>$as.ciu()` method and the [ciu.to.CIU]
#' function.
#'
#' It is recommended to use the object-oriented approach in order to
#' avoid unnecessary conversions back and forth. However, the difference is
#' presumably not very significant.
#'
#' @references Främling, K. *Decision Theory Meets Explainable AI*. 2020, <doi:/10.1007/978-3-030-51924-7_4>.
#'
#' @aliases ciu-package
# @useDynLib ciu
#' @importFrom Rcpp sourceCpp
'_PACKAGE'

