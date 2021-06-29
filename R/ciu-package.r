#' @details
#' This package implements the Contextual Importance and Utility (CIU)
#' concepts for Explainable AI (XAI).
#' CIU allows explaining outputs values of any regression or
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
#' of class `CIU`. If the `ciu` object is created by `ciu <- ciu.new(...)`,
#' then different methods can be called as `ciu$explain()`,
#' `ciu$barplot.ciu()` etc. for obtaining explanations in different forms.
#'
#' `ciu` is implemented using an "old style" (?) R object orientation. However,
#' it provides object-oriented encapsulation of variables and methods of the
#' `CIU` object, which presumably helps to avoid name conflicts with other
#' packages or user code.
#'
#' @references Främling, K. *Explainable AI without Interpretable Model*. 2020, <https://arxiv.org/abs/2009.13996>.
#' @references Främling, K. *Decision Theory Meets Explainable AI*. 2020, <doi.org/10.1007/978-3-030-51924-7_4>.
#' @references Främling, K. *Modélisation et apprentissage des préférences par réseaux de neurones pour l'aide à la décision multicritère*. 1996, <https://tel.archives-ouvertes.fr/tel-00825854/document> (title translation in English: *Learning and Explaining Preferences with Neural Networks for Multiple Criteria Decision Making*)
#'
#' @aliases ciu-package
# @useDynLib ciu
#' @importFrom Rcpp sourceCpp
'_PACKAGE'

