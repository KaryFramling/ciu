\name{pie.ciu}
\alias{pie.ciu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pie Chart CIU explanation for specific instance}
\description{Create a pie chart showing CI as the area of the slice and CU on
color scale from red to green, via yellow, for the given inputs and the
given output. First get a \code{CIU} object by calling \link{ciu.new}
as e.g. \code{ciu <- ciu.new(...)}, then call
\code{ciu.res <- ciu$pie.ciu(...)}.
\emph{"Usage" section is in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}
}
%\usage{
%pie.ciu(
%#' Title
%#'
%#' @param instance
%#' @param ind.inputs
%#' @param ind.output
%#' @param in.min.max.limits
%#' @param n.samples
%#' @param neutral.CU
%#' @param show.input.values
%#' @param concepts.to.explain
%#' @param target.concept
%#' @param target.ciu
%#' @param color.ramp.below.neutral
%#' @param color.ramp.above.neutral
%#' @param sort
%#' @param decreasing
%#' @param main
%#' @param ...
%)
%}
%- maybe also 'usage' for other objects documented here.
\arguments{
  Same as for \code{\link{barplot.ciu}}.
}
\details{
\bold{Usage}
\preformatted{
pie.ciu(
  instance,
  ind.inputs=NULL,
  ind.output=1,
  in.min.max.limits=NULL,
  n.samples=100,
  neutral.CU=0.5,
  show.input.values=TRUE,
  concepts.to.explain=NULL,
  target.concept=NULL,
  target.ciu=NULL,
  color.ramp.below.neutral=NULL,
  color.ramp.above.neutral=NULL,
  sort=NULL,
  decreasing=FALSE,
  main=NULL,
  xlab=NULL,
  xlim=NULL,
  ...)
}
}
\value{"void", i.e. whatever happens to be result of last instruction.
}
\seealso{
  \code{\link{barplot.ciu}}
  \code{\link{ciu.new}}
  \code{\link{explain}}
}
\author{
  Kary Främling
}
