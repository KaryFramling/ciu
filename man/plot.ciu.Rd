\name{plot.ciu}
\alias{plot.ciu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot output value as a function of one input for a specific instance}
\description{Plot how the value of one output changes as a function of one
input, as a line chart. The current input/output values are indicated by a
red dot. The values of all other inputs are the ones given by the
\code{instance} parameter. This method is not specific for CIU but it
allows to study the behaviour of the underlying "black-box model". It also
makes it easy to understand how CI and CU values have been calculated.
}
\arguments{
  \item{instance}{Instance to explain. See \code{\link{explain}}.}
  \item{ind.input}{Index of the input to plot}
  \item{ind.output}{Index of the output to plot}
  \item{in.min.max.limits}{See \code{\link{explain}}.}
  \item{n.points}{The number of points to use on X-axis for plotting.}
  \item{main, xlab, ylab, ylim, ...}{Usual plot parameters, possible to
  override the default ones provided here if needed.}
}
\details{
First get a \code{CIU} object by calling \link{ciu.new} as e.g. \code{ciu <- ciu.new(...)},
then call \code{ciu.res <- ciu$plot.ciu(...)}. \emph{"Usage" section is here in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}
\bold{Usage}
\preformatted{
plot.ciu(
  instance,
  ind.input,
  ind.output,
  in.min.max.limits=NULL,
  n.points=40,
  main=NULL,
  xlab=NULL,
  ylab=NULL,
  ylim=NULL,
  ...)
}
}
\value{"void", i.e. whatever happens to be result of last instruction.
}
\author{
  Kary Främling
}
