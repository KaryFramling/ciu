\name{barplot.ciu}
\alias{barplot.ciu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Barplot CIU explanation for specific instance}
\description{Create a barplot showing CI as the length of the bar and CU on color scale from
red to green, via yellow, for the given inputs and the given output. First get a
\code{CIU} object by calling \link{ciu.new} as e.g. \code{ciu <- ciu.new(...)},
then call \code{ciu.res <- ciu$barplot.ciu(...)}. \emph{"Usage" section is in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}
}
\arguments{
  \item{instance}{Instance to explain. See \code{\link{explain}}.}
  \item{ind.inputs}{\code{\link{vector}} of indices for the
  inputs to be included in the plot.
  If NULL then all inputs will be included.}
  \item{ind.output}{Index of output to be explained.}
  \item{in.min.max.limits}{See \code{\link{explain}}.}
  \item{n.samples}{See \code{\link{explain}}.}
  \item{neutral.CU}{Indicates when the Contextual Utility is considered
to be "negative". The default value of 0.5 seems quite logical for most cases.}
  \item{show.input.values}{Include input values after input labels or not.
  Default is TRUE.}
  \item{concepts.to.explain}{List of concepts to use in the plot, as defined
  by vocabulary provided as argument to \code{\link{ciu.new}}.
  If \code{ind.inputs=NULL}, then use \code{concepts.to.explain} instead.
  If both are NULL, then use all inputs.}
  \item{target.concept}{See \code{\link{explain}}.}
  \item{target.ciu}{See \code{\link{explain}}.}
  \item{color.ramp.below.neutral}{Color ramp function as returned by function
  \code{colorRamp()}. Default color ramp is from red3 to yellow.}
  \item{color.ramp.above.neutral}{Color ramp function as returned by function
  \code{colorRamp()}. Default colorramp is from yellow to darkgreen.}
  \item{use.influence}{Plot using "influence" rather than CIU, i.e. a
  LIME-like barplot. Default is FALSE.}
  \item{sort}{NULL, "CI" or "CU". No sorting by default, other options are
  sorting by CI or CU.}
  \item{decreasing}{Set to TRUE for decreasing sort.}
  \item{main, xlab, xlim, ...}{Usual plot parameters, possible to override the
  default ones provided here if needed.}
}
\details{
\bold{Usage}
\preformatted{
barplot.ciu(
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
  \code{\link{ggplot.col.ciu}}
  \code{\link{pie.ciu}}
  \code{\link{ciu.new}}
  \code{\link{explain}}
}
\author{
  Kary Främling
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
