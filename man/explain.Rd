\name{explain}
\alias{explain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate CIU for specific instance}
\description{Calculate Contextual Importance (CI) and Contextual Utility (CU)
for an instance (Context) using the given "black-box" model. First get a
\code{CIU} object by calling \code{ciu.new} as e.g. \code{ciu <- ciu.new(...)},
then call as \code{ciu.res <- ciu$explain(...)}. \emph{"Usage" section is in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}
}
% "usage" section is not allowed, doesn't pass the devtools::check because
% there's no corresponding "top-level" function. So have to do it differently.
%\usage{
%explain(
%instance,
%ind.inputs.to.explain,
%in.min.max.limits=NULL,
%n.samples=100,
%target.concept=NULL,
%target.ciu=NULL
%)
%}
%- maybe also 'usage' for other objects documented here.
\details{


\bold{Usage}

\preformatted{
explain(
  instance,
  ind.inputs.to.explain,
  in.min.max.limits=NULL,
  n.samples=100,
  target.concept=NULL,
  target.ciu=NULL
)
}
}

\arguments{
  \item{instance}{Input values for the instance to explain.
  Should be a \link{data.frame} even though a \code{\link{vector}} or
  \code{\link{matrix}} might
  work too if input names and other needed metadata can be deduced from the
  dataset or other parameters given to \code{\link{ciu.new}}.}
  \item{ind.inputs.to.explain}{\code{\link{vector}} of indices for the
  inputs to be explained, i.e. for which CIU should be calculated.
  If NULL, then all inputs will be included.}
  \item{in.min.max.limits}{\code{\link{data.frame}} or \code{\link{matrix}}
  with one row per output and
  two columns, where the first column indicates the minimal value and the
  second column the maximal value for that output. ONLY NEEDED HERE IF not
  given as parameter to \code{\link{ciu.new}} or if the limits are different
  for this specific instance than the default ones.}
  \item{n.samples}{How many instances to generate for estimating CI and CU.
  For inputs of type \code{\link{factor}}, all possible combinations of input values
  is generated, so this parameter only influences how many instances are
  (at least) generated for continuous-valued inputs.}
  \item{target.concept}{If provided, then calculate CIU of inputs
  \code{ind.inputs.to.explain} relative to the given concept rather than
  relative to the actual output(s). \code{ind.inputs.to.explain} should
  normally be a subset (or all) of the inputs that \code{target.concept}
  consists of, even though that not required by the CIU calculation.
  If a "target.ciu" is provided, then the "target.concept" doesn't have to
  be included in the \code{vocabulary} gives as parameter to
  \code{\link{ciu.new}} (at least for the moment).}
  \item{target.ciu}{\code{ciu.result} object previously calculated for
  \code{target.concept}. If a \code{target.concept}
  is provided but \code{target.ciu=NULL}, then \code{target.ciu} is
  estimated by a call to \code{explain} with the \code{n.samples} value given as
  a parameter to this call. It may be useful to provide \code{target.ciu}
  if it should be estimated using some other (typically greater) value for
  \code{n.samples} than the default one, or if it has already been calculated
  for some reason.}
}

\value{A \code{ciu.result} object as returned by \code{\link{ciu.result.new}}
}
\seealso{
  \code{\link{ciu.result.new}}
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
