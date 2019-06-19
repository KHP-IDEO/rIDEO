\name{tgt_chol}
\alias{tgt_chol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a set of logical variables which compare cholesterol values to a range of treatment target levels.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tgt_chol(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Dataframe containing a variable of total cholesterol test results. The created target variables will be appended to this dataframe.
}
\item{y}{
Numeric vector of total cholesterol values [mmol/l].
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julian Collins
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# A numeric variable of total cholesterol values, within a dataframe
data <- tibble::tibble(chol_values = rnorm(25, 5, 2.5))

# Measure the total cholesterol test results against the full set of targets, adding logical variables to the dataframe
data <- tgt_chol(data, data$chol_values)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
