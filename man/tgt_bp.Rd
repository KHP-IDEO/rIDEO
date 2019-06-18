\name{tgt_bp}
\alias{tgt_bp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a set of logical variables which compare blood pressure values to a range of treatment target levels.
}
\description{
The function takes an existing dataframe, which contains variables of both systolic and diastolic observations, and measures these against a set of treatment target levels. The target levels are set as: 130/80, 140/80, 140/90 and 150/90. A separate logical variable is produced for each target, with a TRUE or FALSE value to indicate whether the test value is below the respective target levels.
The full set of target variables are appended to the existing dataframe.
}
\usage{
tgt_bp(x)
}

\arguments{
  \item{x}{
Dataframe containing a set of blood pressure observations. The created target variables will be appended to this dataframe.
}
\item{y}{
Numeric vector of systolic blood pressure values.
}
\item{z}{
Numeric vector of diastolic blood pressure values.
}
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
# systolic bp values
sys <- sample(90:200, 50)

# diastolic bp values
dias <- sample(40:120, 50)

# combine into dataframe [bp pairs may be unrealistic due to randomness]
bp <- tibble::tibble(sys, dias)

# Measure the blood pressure values against the full set of targets, adding logical variables to the dataframe
data <- tgt_bp(bp, sys, dias)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
