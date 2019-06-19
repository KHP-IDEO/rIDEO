\name{tgt_hba1c}
\alias{tgt_hba1c}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a set of logical variables which compare HbA1c values to a range of treatment target levels.
}
\description{
The function takes an existing dataframe, which contains a variable of HbA1c test results, and measures the result against
a set of treatment target levels. The target levels are set at 0.5 per cent intervals (DCCT), ranging from 6.5 per cent through to 9.0 per cent, with a final 10.0 per cent upper level. A separate logical variable is produced for each target, with a TRUE or FALSE value to indicate whether the test value is below the respective target levels.
The full set of target variables are appended to the existing dataframe.
}
\usage{
tgt_hba1c(x)
}

\arguments{
  \item{x}{
Dataframe containing a variable of HbA1c test results. The created target variables will be appended to this dataframe.
}
\item{y}{
Numeric vector of HbA1c values [IFCC mmol/mol].
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
# A numeric variable of HbA1c values, within a dataframe
data <- tibble::tibble(hba1c_values = rnorm(25, 65, 15))

# Measure the HbA1c test results against the full set of targets, adding logical variables to the dataframe
data <- tgt_hba1c(data, data$hba1c_values)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
