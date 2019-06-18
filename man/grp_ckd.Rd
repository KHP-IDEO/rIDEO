\name{grp_ckd}
\alias{grp_ckd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates CKD groups from numeric eGFR values. The new variable is created as an ordered factor.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grp_ckd(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector of eGFR values.
}
\item{style}{
Select the presentation of the category labels. 'name' [default] are the standard CKD Stage labels: "Stage 1" through to "Stage 5". 'num' gives the numeric ranges, as character labels: ">90.0" down to "<15.0".
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
https://renal.org/information-resources/the-uk-eckd-guide/ckd-stages/
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
# numeric eGFR values
data <- sample(1:150, 50)

# create new ordered factor column giving corresponding obesity group values
ckd_groups <- grp_ckd(data)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
