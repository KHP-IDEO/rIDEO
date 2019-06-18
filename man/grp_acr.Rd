\name{grp_acr}
\alias{grp_acr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creates Albuminuria groups from numeric ACR values. The new variable is created as an ordered factor.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grp_acr(x, style = "name")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector of ACR values [expecting mg/mmol units].
}
  \item{style}{
Select the presentation of the category labels. 'name' [default] are the standard albuminuria labels: "A1", "A2", "A3". 'num' gives the numeric ranges, as character labels: "<3.0", "3.0-30.0", ">30.0". 'alb' gives descriptive labels to interpret these results: 'Normal albuminuria', 'Microalbuminuria', 'Macroalbuminuria'.
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
https://renal.org/information-resources/the-uk-eckd-guide/proteinuria/
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
# numeric ACR values; unrealistic range, but functional
data <- sample(0.1:50, 50)

# create new ordered factor column giving corresponding obesity group values
acr_groups <- grp_acr(data)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
