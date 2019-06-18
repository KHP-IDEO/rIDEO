\name{grp_bmi}
\alias{grp_bmi}
\title{Creates Obesity groups from numeric BMI values}
\description{
Takes a vector of numeric Body Mass Index values and produces a new vector of grouped obesity values. The new variable is created as an ordered factor.}
\usage{
grp_bmi(x, depth = "full", style = "name")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector of Body Mass Index values
}
  \item{depth}{
Select the level of detail to be mapped to. 'full' [default] contains three levels within the "Obese" factor level, in line with the WHO Obesity classification. 'part' produces a simplified classification, with a single "Obese" category.
}
  \item{style}{
Select the presentation of the category labels. 'name' [default] are the standard WHO Obesity labels: "Underweight", "Healthy Weight" etc. 'num' gives the numeric ranges, as character labels, e.g. "<18.5", "18.5-24.9" etc.
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
https://www.nice.org.uk/guidance/cg189/chapter/1-Recommendations#identification-and-classification-of-overweight-and-obesity
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
# numeric BMI values
data <- rnorm(10, mean = 30, sd = 5)

# create new ordered factor column giving corresponding obesity group values
bmi_groups <- bmi_grp(data))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
