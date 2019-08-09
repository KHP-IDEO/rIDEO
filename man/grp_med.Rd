\name{grp_med}
\alias{grp_med}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates factor variable grouping medications into sub-types relevant to diabetes care
}
\description{
Takes a vector of drug names (generic and brand names accepted) and allocates to relevant level of created grouped factor.
The default 'full' depth classifies oral anti-diabetics (OADs) and anti-hypertensives into sub-classes. Optional 'part' depth, groups these
drug classes into broad factor levels.
}
\usage{
grp_med(x, depth = "full")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character or factor variable containing medicine names.
}
  \item{depth}{
Default ('full') maps detailed drug classes into discrete factor levels; 'part' uses broader drug groups.
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
https://www.diabetes.co.uk/Diabetes-drugs.html

http://www.bloodpressureuk.org/BloodPressureandyou/Medicines/Medicinetypes

}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
