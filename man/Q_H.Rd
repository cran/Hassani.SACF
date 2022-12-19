\name{Q_H}
\alias{Q_H}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing Lower Bound of Ljung-Box Test
}
\description{
Because of the sensitivity of the Ljung-Box test to the number of lags involved in the test, this function computes 
lower bound of this test and draws it's plot.
}
\usage{
Q_H(simnum = 10000, TT = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simnum}{
number of simulation iterations.
}
  \item{TT}{
length of time serie.
}

}

\value{
Lower bound of the Ljung-Box test and it's plot.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Hassani, H., & Yeganegi, M. R. (2019). "Sum of squared ACF and the Ljung-Box statistics." 
Physica A: Statistical Mechanics and Its Applications, 520, 81-86.
}
\author{
Hossein hassani, Masoud yarmohammdi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Box.test}
}
\examples{
\donttest{
Q_H(simnum = 10000, TT = 100)
}
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
