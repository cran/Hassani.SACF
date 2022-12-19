\name{SACF}
\alias{SACF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing Sum of the Sample Autocorrelation Function
}
\description{
The sum of the sample autocorrelation function, found in many standard
time series textbooks and software, at lag h is considered. 
It is shown that this sum is always minus half  for any stationary time series with 
arbitrary length L.
}
\usage{
SACF(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
it is stationary time series.
}
}

\value{
A number. It computes SACF.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
A note on the sum of the sample autocorrelation function
Hossein Hassani 
Statistics Group, Cardiff School of Mathematics, Cardiff University, CF24 4AG, UK
2-Statistical Research and Training Center, Tehran, 1413717911, Iran
}
\author{
Hossein hassani, Masoud yarmohammdi, Mohammad reza yeganegi and Leila Marvian Mashhad.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Box.test}
}
\examples{
x = rnorm(50,mean = 0,sd = 1)
SACF(x)
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
