\name{toLongDate}
\alias{toLongDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts short (old) 'z-Tree' dates into long (new) 'z-Tree' dates.}
\description{
Old versions of 'z-Tree' stored dates as, e.g. 050613JN (the first six
being year-month-date, the last two characters are the time). Newer versions
use strings like 091112_1600 (time is now in the last four
characters). This function converts the old format into the new format
}
\usage{
toLongDate(shortDate)
}
\arguments{
  \item{shortDate}{This should be a vector of strings, each of length 8 characters.
    The first six characters of each element of \code{shortDate} are year, month,
    date. The last two chacters encode the time of the experiment.
}
}
\value{A vector of the same length as \code{shortDate}. The first six
  characters of each element are unchanged. The last five characters are
  now the time translated.
}
\references{
  Urs Fischbacher. "z-Tree Manual 2.1 Experimenter's Manual". Zurich. 2002.
}
\author{Oliver Kirchkamp
}

\examples{
toLongDate("091112JN")
}
%
\keyword{data}
\keyword{character}
