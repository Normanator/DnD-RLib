% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiceRoll.R
\name{withAdvantage}
\alias{withAdvantage}
\title{Advantage or disadvantage rolls}
\usage{
withAdvantage(rollFunc = NULL, disAdv = FALSE)
}
\arguments{
\item{rollFunc}{optionally is roll to function to repeat (default D20)}

\item{disAdv}{set TRUE picks the min of the two rolls (default FALSE)}
}
\value{
The best (or worst) or two calls to rollFunc
}
\description{
\code{withAdvantage} picks the maximum of two rolls of the rollFunc,
or if disAdv=TRUE the minimum.
}
\examples{
  rogueHiddenAttack <- withAdvantage( makeRoll(1,20,5) )
  blindAttack       <- withAdvantage( makeRoll(1,20,5), disAdv=TRUE )

}

