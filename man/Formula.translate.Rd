\name{Formula.translate}
\alias{Formula.translate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An internal function to translate an R formula from a character string into an R list object.
}
\description{
A model formula, as a character string written in R-type syntax, is translated into an R list object.
}
\usage{
Formula.translate(Formula, levID, D = "Normal", indata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Formula}{
A character string specifying a multilevel model. See \code{Value} for details. 
}
  \item{levID}{
A character (vector) specifying the level ID(s).
}
  \item{D}{
A character string/vector specifying the distribution to be modelled.
}
  \item{indata}{
A data.frame object containing the data to be modelled.
}
}

\value{
\item{\code{~}}{ is used to separate response variable(s) and explanatory variable(s);}
\item{\code{()}}{ are used to specify each random variable in the model together with its fixed/random part information;}
\item{\code{|}}{ separates explanatory variable(s) (placed to the right of \code{|}) from the fixed/random part information (placed to the left of \code{|}) when placed within \code{()};}
\item{\code{[]}}{ when placed immediately after an explanatory variable, indicates that the variable is categorical. The string in the \code{[]} represents the reference category; if empty, no reference category is used;}
\item{\code{:}}{ indicates an interaction term: i.e. the variables adjacent to \code{:}, and separated by it, are interacted with each other;}
\item{\code{0}}{ when placed to the left of \code{|} within \code{()} indicates that the variables to the right of \code{|} within the same \code{()} are to be added to the fixed part of the model;}
\item{\code{1}}{ when placed to the left of \code{|} within \code{()} indicates that the coefficients of the variables placed to the right of \code{|} within the same \code{()} are to be allowed to randomly vary at level 1;}
\item{\code{2}}{ when placed to the left of \code{|} within \code{()} indicates that the coefficients of the variables to the right of \code{|} within the same \code{()} are to be allowed to randomly vary at level 2 (and so on for \code{3} for level 3, etc.);}
\item{\code{0s/0c}}{ when placed to the left of \code{|} within \code{()} indicates that separate (hence \code{s}) / common (hence \code{c}) coefficients for the variables to the right of \code{|} within the same \code{()} are to be added to the fixed part (hence \code{0}) of multivariate normal, multinomial and mixed responses models;}
\item{\code{2s/2c}}{ when placed to the left of \code{|} within \code{()} indicates that separate (hence \code{s}) / common (hence \code{c}) coefficients for the variables to the right of \code{|} within the same \code{()} are to be added to the random part of the model, and allowed to vary at level 2; applies to multivariate normal, multinomial and mixed responses models only;}
\item{ }{ ...}
\item{\code{\{\}}}{ gives a vector of binary indicators specifying a common coefficient. 1 is to include the component at the corresponding positions; zero otherwise. These digits are separated by commas; applies to multivariate normal, multinomial and mixed responses models only;}
\item{\code{.}}{ is used for adding a separate coefficient for a particular component at a specific level; applies to multivariate normal, multinomial and mixed responses models only;}
see examples in \code{\link{runMLwiN}} for details.

Outputs an R list object, which is then used as the input for \code{\link{MacroScript1}} and/or \code{\link{MacroScript2}}.
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}
\note{
Note that some characters listed above have special meanings in the formula, so avoid using them when you name the random variable. Alphanumeric characters (i.e. `[:alnum:]') are recommended for naming the random variable.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{runMLwiN}},\code{\link{MacroScript1}},\code{\link{MacroScript2}}
}

