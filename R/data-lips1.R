#' Lips data
#' 
#' Observed counts of male lip cancer for the 56 regions of Scotland over the
#' period 1975-1980.
#' 
#' The \code{lips1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009), and was
#' analysed in Clayton & Kaldor (1987); see also Browne (2012) for more
#' details.
#' 
#' @docType data
#' @format A data frame with 56 observations on the following 41 variables:
#' \describe{ \item{list("area")}{Region ID.} \item{list("cons")}{Constant
#' (=1).} \item{list("obs")}{Observed cases of lip cancer.}
#' \item{list("exp")}{Expected count.} \item{list("perc_aff")}{Percentage of
#' the region who work in agriculture, fishing and forestry.}
#' \item{list("offs")}{Log of the expected count.}
#' \item{list("pcons")}{Constant (=1).} \item{list("denom")}{Constant (=1).}
#' \item{list("neigh1")}{First neighbours.} \item{list("neigh2")}{Second
#' neighbours.} \item{list("neigh3")}{Third neighbours.}
#' \item{list("neigh4")}{Fourth neighbours.} \item{list("neigh5")}{Fifth
#' neighbours.} \item{list("neigh6")}{Sixth neighbours.}
#' \item{list("neigh7")}{Seventh neighbours.} \item{list("neigh8")}{Eighth
#' neighbours.} \item{list("neigh9")}{Ninth neighbours.}
#' \item{list("neigh10")}{Tenth neightbours.} \item{list("neigh11")}{Eleventh
#' neightbours.} \item{list("weight1")}{First neighbours' weights.}
#' \item{list("weight2")}{Second neighbours' weights.}
#' \item{list("weight3")}{Third neighbours' weights.}
#' \item{list("weight4")}{Fourth neighbours' weights.}
#' \item{list("weight5")}{Fifth neighbours' weights.}
#' \item{list("weight6")}{Sixth neighbours' weights.}
#' \item{list("weight7")}{Seventh neighbours' weights.}
#' \item{list("weight8")}{Eighth neighbours' weights.}
#' \item{list("weight9")}{Ninth neighbours' weights.}
#' \item{list("weight10")}{Tenth neightbours' weights.}
#' \item{list("weight11")}{Eleventh neightbours' weights.}
#' \item{list("wcar1")}{First neighbours' CAR weights.}
#' \item{list("wcar2")}{Second neighbours' CAR weights.}
#' \item{list("wcar3")}{Third neighbours' CAR weights.}
#' \item{list("wcar4")}{Fourth neighbours' CAR weights.}
#' \item{list("wcar5")}{Fifth neighbours' CAR weights.}
#' \item{list("wcar6")}{Sixth neighbours' CAR weights.}
#' \item{list("wcar7")}{Seventh neighbours' CAR weights.}
#' \item{list("wcar8")}{Eighth neighbours' CAR weights.}
#' \item{list("wcar9")}{Ninth neighbours' CAR weights.}
#' \item{list("wcar10")}{Tenth neightbours' CAR weights.}
#' \item{list("wcar11")}{Eleventh neightbours' CAR weights.} }
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Clayton, D., Kaldor, J. (1987) Empirical Bayes estimates of age-standardized
#' relative risks for use in disease mapping. \emph{Biometrics} 43: 671-681.
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(lips1, package = "R2MLwiN")
#' 
#' (mymodel <- runMLwiN(log(obs) ~ 1 + perc_aff + offset(offs) + (0 | neigh1) + (1 | area),
#'  D = "Poisson", estoptions = list(EstM = 1), data = lips1))
#' 
#' }
#' 
"lips1"