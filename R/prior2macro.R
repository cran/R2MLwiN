#' Translates informative prior information into a concise MLwiN macro.
#'
#' An internal function which takes an R list object containing informative
#' prior information for a multilevel model and translates it into a concise
#' vector object to be used in an MLwiN macro.
#'
#' @param prior An R list object containing prior information for a multilevel
#' model. See `Details' below.
#' @param D A character string specifying the type of distribution, which
#' can be one of \code{'Normal'}, \code{'Binomial'}, \code{'Poisson'},
#' \code{'Negbinom'}, \code{'Multinomial'}, \code{'Multivariate Normal'},
#' or \code{'Mixed'}
#' @param fpart An R list containing the list of fixed part parameter labels.
#' @param nrand An R list of lists, containing the number of random parameters
#' at each level.
#'
#' @details
#' The \code{prior} list can contain the following:
#' \itemize{
#' \item \code{fixe}: For the fixed parameters, if proper normal priors are used
#' for some parameters, a list of vectors of length two is provided, each of which
#' specifies the mean and the standard deviation. If not given, default ('flat' or 'diffuse')
#' priors are used for the parameters. The names used in the list should match those in the
#' model output.
#' \item \code{rp<level number>}: A list object specifying the Wishart or gamma prior for the
#' covariance matrix or scalar variance at the levels specified, e.g. \code{rp1} for
#' level 1, \code{rp2} for level 2, etc. Consists of: (1)
#' \code{estimate} -- a prior guess for the true value of the covariance matrix;
#' (2) \code{size} -- sample size for guess.
#' Note that this is a weakly-informative prior and the default prior
#' is used if missing.
#' }
#'
#' @return A long vector is returned in the format of MLwiN macro language. This
#' includes all the specified prior parameters.
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso \code{\link{runMLwiN}}
#'
prior2macro <- function(prior, D, fpart, nrand) {
  TT <- NULL
  # Merge in any priors specified as common/separate 
  if (!is.null(prior$fixe.common)) {
    prior$fixe <- c(prior$fixe, prior$fixe.common)
    prior$fixe.common <- NULL
  }
  if (!is.null(prior$fixe.sep)) {
    prior$fixe <- c(prior$fixe, prior$fixe.sep)
    prior$fixe.sep <- NULL
  }

  for (fvar in fpart) {
    if (is.null(prior$fixe) == FALSE) {
      names(prior$fixe) <- gsub("^1", "Intercept", names(prior$fixe))
    }
    if (fvar %in% names(prior$fixe)) {
      if (length(prior$fixe[[fvar]]) != 2) {
        stop("Either both mean and SD have to be specified, or neither")
      }
      if (prior$fixe[[fvar]][2] < 0) {
        stop("Prior standard deviation must be positive")
      }
      TT <- c(TT, 1, prior$fixe[[fvar]][1], prior$fixe[[fvar]][2])
      prior$fixe[[fvar]] <- NULL
    } else {
      TT <- c(TT, 0)
    }
  }
  
  for (rlev in names(nrand)) {
    if (rlev %in% names(prior)) {
      if (is.null(prior[[rlev]]$estimate) || is.null(prior[[rlev]]$size)) {
        stop(paste0("Both prior matrix and sample size need to be specified for ", rlev))
      }
      mat <- as.matrix(prior[[rlev]]$estimate)
      if (nrow(mat) != nrand[[rlev]] || ncol(mat) != nrand[[rlev]]) {
        stop(paste0("Prior matrix at level ", rlev, " incorrect size"))
      }
      mat[upper.tri(mat)] <- mat[lower.tri(mat)]
      testpd <- try(chol(mat), silent=TRUE)
      if (class(testpd) == "try-error") {
        stop(paste0("Prior matrix at level ", rlev, " must be positive-definite"))
      }
      if (prior[[rlev]]$size < 0) {
        stop(paste0("Sample size for ", rlev, " must be a positive integer"))
      }
      tt <- c(as.vector(mat[upper.tri(mat, diag = TRUE)]), prior[[rlev]]$size)
      TT <- c(TT, 1, tt)
      prior[[rlev]] <- NULL
    } else {
      TT <- c(TT, 0)
    }
  }

  # Response indicator level for Multivariate/Multinomial
  if (D == "Multivariate Normal" || D == "Mixed" || D == "Multinomial") {
    TT <- c(TT, 0)
  }

  for (var in names(prior$fixe)) {
    warning(paste0("Unmatched prior: ", var))
  }
  prior$fixe <- NULL
  for (rlev in names(prior)) {
    warning(paste0("Unmatched prior: ", rlev))
  }

  TT
}
