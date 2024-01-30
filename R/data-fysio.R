#' Data on physiotherapy referrals from 100 general practices in the Netherlands,
#' collected in 1987
#' 
#' These data were collected in 1987 as part of a large national survey of general
#' practice (Van der Velden 1999).
#' 
#' The \code{fysio} dataset is one of the example datasets analysed in
#' Leyland and Groenewegen (2020), and provided with the
#' multilevel-modelling software package MLwiN (Charlton et al., 2024).
#' 
#' @docType data
#' @format A data frame with 16700 observations on the following 14 variables:
#' \describe{
#' \item{gpid}{GP identifier.}
#' \item{patid}{Patient identifier.}
#' \item{patage}{Patient age in years.}
#' \item{pagegrp}{Patient age group, with ages grouped into 7 categories (ordered factor with levels
#' \code{page<35}, \code{35<=page<45}, \code{45<=page<55}, \code{55<=page<65}, \code{65<=page<75},
#' \code{75<=page<85}, \code{85<=page}).}
#' \item{patsex}{Patient gender (factor with levels \code{female}, \code{male}).}
#' \item{patinsur}{Patient insurance indicator (factor with levels
#' \code{privateins} (privately insured), \code{publicins} (publically insured)).}
#' \item{patedu}{Patient education level (ordered factor with levels \code{none}
#' (no formal education), \code{primary} (primary education), \code{secondary}
#' (secondary and lower/middle vocational education), \code{higher} (higher vocational
#' and university education)).}
#' \item{diag}{Primary diagnosis resulting from care episodes
#' (factor with levels
#' \code{1} (symptoms/complaints neck),
#' \code{2} (symptoms/complaints back),
#' \code{3} (myalgia/fibrositis),
#' \code{4} (symptoms of multiple muscles),
#' \code{5} (disabilities related to the locomotive system),
#' \code{6} (impediments of the cervical spine),
#' \code{7} (arthrosis cervical spine),
#' \code{8} (lumbago),
#' \code{9} (ischialgia),
#' \code{10} (hernia nuclei pulposi),
#' \code{11} (impediments of the shoulder),
#' \code{12} (epicondylitis lateralis),
#' \code{13} (tendinitis/synovitis)).}
#' \item{gpexper}{GP experience (number of years working as a GP divided by ten).}
#' \item{gpworkload}{GP workload (number of contacts in the 3-month registration period divided by 1000).}
#' \item{practype}{Practice type (factor with levels \code{solo},
#' \code{duo}, \code{group}, \code{healthcentre}).}
#' \item{location}{Practice location (factor with levels \code{rural},
#' \code{suburban}, \code{urban}, \code{bigcity}).}
#' \item{gpphysifr}{Indicator of whether the GP has physiotherapists in their
#' social network (factor with levels \code{no}, \code{yes}).}
#' \item{referral}{Indicator of whether the patient was referred to a
#' physiotherapist (factor with levels \code{no}, \code{yes}).} }
#' @source Charlton, C., Rasbash, J., Browne, W.J., Healy, M. and Cameron, B. (2024)
#' \emph{MLwiN Version 3.08} Centre for Multilevel Modelling, University of
#' Bristol.
#' 
#' Leyland, A.H., Groenewegen, P.P. (2020). Multilevel Logistic Regression Using
#' MLwiN: Referrals to Physiotherapy. In: \emph{Multilevel Modelling for Public
#' Health and Health Services Research}. Springer, Cham. \doi{10.1007/978-3-030-34801-4_12}
#' 
#' Van der Velden, K. (1999). \emph{General practice at work: its contribution to
#' epidemiology and health policy}. NIVEL, PhD thesis Erasmus University, Utrecht
#' @keywords datasets
#' @examples
#'
#' \dontrun{
#' 
#' data(fysio, package = "R2MLwiN")
#' 
#' # Example taken from Leyland and Groenewegen (2020)
#' 
#' # Change contrasts if wish to avoid warning indicating that, by default,
#' # specified contrasts for ordered predictors will be ignored by runMLwiN
#' # (they will be fitted as "contr.treatment" regardless of this setting). To
#' # enable specified contrasts, set allowcontrast to TRUE (this will be the
#' # default in future package releases).
#' my_contrasts <- options("contrasts")$contrasts
#' options(contrasts = c(unordered = "contr.treatment",
#'                       ordered = "contr.treatment"))
#' 
#' # As an alternative to changing contrasts, can instead use C() to specify
#' # contrasts for ordered predictors in formula object, e.g.:
#'
#' # F1 <- logit(referral) ~ 1 + C(pagegrp, "contr.treatment") + patsex + diag +
#' #   C(patedu, "contr.treatment") + patinsur + gpexper + gpworkload +
#' #   practype + location + gpphysifr +
#' #   (1 | gpid)
#' # 
#' # (mod_MQL1 <- runMLwiN(Formula = F1,
#' #                       D = "Binomial",
#' #                       data = fysio,
#' #                       allowcontrast = TRUE))
#' 
#' F1 <- logit(referral) ~ 1 + pagegrp + patsex + diag +
#'   patedu + patinsur + gpexper + gpworkload +
#'   practype + location + gpphysifr +
#'   (1 | gpid)
#' 
#' (mod_MQL1 <- runMLwiN(Formula = F1,
#'                       D = "Binomial",
#'                       data = fysio))
#' 
#' (mod_PQL2 <- runMLwiN(Formula = F1,
#'                       estoptions = list(nonlinear = c(N = 1, M = 2),
#'                                         startval = list(FP.b = mod_MQL1@FP,
#'                                                         FP.v = mod_MQL1@FP.cov,
#'                                                         RP.b = mod_MQL1@RP,
#'                                                         RP.v = mod_MQL1@RP.cov)),
#'                       D = "Binomial",
#'                       data = fysio))
#'                       
#' # Change contrasts back to pre-existing:
#' options(contrasts = my_contrasts)
#' }
"fysio"