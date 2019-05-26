.onAttach <- function(...) {
  if (is.null(getOption("MLwiN_path"))) {
    if (Sys.info()["sysname"] == "Linux") {
      options(MLwiN_path = "/usr/bin/mlnscript")
    }
    if (Sys.info()["sysname"] == "Darwin") {
      options(MLwiN_path = "/opt/mln/mlnscript")
    }
    if (Sys.info()["sysname"] == "FreeBSD") {
      options(MLwiN_path = "/usr/local/bin/mlnscript")
    }
    if (Sys.info()["sysname"] == "Windows") {
      options(MLwiN_path = "C:/Program Files/MLwiN v3.03/")
    }
  }
  packageStartupMessage("")
  packageStartupMessage("R2MLwiN: A package to run models implemented in MLwiN from R")
  packageStartupMessage("Copyright 2013-2017 Zhengzheng Zhang, Christopher M. J. Charlton, Richard M. A. Parker, William J. Browne and George Leckie")
  packageStartupMessage("")
  packageStartupMessage("Support provided by the Economic and Social Research Council (ESRC)")
  packageStartupMessage("(Grants RES-149-25-1084, RES-576-25-0032 and ES/K007246/1)")
  packageStartupMessage(paste(format(citation(package="R2MLwiN")), collapse="\n"))
  packageStartupMessage(paste0("The MLwiN_path option is currently set to ", getOption("MLwiN_path")))
  packageStartupMessage("To change this use: options(MLwiN_path=\"<path to MLwiN>\")")
} 
