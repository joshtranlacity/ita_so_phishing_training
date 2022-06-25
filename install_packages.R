source("utils.R")

packages <- utils.packages_vector()
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
)

tryCatch({
  tinytex::tlmgr_version()
}, error = function(e) {
  # https://yihui.org/tinytex/
  cat("Could not find TinyTeX, installing now...")
  tinytex::install_tinytex()
}
)

# needed if running with Rscript through command line
# install.packages("installr")
# installr::install.pandoc() 
