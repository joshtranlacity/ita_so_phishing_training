packages <- c("tidyverse", "readxl", "janitor","ggplot2","tinytex","writexl","tidyr","installr")
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

# installr::install.pandoc()
