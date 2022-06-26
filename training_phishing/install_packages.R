packages <- c("tidyverse","readxl","openxlsx","janitor","svDialogs")
package_check <- lapply(
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
