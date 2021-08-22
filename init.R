libraries_to_install <- c("shiny",
                          "pacman",
                          "tidyverse",
                          "anytime",
                          "lubridate",
                          "zoo",
                          "stringr",
                          "tidytext",
                          "shinythemes",
                          "shinyWidgets",
                          "zoo")
install_if_missing = function(p) {
    if (p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p)
    }
}
invisible(sapply(libraries_to_install, install_if_missing))