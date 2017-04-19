



force.load.packages <- function(package.names) {
  installed <- rownames(installed.packages())
  need.install <- package.names[!package.names %in% installed]
  install.packages(need.install)
  sapply(package.names, require, quietly = FALSE, character.only = TRUE)
}