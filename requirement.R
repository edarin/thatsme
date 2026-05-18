packages <- c(
  "bookdown",
  "DT",
  "doParallel",
  "distill",
  "dplyr",
  "exactextractr",
  "ggplot2",
  "here",
  "kableExtra",
  "knitr",
  "plotly",
  "postcards",
  "RColorBrewer",
  "randomForest",
  "raster",
  "readxl",
  "rmarkdown",
  "scales",
  "sf",
  "stars",
  "tmap",
  "tidyverse",
  "units",
  "xaringan",
  "xaringanExtra"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}
