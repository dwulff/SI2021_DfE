# 7 January 2018
# http://therbootcamp.github.io

# Script to make sure packages for the BaselRBootcamp are correctly loaded:


installed_packages <- rownames(installed.packages())

packages.to.install <- c("devtools", "tidyverse", "yarrr", "afex",
                         "car", "markdown", "rmarkdown", "FFTrees",
                         "rpart", "randomForest", "e1071", "speff2trial",
                         "shiny", "shinyjs", "caret", "mlr", "parallel", "Rcpp",
                         "BayesFactor", "parfm", "lava", "rvest", "diagram", "ggjoy",
                         "rmdformats", "ggRandomForests", "party")

for(package.i in sort(packages.to.install)) {
  
  if((package.i %in% installed_packages) == FALSE) {
    
    message(paste(package.i, "is missing..."))
    
  } else {
    message(paste(package.i, "is installed!"))
  }
  
}
