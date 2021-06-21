# 7 January 2018
# http://therbootcamp.github.io

# Script to install any packages necessary for the BaselRBootcamp

Bootcamp_package_installation <- function() {

  installed_packages <- rownames(installed.packages())

  packages.to.install <- c("devtools", "tidyverse", "yarrr", "afex",
                           "car", "markdown", "rmarkdown", "FFTrees",
                           "rpart", "randomForest", "e1071", "speff2trial",
                           "shiny", "shinyjs", "caret", "mlr", "parallel", "Rcpp",
                           "BayesFactor", "parfm", "lava", "rvest", "diagram", "ggjoy",
                           "rmdformats", "ggRandomForests", "party")

  for(package.i in sort(packages.to.install)) {

    if((package.i %in% installed_packages) == FALSE) {

      if(package.i == "Rcpp"){
        test <- try(install.packages(package.i))
        if(is.null(test)){
          message(paste("Installing", package.i, "..."))
        } else {
        message(paste("Error: could not install package Rcpp"))
        }
      } else {
        install.packages(package.i)
        message(paste("Installing", package.i, "..."))
      }

    } else {
    message(paste(package.i, "already installed!"))
    }


  }
  
  devtools_packages_to_install <- c("kassambara/ggcorrplot")
  
    for(package.i in sort(devtools_packages_to_install)) {

    if((package.i %in% installed_packages) == FALSE) {

        devtools::install_github(package.i)
        message(paste("Installing", package.i, "..."))
      }  else {
    message(paste(package.i, "already installed!"))
      }
    }


  }
  
  

Bootcamp_package_installation()

