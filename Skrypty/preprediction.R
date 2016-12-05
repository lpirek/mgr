args <- commandArgs(trailingOnly = TRUE)

#Sciezka do zbioru uczacego
csvFile1 = "C:\\mgr\\1.0\\bugs.csv"    
#Sciezka do zbioru na ktorym ma zostac dokonana predykcja
csvFile2 = "C:\\mgr\\1.0\\bugs_service.csv"

## configuration
config.separator <- ';'
config.fileSep <- .Platform$file.sep
config.packages <- c('MASS', 'SDMTools') # needed CRAN libraries

# Custom functions
RequirePackages <- function(packages) {
  # Install and load all necessary packages
  # Args:
  #   packages: packages to load or install & load
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())));
  }
  for(package in packages) {
    print(paste('Try to load library:', package));
    library(package, character.only = TRUE);
  }
}


## Load all necessary packages
RequirePackages(config.packages);

# metryki, ktore wchodza w sklad kazdego modelu
config.basicMetrics <- c("WMC", "DIT", "NOC", "CBO", "RFC", "LCOM", "CA",
                         "CE", "NPM", "LCOM3", "LOC", "DAM", "MOA", "MFA", "CAM", "IC", "CBM", "AMC")

# metryki, ktore dolaczane do powyzszych tworza nowe modele
config.extraMetrics <- c(
  "Number.of.Revisions",
  "Number.of.Commiters", 
  "Number.of.Modified.Lines",
  "Number.of.Revisions + Number.of.Commiters",
  "Number.of.Revisions + Number.of.Modified.Lines",
  "Number.of.Revisions + Number.of.Commiters + Number.of.Modified.Lines")


# Custom functions
RequirePackages <- function(packages) {
  # Install and load all necessary packages
  # Args:
  #   packages: packages to load or install & load
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())));
  }
  for(package in packages) {
    print(paste('Try to load library:', package));
    library(package, character.only = TRUE);
  }
}

CreateConfusionMatrix <- function(model) {
  predictedValues <- predict.lm(model, df.predict)
  realValues <- df.predict$Number.of.Bugs
  # binarization
  realValues <- sapply(realValues, function(x) { ifelse(x >= 1, 1, 0) })
  confusion.matrix(realValues, predictedValues, threshold = 0.5)
}


ShowPredictResult <- function(model) {
  predictedValues <- predict.lm(model, df.predict)
  values <- sapply(predictedValues, function(x) { ifelse(x > 0.5, 1, 0) })
  cat(values, "\n")
}

## Load all necessary packages
RequirePackages(config.packages);

## Load csv to data frame
# training set
df.train = read.csv(csvFile1, sep = config.separator);
# validation set
df.predict = read.csv(csvFile2, sep = config.separator);

## Build extended models
for (metric in config.extraMetrics) {
  ## Build extend model
  formulaE <- as.formula(paste("Number.of.Bugs ~ ", paste(c(config.basicMetrics, metric), collapse = "+")))
  cat("MODEL: ", paste("Number.of.Bugs ~ ", paste(c(config.basicMetrics, metric), collapse = " + ")), "\n");
  print(formulaE)
  extendModel = lm(
    formulaE,
    data = df.train)
  ## stepwise regression with AIC method (not p-value), alternative method : step
  extendModelS = stepAIC(extendModel, trace = FALSE)
  ## validate predictions:
  # summary(predict.lm(model, dataframe to validate))
  print(CreateConfusionMatrix(extendModelS))
  ShowPredictResult(extendModelS)
  cat("END", "\n")
}
