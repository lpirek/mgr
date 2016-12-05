#Skrypt wykorzystywany przez aplikacje GameTheoryPrediction

args <- commandArgs(trailingOnly = TRUE)
csvFile1 = args[1]
csvFile2 = args[2]

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


ShowPredictResult <- function(model) {
  predictValues <- predict.lm(model, df.predict)
  values <- sapply(predictValues, function(x) { if (x > 0.5) 1 else 0 })
  cat(values, "\n")
}

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
  extendModel = lm(
    formulaE,
    data = df.train)
  ## stepwise regression with AIC method (not p-value), alternative method : step
  extendModelS = stepAIC(extendModel, trace = FALSE)
  ## validate predictions:
  ShowPredictResult(extendModelS)
  cat("END", "\n")
}



