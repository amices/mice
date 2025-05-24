# Adapted from mlbook2_prepare_data.r
# Snijders and Bosker 2012
# This script reads the original data set, does minimal transformations,
# and saves to /data
# Note: this file preserves all information

# read data
library(haven)
library(dplyr)
project <- path.expand("~/Package/mice/multilevel/Snijders")
fn <- file.path(project, "MLbook_2nded_total_4106-99.sav")
mlbook2 <- read_sav(fn)

# define function for transforming missing value codes to NA
makemis <- function(a) {
  return(ifelse(a == -99, NA, a))
}

# Any missings recognizable in this coding?
count.na <- function(x) {
  sum(is.na(x))
}
sapply(mlbook2, count.na)

# What are the variables?
names(mlbook2)

# Transform missing value codes to NA
mlbook2$langPRET <- makemis(mlbook2$langPRET)
mlbook2$langPOST <- makemis(mlbook2$langPOST)
mlbook2$ses <- makemis(mlbook2$ses)
mlbook2$IQ_verb <- makemis(mlbook2$IQ_verb)
mlbook2$IQ_perf <- makemis(mlbook2$IQ_perf)
mlbook2$mixedgra <- makemis(mlbook2$mixedgra)
mlbook2$Minority <- makemis(mlbook2$Minority)
mlbook2$sex <- makemis(mlbook2$sex)
mlbook2$repeatgr <- makemis(mlbook2$repeatgr)
mlbook2$aritPRET <- makemis(mlbook2$aritPRET)
mlbook2$aritPOST <- makemis(mlbook2$aritPOST)
mlbook2$meetings <- makemis(mlbook2$meetings)
mlbook2$currmeet <- makemis(mlbook2$currmeet)
mlbook2$homework <- makemis(mlbook2$homework)
mlbook2$percmino <- makemis(mlbook2$percmino)
mlbook2$satiprin <- makemis(mlbook2$satiprin)
mlbook2$natitest <- makemis(mlbook2$natitest)
mlbook2$aritdiff <- makemis(mlbook2$aritdiff)
mlbook2$classsiz <- makemis(mlbook2$classsiz)
mlbook2$schoolSES <- makemis(mlbook2$schoolSES)
mlbook2$denomina <- makemis(mlbook2$denomina) # leave as missing

# Do not drop any schools
# Do not add school averages (because of missing data)

# How many missings are there now in mlbook2?
apply(mlbook2, 2, count.na)

schools <- transmute(mlbook2,
  sch = as.integer(schoolnr),
  pup = as.integer(pupilNR_new),
  iqv = as.vector(scale(IQ_verb, scale = FALSE)),
  iqp = as.vector(scale(IQ_perf, scale = FALSE)),
  sex = as.integer(sex),
  ses = as.vector(scale(ses, scale = FALSE)),
  min = as.integer(Minority),
  rpg = as.integer(repeatgr),
  lpr = langPRET,
  lpo = langPOST,
  apr = aritPRET,
  apo = aritPOST,
  den = as.integer(denomina),
  ssi = schoolSES
)

brandsma <- as.data.frame(schools)

devtools::use_data(brandsma, overwrite = TRUE)
