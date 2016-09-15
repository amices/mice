
# Main Script

# Load packages
require("MASS")
require("mice")
require("Matrix")
require("lattice")

# Create test data
source("SimulateData.R")

# Underlying functions
source("./Ampute/AmputeMCAR.R")
source("./Ampute/AmputeMARCont.R")
source("./Ampute/AmputeMARDisc.R")
source("./Ampute/AmputeDefault.R")
source("./Ampute/Is.R")
source("./Ampute/Print.R")
source("./Ampute/Summary.R")
source("./Ampute/Plot.R")
source("./Ampute/Mads.R")
source("./Ampute/MiceTheme.R")

# Main function
source("./Ampute/Ampute.R")

# Test Code
source("./Check/TestCode.R")

# Test Function
# source("Check/TestFunction.R")

# Run document TestErrors.R to check the defaults
# Not possible with source()



