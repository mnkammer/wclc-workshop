## Install Packages
## 

# List of required packages
required_packages <- c(
  "glmnet", "ROCR", "lmtest", "readxl", "rstatix", "ggpubr", "car", "Epi",
  "lme4", "lmerTest", "splines", "caret", "ggplot2", "rms", "gridExtra",
  "mice", "dplyr", "knitr", "kableExtra", "pROC", "RColorBrewer", "tidyverse", "vtable"
)

# Function to check and install missing packages
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Run the function for the required packages
check_and_install(required_packages)