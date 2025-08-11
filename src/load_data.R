
# Install and load packages
packages <- c("tidyverse")

install_and_load <- function(pkg_list = packages) {
     for (pkg in pkg_list) {
         if (!require(pkg, character.only = TRUE)) {
         install.packages(pkg, dependencies = TRUE)
         library(pkg, character.only = TRUE)
         }
     }
}

# Load data
load_data <- function(file_path) {

    # Read the file
    ab_data <- read_csv(file_path)
  
    return(ab_data)
} 