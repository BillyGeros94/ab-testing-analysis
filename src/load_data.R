# Load data
load_data <- function(file_path) {

    # Read the file
    ab_data <- read_csv(file_path)

    return(ab_data)
}
