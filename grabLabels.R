#Grab labels from an .aux file so that they can be included in 
# another .tex file.
# The output should be pasted into the premable of the other .tex file enclosed within
#   \makeatletter
#   \protected@write\@auxout{}{
#   ...
#   }

# Define the function to filter lines starting with "\\newlabel"
filter_newlabel_lines <- function(input_filepath, output_filepath, label_prefix) {
  # Open the input file for reading
  input_file <- file(input_filepath, open = "r")
  
  # Open the output file for writing
  output_file <- file(output_filepath, open = "w")
  
  # Read the input file line-by-line
  while (TRUE) {
    # Read the next line
    line <- readLines(input_file, n = 1, warn = FALSE)
    
    # Break the loop if the end of the file is reached
    if (length(line) == 0) {
      break
    }
    
    # Check if the line starts with "\\newlabel"
    if (startsWith(line, "\\newlabel")) {
      # Write the line to the output file
      writeLines(paste0("\\string\\newlabel{", label_prefix, substring(line, 11)), output_file)
    }
  }
  
  # Close both files
  close(input_file)
  close(output_file)
}


setwd("/Users/james/Documents/Overleaf/DPSwapping")
input_filepath <- "bond1toHDSRReviewers.aux"
output_filepath <- "bond1labels.txt"
label_prefix <- "I-"
filter_newlabel_lines(input_filepath, output_filepath, label_prefix)

setwd("/Users/james/Documents/Overleaf/A Refreshment Stirred, Not Shaken (II)/hdsr-submission-15Jan25/latex")
input_filepath <- "main.aux"
output_filepath <- "bond2labels.txt"
label_prefix <- "II-"
filter_newlabel_lines(input_filepath, output_filepath, label_prefix)
