# Function to replace month names with numbers in a bib file
library(gsubfn)

replace_months <- function(input_filepath, output_filepath) {
  # Define a named vector mapping month names to numbers
  
  # Open the input file for reading
  input_file <- file(input_filepath, open = "r")
  
  # Open the output file for writing
  output_file <- file(output_filepath, open = "w")
  
  input_lines <- readLines(input_file, warn = FALSE)
  
  month_map <- c(
    "jan" = 1, "january" = 1,
    "feb" = 2, "february" = 2,
    "mar" = 3, "march" = 3,
    "apr" = 4, "april" = 4,
    "may" = 5,
    "jun" = 6, "june" = 6,
    "jul" = 7, "july" = 7,
    "aug" = 8, "august" = 8,
    "sep" = 9, "september" = 9,
    "oct" = 10, "october" = 10,
    "nov" = 11, "november" = 11,
    "dec" = 12, "december" = 12
  )
  
  #replace line "month = {jan}," (or similar) with "month = {1},"
  modified_lines <- gsubfn(pattern = "month\\s*=\\s*\\{\\s*(jan|january|feb|february|mar|march|apr|april|may|jun|june|jul|july|aug|august|sep|september|oct|october|nov|november|dec|december)\\s*\\}\\s*,\\s*", 
                           replacement = function(m) paste0("month = {", month_map[tolower(gsub("month\\s*=\\s*\\{\\s*|\\s*\\}\\s*,\\s*", "", m))], "},"),
                           x = input_lines, 
                           ignore.case = TRUE)
  # Some explanation: \\s* matches zero or more whitespace, a|b matches a or b (so in the replacement gsub (line 32) we are replacing "month = " or "," with "")
  
  #replace mine "month = jan," with "month = {1},"
  modified_lines <- gsubfn(pattern = "month\\s*=\\s*(jan|january|feb|february|mar|march|apr|april|may|jun|june|jul|july|aug|august|sep|september|oct|october|nov|november|dec|december),", 
                           replacement = function(m) paste0("month = {", month_map[tolower(gsub("month\\s*=\\s*|\\s*,\\s*", "", m))], "},"),
                           x = modified_lines, 
                           ignore.case = TRUE)
  
  # Write the modified content to a new file
  writeLines(modified_lines, output_file)
  
  # Close both files
  close(input_file)
  close(output_file)
}


#Testing:
setwd("/Users/james/Documents/git/LaTeX-scripts")
input_file <- "test.bib"
output_file <- "output.bib"
replace_months(input_file, output_file)




