library(stringr)

replaceCitations <- function(folder, mapping_file, verbose = T) {
  
  # The mapping_file consists of lines of the form "citeKeyX : citeKeyY"
  # What does this function do?
  # For all files in the folder (recursively) replace any occurence of 
  # "citeKeyX" with "citeKeyY", provided that the occurence is within a \cite, 
  # \citep, or \citet command.
 
  
  
  # --- Step 1: Read citation mapping file ---
  # Format: citeKeyX : citeKeyY
  mapping_lines <- readLines(mapping_file, warn = F)
  mapping_lines <- mapping_lines[!grepl("^\\s*#", mapping_lines)]
  mapping <- str_split_fixed(mapping_lines, "\\s*:\\s*", 2)
  colnames(mapping) <- c("from", "to")
  
  # --- Step 2: Find all .tex files recursively ---
  tex_files <- list.files(folder, pattern = "\\.tex$", recursive = TRUE, full.names = TRUE)
  
  # --- Step 3: Process each file ---
  for (file in tex_files) {
    cat("Processing:", file, "\n")
    lines <- readLines(file, warn = F)
    
    linesOriginal <- lines
    
    # For each mapping line
    for (i in seq_len(nrow(mapping))) {
      from <- mapping[i, "from"]
      to <- mapping[i, "to"]
      
      # Regex pattern: \cite{...}, \citep{...}, \citet{...}
      # Replace citeKeyX ONLY when it appears within these commands
      pattern <- paste0("(\\\\cite[tp]?\\{[^}]*)\\b", from, "\\b([^}]*\\})")
      replacement <- paste0("\\1", to, "\\2")
      
      lines <- str_replace_all(lines, pattern, replacement)
      
      # Regex pattern: \cite[tp]?[...]{...} 
      #We must assume that there is only one key in \cite[...]{...}
      noViolation <- checkAssumption(lines, keyWord = from)
      if (noViolation) {
        pattern <- paste0("(\\\\cite[tp]?)(\\[[^\\]]*\\])\\{", from, "\\}")
        replacement <- paste0("\\1\\2{", to, "}")
      
        lines <- str_replace_all(lines, pattern, replacement)
      }
    }
    
    if (verbose) {
      for (i in seq_len(length(lines))) {
        if (lines[i] != linesOriginal[i]) {
          cat(paste0("Replaced line:\n", linesOriginal[i], "\n", "With this line:\n", lines[i], "\n"))
        }
      }
    }
    
    # Write back to file (overwrites original)
    writeLines(lines, file)
  }
  
  cat("All files processed.\n") 
  
}

checkAssumption <- function(lines, keyWord) {
  pattern <- "\\\\cite[tp]?(\\[[^\\]]*\\])+\\{([^}]+)\\}"
  
  # Extract matches and check
  bad_lines <- vector()
  for (i in seq_along(lines)) {
    matches <- str_match_all(lines[i], pattern)[[1]]
    if (nrow(matches) > 0) {
      for (j in 1:nrow(matches)) {
        cited_keys <- matches[j, 3]
        num_keys <- length(strsplit(cited_keys, ",\\s*")[[1]])
        if ((num_keys > 1) & grepl(keyWord, lines[i])) {
          bad_lines <- c(bad_lines, paste0("Line ", i, ": ", lines[i]))
        }
      }
    }
  }
  
  # Show results
  if (length(bad_lines) != 0) {
    cat("Lines that violate assumption:")
    cat(paste0(bad_lines, collapse = "\n"), "\n")
    warning("Violating assumption that \\cite[...]{...} only has one key")
  }
  
  return((length(bad_lines) == 0))
}

#Usage
if (FALSE) {
  replaceCitations(folder = "/Users/james/My Drive/PhD/Dissertation Thesis/Thesis",
                   mapping_file = "/Users/james/My Drive/PhD/Dissertation Thesis/Thesis/citeChange.txt")
}