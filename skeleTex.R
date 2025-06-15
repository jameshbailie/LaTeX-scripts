# Things to add, possibly:
# - Keep section labels
# - At the moment, \begin{document}, \end{document} and \appendix commands are
#   assumed to be on their own line, without any other text

#' Filter a LaTeX .tex file to retain only structural sectioning commands within the document body
#'
#' This function reads a LaTeX .tex file, and deletes all content between 
#' \begin{document} and \end{document}, except for structural commands such as 
#' \section{}, \subsection{}, \subsubsection{}, \paragraph{}, and \chapter{}, 
#' including variants with optional arguments (e.g., \section[short]{long}).
#' 
#' Everything before \begin{document} and after \end{document} is preserved unchanged.
#'
#' @param input_file Path to the input .tex file
#' @param output_file Path to the output .tex file where filtered content will be saved
#'
#' @return No return value; writes filtered LaTeX content to output_file


filter_tex_file <- function(input_file, output_file) {
  # Read all lines from the input file
  lines <- readLines(input_file, warn = FALSE)
  
  # Locate the positions of \begin{document} and \end{document}
  begin_doc <- grep("\\\\begin\\{document\\}", lines)
  end_doc <- grep("\\\\end\\{document\\}", lines)
  
  if (length(begin_doc) == 0 || length(end_doc) == 0 || begin_doc >= end_doc) {
    stop("Could not identify valid \\begin{document} and \\end{document} markers.")
  }
  
  # Regex pattern to match sectioning commands with or without optional arguments
  #keep_pattern <- "^\\s*\\\\(section|subsection|subsubsection|paragraph|chapter)(\\[[^\\]]*\\])?\\{[^\\}]*\\}"
  #keep_pattern <- "\\\\(section|subsection|subsubsection|paragraph|chapter)(\\[[^\\]]*\\])?\\{[^}]*\\}"
  keep_pattern <- "\\\\(section|subsection|subsubsection|paragraph|chapter)"
  
  # Extract three parts: pre-document, main content, post-document
  before <- lines[1:begin_doc]
  middle <- lines[(begin_doc + 1):(end_doc - 1)]
  after <- lines[end_doc:length(lines)]
  
  # Filter only the lines matching structural commands
  # middle_filtered <- grep(keep_pattern, middle, value = TRUE)
  
  # Track comment environment state
  in_comment_block <- 0
  
  # Function to extract matches with original indentation
  extract_with_indent <- function(line) {
    
    # Get leading whitespace from original line
    indent <- sub("^([ \t]*).*", "\\1", line)
    
    # Remove anything after unescaped % (skip commented-out commands)
    line <- sub("(?<!\\\\)%.*$", "", line, perl = TRUE)
    
    # Look for \appendix command and print if found:
    matches <- gregexpr("\\\\appendix", line, perl = TRUE)[[1]]
    if (matches[1] != -1) return(c(paste0(indent, "\\appendix"), paste0(indent, "")))
    
    #This is not necessary, but to avoid running the rest of this function,
    # exit now if the line is only whitespace:
    if (trimws(line) == "") return(NULL)
    
    #Recursively remove comment blocks:
    remove_comment_blocks <- function(line) {
      # Track position of comment boundaries in this line
      start_comment <- gregexpr("\\\\begin\\{comment\\}", line, perl = TRUE)[[1]]
      end_comment <- gregexpr("\\\\end\\{comment\\}", line, perl = TRUE)[[1]]
      
      if ((in_comment_block > 0) & (end_comment[1] != -1)) {
        #Remove everything before the first end comment:
        line <- substr(line, 
                       end_comment[1] + attr(end_comment, "match.length")[1], 
                       nchar(line))
        in_comment_block <<- in_comment_block - 1
        return(remove_comment_blocks(line))
      }
      else if (in_comment_block > 0) {
        #Remove line:
        return("")
      }
      else if ((start_comment[1] != -1) & (end_comment[1] == -1)) {
        #Remove everything after begin comment:
        line <- substr(line, 1, start_comment[1]-1)
        in_comment_block <<- in_comment_block + length(start_comment)
        return(line)
      }
      else if ((start_comment[1] != -1) & (end_comment[1] != -1)) {
        #Find the nearest start comment before the first end comment, and remove this part
        if (!any(start_comment < end_comment[1])) {
          stop("End comment not paired with begin comment")
        }
        
        lineStart <- substr(line, 1, max(start_comment[which(start_comment < end_comment[1])]))
        lineEnd <- substr(line, 
                          end_comment[1] + attr(end_comment, "match.length")[1], 
                          nchar(line))
        line <- paste0(lineStart, lineEnd)
        return(remove_comment_blocks(line))
      } 
      else {
        return(line)
      }
    }
    line <- remove_comment_blocks(line)
    
    #Look for any matches to \section or other similar commands:
    matches <- gregexpr(keep_pattern, line, perl = TRUE)[[1]]
    if (matches[1] == -1) return(NULL)

    #Find the ends of the commands
    indices_after_cmds <- as.vector(matches + attr(matches, "match.length"))
    
    #Include star versions of the commands:
    indices_after_cmds <- indices_after_cmds + 
      (substring(line, indices_after_cmds, indices_after_cmds) == "*")
    
    #Filter out any commands which have the commands we actually want as prefixes:
    starts_with_bracket <- substring(line, indices_after_cmds, indices_after_cmds) == "["
    starts_with_brace <- substring(line, indices_after_cmds, indices_after_cmds) == "{"
    valid_cmds <- which(starts_with_bracket | starts_with_brace)
    
    matches <- matches[valid_cmds]
    indices_after_cmds <- indices_after_cmds[valid_cmds]
    
    get_end_of_brace_block <- function(s, start, open_brace, close_brace) {
      depth <- rep(0, length(start))
      i <- start
      end <- rep(-1, length(start))
      
      while (min(i) <= nchar(s)) {
        ch <- substring(s, i, i)
        depth <- depth + (ch == open_brace) - (ch == close_brace)
        end <- (depth == 0)*(ch == close_brace)*(end == -1)*i-1
        i <- i + 1
      }
      
      end <- (end+2)*(substring(s, start, start) == open_brace) +
        start*(substring(s, start, start) != open_brace)
      
      if (any(!((depth == 0) | (start == end)))) stop("Section brace { or [ not closed")
      
      return(end)
    }
    
    indices_after_cmds <- get_end_of_brace_block(line, indices_after_cmds, "[", "]")
    indices_after_cmds <- get_end_of_brace_block(line, indices_after_cmds, "{", "}")
    
    matched_cmds <- substring(line, matches, indices_after_cmds-1)
    
    # For each match: keep indent, add two blank indented lines
    unlist(lapply(matched_cmds, function(cmd) {
      c(paste0(indent, cmd), paste0(indent, ""), paste0(indent, ""))
    }))
  }
  
  # Apply extraction and flatten
  middle_filtered <- unlist(lapply(middle, extract_with_indent), use.names = FALSE)
  
  
  # Combine parts back together
  new_content <- c(before, middle_filtered, after)
  
  # Write the cleaned content to the output file
  writeLines(new_content, con = output_file)
  
  cat("Filtered content written to", output_file, "\n")
}

# Example usage:
# filter_tex_file("example_input.tex", "example_output.tex")
# filter_tex_file("test.tex", "test_output.tex")

# setwd("C:\\Users\\User\\Documents\\Overleaf\\DPSamplevsPop")
# filter_tex_file("mainRemoved.tex", "test_output.tex")
