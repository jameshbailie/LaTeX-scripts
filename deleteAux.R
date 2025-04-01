
deleteAuxFiles <- function(folder) {
  #Recursively delete .aux files within folder
  
  # Get all .aux files recursively
  aux_files <- list.files(path = folder, pattern = "\\.aux$", recursive = TRUE, full.names = TRUE)
  
  # Show what will be deleted
  cat("Found", length(aux_files), ".aux file(s):\n")
  cat(paste(aux_files, collapse = "\n"), "\n")
  
  choice <- utils::menu(
    choices = c("Yes, delete them", "No, cancel"),
    title = "Do you want to delete these .aux files?"
  )
  
  if (choice == 1) {
    # Delete each file
    for (file in aux_files) {
      cat("Deleting:", file, "\n")
      file.remove(file)
    }
  }
  
}