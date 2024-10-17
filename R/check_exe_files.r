check_exe_files <- function(dest_folder="gempack") {

  # Check if the gempack folder is present. If it is not, create, copy
  # the tab files, and compile the exe files:
    if (!dir.exists(dest_folder)){
        copy_files_to_dir()
        ## compile_tab_files()
        ## Exit the function
        ##return()
        }

  # Get the path to the directory with the tablo files
  extdata_path <- system.file("tablo", package = "gtapshape")

  # Get all files with the specified extension
  files <- list.files(extdata_path, pattern = paste0("\\.tab$"), full.names = FALSE)

  # Store the current working directory
  original_dir <- getwd()

  # Change to the destination folder
  setwd(file.path(original_dir, dest_folder))

  # Create a vector of .exe file names
  exe_files <- gsub("\\.tab$", ".exe", files)

  # Check for existence of each .exe file
  existing_exe_files <- exe_files[file.exists(exe_files)]
  missing_exe_files <- exe_files[!file.exists(exe_files)]

  # Return to the original directory
  setwd(original_dir)

  # Print results
  if (length(existing_exe_files) > 0) {
    cat("The following required .exe files were found in", dest_folder, ":\n")
    cat(paste("- ", existing_exe_files), sep = "\n")
  }

  if (length(missing_exe_files) > 0) {
    cat("\nThe following required .exe files were not found in", dest_folder, ":\n")
    cat(paste("- ", missing_exe_files), sep = "\n")

    ## # Call the function to copy missing .tab files
    ## copied_files <- copy_missing_tab_files(dest_folder, missing_exe_files)

    ## # Update the lists of existing and missing files
    ## newly_copied_tab <- gsub("\\.exe$", ".tab", missing_exe_files[missing_exe_files %in% basename(copied_files)])
    ## cat("\nThe following .tab files were copied:\n")
    ## cat(paste("- ", newly_copied_tab), sep = "\n")
    ## cat("\nPlease run the appropriate command to generate .exe files from these .tab files.\n")
  }
  # Return a list of existing and missing .exe files
    return(list(existing = existing_exe_files, missing = missing_exe_files))
      # Return to the original directory
  setwd(original_dir)

}
