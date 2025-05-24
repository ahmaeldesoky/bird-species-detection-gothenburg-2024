#________________________________________________________
# Load necessary libraries ----
#________________________________________________________

library(openxlsx)
library(progress)
library(stringr)
library(fs)
library(dplyr)
library(gtools)

#________________________________________________________
# Function to retrieve and sample audio files for expert validation ----
#________________________________________________________

#' Retrieve and sample audio files per species for manual expert validation
#'
#' This function takes a formatted BirdNET results dataframe object and performs
#' the following tasks:
#' 
#' 1. Filters the dataset based on confidence level and selected species (optional).
#' 2. Filters out audio files with human vocalizations for privacy.
#' 3. Samples a specified number of audio files for each species.
#' 4. Copies the sampled audio files into designated directories.
#' 5. Creates spreadsheets for each species containing columns for manual validation.
#'
#' @param input_dataframe A dataframe object containing the output of the BirdNET analysis (formatted_birdnet_results). 
#'                        It must include 4 columns "filepath", "scientific_name", "recordingID", and "confidence".
#'                        The "filepath" column should contain the paths to audio files, "scientific_name" should
#'                        contain species scientific name, and "recordingID" should have unique identifiers for recordings
#'                        with ".wav" extension.
#' @param output_directory A string indicating the directory where sampled audio files and validation spreadsheets 
#'                         will be stored.
#' @param confidence_threshold A numeric value specifying the minimum confidence level required to sample audio files.
#' @param sample_size A numeric value specifying the number of audio files to be sampled for each species.
#' @param species_list An optional character vector specifying the species to sample. If NULL, all species in the
#'                     dataset will be used.
#' @param seed An optional numeric value to set the random seed for reproducibility.
#' @param prioritize_high_confidence A logical value indicating whether to prioritize higher confidence samples for 
#'                                   selection. The default is `FALSE`.
#  
#' @return This function performs file operations and creates validation spreadsheets. It does not return any value.
#'
#' @examples
#' retrieve_sample_audio(input_dataframe = formatted_birdnet_results, 
#'                       output_directory = "path/to/output", 
#'                       confidence_threshold = 0.85, 
#'                       sample_size = 50)


retrieve_sample_audio <- function(input_dataframe, output_directory, confidence_threshold, 
                                  sample_size, species = NULL, seed = 123, 
                                  prioritize_high_confidence = FALSE){
        
        #_________________________________________________________
        # Validate input arguments ----
        #_________________________________________________________
        
        if (!is.data.frame(input_dataframe)) stop("Input 'dataframe' must be a valid dataframe.")
        if (!all(c("filepath", "scientific_name", "recordingID", "confidence") %in% colnames(input_dataframe)))
                stop("Dataframe must contain 'filepath', 'scientific_name', 'recordingID', and 'confidence' columns.")
        if (!is.numeric(confidence_threshold) || confidence_threshold <= 0) stop("Input 'confidence_threshold' must be a non-negative numeric value.")
        if (!is.numeric(sample_size) || sample_size < 1) stop("Input 'sample_size' must be at least 1.")
        if (!dir.exists(output_directory)) stop("The specified 'output_directory' does not exist.")
        
        # Handle optional species_list
        if (is.null(species)) {
                species <- unique(input_dataframe$scientific_name)
                message("Species list not provided. Using all species in the dataframe.")
        } else{
                if (!is.character(species) || length(species) == 0 || !any(species %in% input_dataframe$scientific_name)) stop("Input 'species_list' must be a non-empty character vector containing species present in the 'scientific_name' column.")
                
        }
        
        #_________________________________________________________
        # Filter out audio files with human vocalizations
        #_________________________________________________________
        
        audio_files_with_human_vocals <- input_dataframe %>%
                filter(scientific_name %in% c("Human vocal", "Human non-vocal", "Human whistle")) %>%
                pull(filepath) %>%
                unique()
        
        message(length(unique(audio_files_with_human_vocals)), " audio files with human vocals were found.")

        # Remove audio files with human vocals from the sampling dataframe and filter based on the entered confidence threshold and species list
        df_for_sampling_audio_files <- input_dataframe %>%
                filter(!filepath %in% audio_files_with_human_vocals &
                               confidence >= confidence_threshold &
                               scientific_name %in% species
                )

        #_________________________________________________________
        # Check if there are any records remaining after filtering
        #_________________________________________________________
        
        if(nrow(df_for_sampling_audio_files) == 0){
                stop("No records found for any of the specified species at the selected confidence threshold. Please check your parameters.")
        }
        
        #_________________________________________________________
        # Split the sampling dataframe based on the species and remove duplicate audio files in each dataframe
        #_________________________________________________________
        
        list_dfs_per_species_for_sampling <- split(df_for_sampling_audio_files, df_for_sampling_audio_files$scientific_name)
        
        # If prioritizing high confidence, arrange by confidence and remove duplicates with lower confidence
        if(prioritize_high_confidence){
                list_dfs_per_species_for_sampling <- lapply(list_dfs_per_species_for_sampling, function(df){
                        df %>%
                                arrange(desc(confidence)) %>%
                                distinct(filepath, .keep_all = TRUE)
                }
                ) 
        } else {
                # If not prioritizing high confidence
                list_dfs_per_species_for_sampling <- lapply(list_dfs_per_species_for_sampling, function(df){
                        distinct(df, filepath, .keep_all = TRUE)
                }
                )      
        }
        
        #_________________________________________________________
        # Sample recordings for each species
        #_________________________________________________________
        
        set.seed(seed)  # Set seed for reproducibility
        list_sampled_dfs_per_species <- lapply(list_dfs_per_species_for_sampling, function(df){
                if(nrow(df) >= sample_size){
                        if(prioritize_high_confidence){
                                # Weighted sampling based on the "confidence" column
                                df %>%
                                        arrange(desc(confidence)) %>%
                                        slice_head(n = sample_size)
                        } else {
                                # Simple random sampling
                                slice_sample(df, n = sample_size)
                        }
                } else {
                        # If there are fewer records than the "sample_size", return the whole dataframe
                        df
                }
        })
        
        #_________________________________________________________
        # Create directories for output ----
        #_________________________________________________________
        
        for (species in names(list_sampled_dfs_per_species)) {
                dir_create(file.path(output_directory, species))
        }
        
        #_________________________________________________________
        # Loop over each species dataframe and copy sampled audio files to the output directory ----
        #_________________________________________________________
        
        # Initialize a progress bar object
        pb <- progress_bar$new(
                format = "  Copying audio files [:bar] :percent ends in :eta",
                total = sum(sapply(list_sampled_dfs_per_species, nrow)), # Total number of files to process
                width = 60
        )
        
        for (species in names(list_sampled_dfs_per_species)) {
                species_df <- list_sampled_dfs_per_species[[species]]
                
                # Loop over each row in the species-specific dataframe
                for (i in 1:nrow(species_df)) {
                        
                        # Get the source file path
                        origin_path <- species_df$filepath[i]
                        
                        # Construct the output file path
                        destination_path <- file.path(output_directory, species, species_df$recordingID[i])
                        
                        # Copy the file to the new location
                        file_copy(path = origin_path, new_path = destination_path, overwrite = TRUE)
                        
                        # Update the progress bar
                        pb$tick() # The progress bar is displayed after the first tick command
                        
                }
        }
        
        message("File copying completed successfully at ", Sys.time())
        
        #_________________________________________________________
        # Create validation spreadsheets ----
        #_________________________________________________________
        
        message("Creating validation spreadsheets.")
        
        for (species in names(list_sampled_dfs_per_species)) {
                species_df <- list_sampled_dfs_per_species[[species]]
                
                # Create a dataframe template for validation
                validation_sheet <- data.frame(
                        recordingID = str_sort(species_df$recordingID, numeric = TRUE), # Sort naturally
                        correct = NA_real_, 
                        incorrect = NA_real_, 
                        unsure = NA_real_,
                        expertLabel = NA_character_, 
                        notes = NA_character_,
                        stringsAsFactors = FALSE
                )
                
                # Construct the output file path for saving the spreadsheets
                path_xlsx <- file.path(output_directory, species, 
                                       paste0(species, ".xlsx") # sheet name
                                       )
                
                # Create the spreadsheet in the designated output folder
                write.xlsx(validation_sheet, file = path_xlsx, overwrite = TRUE)
        }
        
        message("Validation spreadsheets created successfully at ", Sys.time())
}

