#________________________________________________________
# Script Overview ----
#________________________________________________________

# This script analyzes raw audio files using the BirdNET classifier (Kahl et al., 2021)
# and formats the output with "recordingID", "siteID", and "datetime" columns for
# easier data manipulation.

# It relies on paths specified in the "config.yaml" file and writes results in
# the "BirdNET_raw_results/" directory.

# Before running this script ensure:
# 1. The "config.yaml" file is correctly configured with accurate paths.
# 2. The directory structure follows the expected structure (see the README file).

#________________________________________________________
# Load Required Libraries ----
#________________________________________________________

library(yaml)
library(fs)
library(readr)
library(dplyr)
library(NSNSDAcoustics)
library(stringr)
library(lubridate)

#________________________________________________________
# Load the Configuration File "config.yaml" ----
#________________________________________________________

# Load the configuration file
config <- read_yaml("config.yaml")

# Extract paths and strings from the configuration file
birdnet_version <- config$birdnet$birdnet_version
birdnet_executable <- config$birdnet$birdnet_executable  # Absolute path to BirdNET-Analyzer.exe installation
raw_audio <- config$raw_audio                            # Path to raw audio files for BirdNET analysis
BirdNET_raw_results <- config$BirdNET_raw_results        # Path to processed audio results
path_to_species_list <- config$path_to_species_list      # Path to species list file

#________________________________________________________
# Running the BirdNET Classification ----
#________________________________________________________

?birdnet_analyzer
birdnet_analyzer(
        birdnet.version = birdnet_version,  # Version of installed BirdNET from config
        birdnet.path = birdnet_executable,  # BirdNET executable path from config
        i.audio = raw_audio,                # Path to input folder from config
        o.results = BirdNET_raw_results,    # Path to save BirdNET results from config
        rtype = 'r',
        slist = path_to_species_list,       # Path to species list file from config
        lat = -1,
        lon = -1,
        sensitivity = 1,
        min.conf = 0.1,
        overlap = 0,
        threads = 11,                       # Number of CPU threads. As a rule of thumb, set "threads" < logical processors - 1.
        batchsize = 32,                     # Number of samples to process at the same time
        fmax = 20000                        # Maximum frequency for bandpass filter
)

#________________________________________________________
# Gather, Combine, and Format all Raw CSV BirdNET Results ----
#________________________________________________________

# Get and store the paths for all the raw csv BirdNET results
birdnet_raw_csv_paths <- dir_ls(BirdNET_raw_results, type = "file", 
                                recurse = TRUE, regexp = "\\.csv$")

# Gather all csv files as data frames in one list (ensure that the column names
# in the BirdNET raw CSV files match the ones listed below, as they may differ
# slightly depending on the BirdNET version)

list_of_data_frames <- lapply(birdnet_raw_csv_paths, function(file) {
        read_csv(file, col_types = cols(
                filepath = col_character(),
                start = col_double(),
                end = col_double(),
                scientific_name = col_character(),
                common_name = col_character(),
                confidence = col_double(),
                lat = col_double(),
                lon = col_double(),
                week = col_double(),
                overlap = col_double(),
                sensitivity = col_double(),
                min_conf = col_double(),
                species_list = col_character(),
                model = col_character()
        ))
})

# Combine rows from all csv files into one data frame
raw_birdnet_results <- bind_rows(list_of_data_frames)

# Format BirdNET results with a "recordingID", "siteID", and "datetime" columns
formatted_birdnet_results <- raw_birdnet_results %>%
        mutate(
                recordingID = paste0(
                        str_extract(filepath, "Survey-\\d+"), "_",              # Extract Survey info (e.g., Survey-001)
                        str_extract(filepath, "Site-\\d+"), "_",                # Extract Site info (e.g., Site-001)
                        str_extract(filepath, "\\d{8}_\\d{6}"), "_",            # Extract datetime (e.g., 20240423_052000)
                        
                        "start_sec_", start, "_",                               # Add start second (e.g., start_sec_18)
                        "end_sec_", end, "_",                                   # Add end second(e.g., end_sec_21)
                        "confidence_", confidence, "_",                         # Add confidence (e.g., confidence_0.8658)
                        str_replace_all(scientific_name, " ", "_"), ".wav"      # Add scientific name and file extension
                ),
                siteID = str_extract(filepath, "Site-\\d+"),
                datetime = ymd_hms(str_extract(filepath, "\\d{8}_\\d{6}"))
        )
