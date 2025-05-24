#________________________________________________________
# Script Overview: Producing and Exporting the Final Occurrence Dataset ----
#________________________________________________________

# This script processes the "formatted_birdnet_results_isolated_detections" object,
# which is produced in the "RF_probability_modelling.R" script, and creates the 
# final occurrence dataset following the Darwin Core standard. In addition to the
# standard Darwin Core terms, the final dataset includes other custom attributes/terms.
# A detailed description of the list of attributes included in the final dataset 
# is available at:
# https://smog-chalmers.github.io/BirdMonitoringGothenburg/

# Required Objects:
 
# The script relies on the following R objects produced in prior scripts:

# 1. formatted_birdnet_results_isolated_detections
# 2. processed_valid_sheets
# 3. rf_cv_model
# 4. expert_validated_training_dataset
# 5. species_model_train

# Please ensure these objects are sourced before running this script.

#________________________________________________________
# Load Necessary Libraries ----
#________________________________________________________

library(yaml)
library(dplyr)
library(readxl)
library(uuid)
library(fs)
library(stringr)

#________________________________________________________
# Load the Configuration File "config.yaml" ----
#________________________________________________________

# Load the configuration file
config <- yaml::read_yaml("config.yaml")

# Extract paths from the configuration file
output_path <- config$output_path                
path_to_species_taxon_data <- config$path_to_species_taxon_data
path_to_site_metadata <- config$path_to_site_metadata

#________________________________________________________
# Filter the Formatted BirdNET Results ----
#________________________________________________________

# Define the confidence threshold and species list that were used to retrieve sample
# audio files for validation using the "retrieve_sample_audio" function.
confidence_threshold <- "<confidence_threshold_value>"  # Replace with a numeric value (e.g., 0.85)
species_list <- c("<species_1>", "<species_2>", "<species_3>")  # Replace with species names

filtered_birdnet_results <- formatted_birdnet_results_isolated_detections %>%
        filter(confidence >= confidence_threshold & scientific_name %in% species_list
               ) %>%
        select(filepath, start, end, scientific_name, confidence, recordingID, 
               siteID, datetime, isIsolated
               )

#________________________________________________________
# Add Additional Data from the Validation Process to the Filtered BirdNET Results ----
#________________________________________________________

# Merge "filtered_birdnet_results" with "processed_valid_sheets" based on the 
# "recordingID". The "BirdNETClass" column is removed from "processed_valid_sheets"
# because the species name is already provided under the "scientific_name" column
# in "filtered_birdnet_results".
birdnet_results_with_validation_data <- merge(filtered_birdnet_results, 
                                              processed_valid_sheets[, -which(names(processed_valid_sheets) == "BirdNETClass")],
                                                     by = "recordingID",
                                                     all.x = TRUE)

#________________________________________________________
# Predict the Occurrence Probability of Non-validated Records Using the Trained ----
# Random Forest Model
#________________________________________________________

# Extract the non-validated records (i.e., those that were not validated by the 
# expert) for prediction
non_validated_records <- birdnet_results_with_validation_data %>%
        filter(is.na(expertValidated))

# Prepare for modeling: We should model only the species present in the training
# dataset and ensure that the column names and structure are consistent with those
# used in the training dataset ("expert_validated_training_dataset").
colnames(expert_validated_training_dataset)
# [1] "confidence"   "isIsolated"   "BirdNETClass" "correct"

# Filter the non-validated records to only include those species that were present
# in the training dataset and select relevant columns for prediction.
non_validated_records_to_predict <- non_validated_records %>%
        filter(scientific_name %in% species_model_train) %>%
        select(recordingID, confidence, isIsolated, scientific_name) %>%
        rename(BirdNETClass = scientific_name)  # Renaming scientific_name to BirdNETClass to match the training dataset's structure

str(non_validated_records_to_predict)

# Convert necessary columns to factors to match the model's expected input
non_validated_records_to_predict$BirdNETClass <- factor(non_validated_records_to_predict$BirdNETClass,
                                                        levels = levels(expert_validated_training_dataset$BirdNETClass))
non_validated_records_to_predict$isIsolated <- factor(non_validated_records_to_predict$isIsolated,
                                                      levels = levels(expert_validated_training_dataset$isIsolated))

str(non_validated_records_to_predict)

# Apply the RF model to predict the probability of non-validated records being correct
non_validated_records_to_predict <- non_validated_records_to_predict %>%
        mutate(predicted_prob = predict(rf_cv_model, newdata = ., type = "prob")[, "yes"]) %>%
        # Round the predicted probabilities to two decimal places
        mutate(predicted_prob = round(predicted_prob, 2))

# Merge the predicted probabilities back to the full dataset with the expert validation
# data (birdnet_results_with_validation_data) using "recordingID"
birdnet_results_with_validation_data_probs <- left_join(birdnet_results_with_validation_data, 
                                                        non_validated_records_to_predict[,c("recordingID", "predicted_prob")],
                                                        by = "recordingID")

#________________________________________________________
# Assign Final Classification Based on the Expert Validation ----
#________________________________________________________

birdnet_results_with_final_classification <- birdnet_results_with_validation_data_probs %>%
        rowwise() %>%
        mutate(final_classification = 
                       case_when(
                               # For expert-validated records
                               expertValidated == TRUE & correct == 1 ~ scientific_name,
                               expertValidated == TRUE & unsure == 1  ~ "Remove", # Expert-validated records marked unsure are removed
                               expertValidated == TRUE & incorrect == 1 & n_expertLabels != 1 ~ "Remove", # Incorrect expert-validated records with no or multiple labels are removed
                               expertValidated == TRUE & incorrect == 1 & n_expertLabels == 1  ~ expertLabel_1, # Incorrect expert-validated records with only one label, use the expert label
                               
                               # For non expert-validated records
                               is.na(expertValidated) & predicted_prob > 0.50 ~ scientific_name,
                               is.na(expertValidated) & predicted_prob <= 0.50 ~ "Remove",
                               is.na(expertValidated) & is.na(predicted_prob) ~ "Remove" # Non-validated records with no predicted probability (species not in final list/training dataset)
                       )
        )

#________________________________________________________
# Filter Out Records Marked for Removal ----
#________________________________________________________

# Filter out records that have been marked for removal in the "final_classification" column
birdnet_results_with_final_classification_cleaned <- birdnet_results_with_final_classification %>%
        filter(final_classification != "Remove")

#________________________________________________________
# Flag Reclassified Records ----
#________________________________________________________

# Create a new column to flag whether the record was reclassified by the expert 
# from the original BirdNET classification
birdnet_results_with_flagged_reclassified_records <- birdnet_results_with_final_classification_cleaned %>%
        rowwise() %>%
        mutate(reclassified = 
                       case_when(expertValidated == TRUE & incorrect == 1 & n_expertLabels == 1  ~ "Yes")
        )

#________________________________________________________
# Calculate Occurrence Probability for Each Record ----
#________________________________________________________

# Calculate the occurrence probability based on different conditions
birdnet_results_with_occurrence_probability <- birdnet_results_with_flagged_reclassified_records %>%
        rowwise() %>%
        mutate(occurrenceProbability = 
                       case_when(
                               # For expert-validated records
                               expertValidated == TRUE & correct == 1 ~ 1,
                               expertValidated == TRUE & incorrect == 1 & n_expertLabels == 1  ~ 1,
                                 
                               # For non-expert-validated records
                               is.na(expertValidated) ~ predicted_prob
                       )
        )

#________________________________________________________
# Load Species Taxon Data ----
#________________________________________________________

# Load species taxon data from the Swedish Species Information Center (Artportalen)
species_taxon_data <- read_xlsx(path_to_species_taxon_data)
#colnames(species_taxon_data)

# Merge the taxon attributes with the processed BirdNet results
birdnet_results_with_taxon_data <- merge(birdnet_results_with_occurrence_probability, species_taxon_data, 
                                         by.x = "final_classification", 
                                         by.y = "Sökterm", # Make sure this column name corresponds to the taxon data
                                         all.x = TRUE)

#________________________________________________________
# Load Site Metadata ----
#________________________________________________________

# Load site metadata and merge with the BirdNet results
site_metadata <- read.csv(path_to_site_metadata, sep = ";")
#colnames(site_metadata)

birdnet_results_with_site_metadata <- merge(birdnet_results_with_taxon_data, site_metadata,
                                            by.x = "siteID",
                                            by.y = "MSD", # Make sure this column name corresponds to the site metadata
                                            all.x = TRUE)

#________________________________________________________
# Create the Final Dataset in Darwin Core Standard ----
#________________________________________________________

# Create the final dataset by renaming and restructuring columns to align with
# the Darwin Core standard
final_occurance_dataset <- birdnet_results_with_site_metadata %>%
        # Select the required columns
        select(filepath, recordingID, scientific_name, confidence, datetime, start, end,
               
               Vetenskapligt.namn, Auktor, Svenskt.namn, Engelskt.namn, 
               Guid, Global.sorteringsordning, Rike, Fylum, Klass, Ordning, Familj, Släkte,
               
               expertValidated, isIsolated, reclassified, occurrenceProbability,
               
               lat, long, bcl, site_name) %>%
        
        # Rename columns to match the Darwin Core standard
        rename(decimalLatitude = lat,
               decimalLongitude = long,
               taxonID = Guid,
               kingdom = Rike,
               phylum = Fylum,
               class = Klass,
               order = Ordning,
               family = Familj,
               genus = Släkte,
               BirdNETConfidence = confidence,
               BirdNETClass = scientific_name,
               commonNameSwedish = Svenskt.namn,
               commonNameEnglish = Engelskt.namn,
               siteName = site_name,
               globalSortOrder = Global.sorteringsordning
               ) %>%
        
        # Create or modify columns to adhere to the Darwin Core standard
        mutate(
                occurrenceID  = UUIDgenerate(n = n()),  # Generate a UUID
                basisOfRecord = "MachineObservation",
                scientificName = ifelse(!is.na(Auktor), paste0(Vetenskapligt.namn, " ", Auktor),
                                        Vetenskapligt.namn),
                eventDate = format(datetime, "%Y-%m-%d"),
                
                # Add the time interval during which the detection occurred. 
                # The “/” solidus character separates start and end times in the
                # representation of a time interval following ISO 8601-1:2019.
                eventTime = ifelse(
                        end == 60,
                        paste0(
                                format(datetime, "%H:%M:"),
                                sprintf("%02d", start),
                                "/",
                                format(datetime + 60, "%H:%M:00") # Add 60 seconds to adjust overflow
                        ),
                        paste0(
                                format(datetime, "%H:%M:"),
                                sprintf("%02d", start),
                                "/",
                                format(datetime, "%H:%M:"),
                                sprintf("%02d", end)
                        )
                ),
                geodeticDatum = "EPSG:4326",
                country = "Sweden",
                countryCode = "SE",
                detectionDistanceInMeters = "20-70",
                taxonRank = "species",
                expertValidated = case_when(
                        expertValidated == TRUE ~ "Yes",
                        is.na(expertValidated) ~ "No"),
                isIsolated = ifelse(isIsolated == TRUE, "Yes", "No"),
                reclassified = ifelse(is.na(reclassified), "No", reclassified)
        ) %>%
        
        # Remove un-needed columns
        select(-c(filepath, Vetenskapligt.namn, datetime, start, end, Auktor, recordingID)) %>% 
        
        # Re-order columns
        relocate(
                # Darwin Core required columns/attributes/terms
                occurrenceID, basisOfRecord, scientificName, eventDate,
                
                # Recommended columns/attributes/terms 
                eventTime,
                
                # Location columns/attributes/terms
                decimalLatitude, decimalLongitude, geodeticDatum,
                country, countryCode,
                
                # Taxonomy columns/attributes/terms 
                taxonRank, kingdom, phylum, class, order, family, genus, taxonID,
                globalSortOrder,
                
                # Custom columns/attributes/terms
                BirdNETClass,BirdNETConfidence, expertValidated, isIsolated, 
                reclassified, occurrenceProbability, commonNameSwedish, commonNameEnglish,
                detectionDistanceInMeters, siteName, bcl
        )

#________________________________________________________
# Handle Duplicate Records (if any) in the Columns "scientificName", "siteName", ----
# "eventTime", "eventDate" as a Result of Re-classification
#________________________________________________________

# Reclassification of records based on an the expert's suggestions may result in
# a record being reclassified to the same species already identified by BirdNET
# at the same date, time, and site. Since the dataset does not include individual
# counts, we should retain only the record with the highest occurrenceProbability
# (where 1 = expert-validated).

# Define the columns for duplicate identification
duplicate_cols <- c("scientificName", "siteName", "eventDate", "eventTime")

# Remove duplicates while prioritizing based on occurrenceProbability
final_occurance_dataset_no_duplicates <- final_occurance_dataset %>%
        group_by(across(all_of(duplicate_cols))) %>%
        
        # Prioritize rows with the highest occurrenceProbability
        arrange(desc(occurrenceProbability)) %>% 
        
        # Keep only the top row of each group
        slice_head(n = 1) %>%
        ungroup()

#________________________________________________________
# Save the Final Dataset as CSV ----
#________________________________________________________

# Write the final dataset to a CSV file in the specified output directory
write.csv(final_occurance_dataset_no_duplicates, file = file.path(output_path, "occurrence.txt"), row.names = FALSE)

#________________________________________________________
# Create and Save a Species Checklist
#________________________________________________________

species_checklist <-  final_occurance_dataset_no_duplicates %>% 
        distinct(scientificName, .keep_all = TRUE) %>%
        select(scientificName, taxonID)

# Write the species_checklist to a CSV file in the specified output directory
write.csv(species_checklist, file = file.path(output_path, "species.txt"), row.names = FALSE)

#________________________________________________________
# Create and save a Histogram for the occurrenceProbability ----
#________________________________________________________

occurrenceProbability_hist <- ggplot(data = final_occurance_dataset_no_duplicates, 
                                     mapping = aes(x = occurrenceProbability, fill = expertValidated)) +
        geom_histogram() +
        geom_vline(xintercept = round(mean_best_threshold, 2), linetype = "dashed", color = "black") +
        geom_vline(xintercept = round(mean_best_specificity_threshold, 2), linetype = "dashed", color = "black") +
        scale_x_continuous(
                breaks = c(round(mean_best_threshold, 2), round(mean_best_specificity_threshold, 2), seq(0, 1, by = 0.1)),  # Add thresholds as breaks along with default ones
                labels = scales::label_number()  # Ensures the labels are formatted as numbers
                ) +
        labs(x = "Probability threshold/cutoff", y = "Frequency")

# Save histogram to file
ggsave(
        filename = "occurrenceProbability_hist.jpg",
        plot = occurrenceProbability_hist,
        units = "in",
        width = 8,
        height = 5,
        dpi = 330
)
