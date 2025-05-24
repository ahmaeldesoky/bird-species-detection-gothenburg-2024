#________________________________________________________
# Script Overview: BirdNET Classification Accuracy ----
#________________________________________________________

# This script calculates and visualizes, following Sethi et al. (2024), BirdNET 
# classification accuracy for each species based on expert manual validation. 

# Key outputs include:

# 1. species_accuracy_and_misclassification.txt: A summary table that includes the
#    accuracy of the BirdNET classification for each species and misclassification
#    probabilities (probabilities being other species) based on expert validation. 
# 2. final_species_list: A vector of species found in the analyzed recordings.

# Key steps in the script:

# 1. Load expert validation data from multiple Excel files created using the 
#    "retrieve_sample_audio" function.
# 2. Calculate validation statistics (correct, incorrect, unsure).
# 3. Visualize the classification accuracy for each species using a bar chart
#    following Sethi et al. (2024).
# 4. Generate the final species list found in the analyzed recordings
# 4. Calculate misclassification probabilities for each species (probabilities 
#    being other species)
# 5. Export the species summary table (accuracy and misclassification probabilities) 
#    as a TXT file.

# The script relies on the following inputs:
 
# 1. Configuration settings from a "config.yaml" file, which specifies paths and output
#    locations.
# 2. "validation_data" directory containing Excel files with expert validation data.

#________________________________________________________
# Load Necessary Libraries -----
#________________________________________________________

library(yaml)
library(fs)
library(stringr)
library(readxl)
library(dplyr)
library(tibble)
library(reshape2)
library(ggplot2)
library(tidyr)
library(gridExtra)

#________________________________________________________
# Load Configuration File "config.yaml"
#________________________________________________________

# Load the configuration file "config.yaml" and extract the path to validation data.
config <- read_yaml("config.yaml")
validation_data <- config$validation_data
output_path <- config$output_path 

#________________________________________________________
# List and Process Excel Files ----
#________________________________________________________

# Retrieve a list of all xlsx files in the validation_data directory and its sub-directories
paths_xlsx <- dir_ls(validation_data, type = "file", regexp = "\\.xlsx", recurse = TRUE)

# Extract the base names (file names without directory paths) and remove the ".xlsx" extension
names_xlsx <- basename(paths_xlsx) %>% str_remove("\\.xlsx") 

# Print file names for debugging and confirmation
print(names_xlsx)  

#________________________________________________________
# Read all XLSX Files into a List ----
#________________________________________________________

# Read the contents of each file and store them in a list
list_xlsx <- lapply(paths_xlsx, read_xlsx)

# Assign the xlsx file names (species names) as keys to the list
names(list_xlsx) <- names_xlsx

#________________________________________________________
# Add Species Names and Calculate Statistics ----
#________________________________________________________

# For each xlsx in the list, add a new column ("BirdNETClass") with the corresponding species name
list_xlsx <- lapply(names(list_xlsx), function(name) {
        mutate(list_xlsx[[name]], BirdNETClass = name)
        })

# Restore names after modification
names(list_xlsx) <- names_xlsx  

# Calculate validation statistics
# For each xlsx file, calculate the column sums for "correct", "incorrect", and "unsure".
# Calculate the accuracy by dividing these sums by the number of rows in the data frame.
valid_stats <- sapply(list_xlsx, function(sheet) {
        colSums(sheet[, c("correct", "incorrect", "unsure")], na.rm = TRUE) / nrow(sheet)
        })

#________________________________________________________
# Transform and Organize Validation Statistics ----
#________________________________________________________

# Transpose the matrix of statistics, convert it to a data frame, and add a column for species names.
valid_stats <- valid_stats %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var = "BirdNETClass") %>%
        # Arrange rows by descending "correct" and "incorrect" values for later visualization.
        arrange(desc(correct), desc(incorrect))

#________________________________________________________
# Reshape Data for Visualization ----
#________________________________________________________

# Convert the data from wide to long format to prepare for ggplot2 bar chart plotting.
valid_stats_long <- melt(valid_stats, id.vars = "BirdNETClass") %>%
        # Convert the "BirdNETClass" column into a factor and set its levels based
        # on the order in the valid_stats data frame. This ensures the levels (species)
        # are ordered by descending "correct" and "incorrect" values for proper plotting.
        mutate(BirdNETClass = factor(BirdNETClass, levels = valid_stats$BirdNETClass))

#________________________________________________________
# Add the Number of Expert-validated Files per Species to the Reshaped Data for Visualization ----
#________________________________________________________

# Combine all sheets in one data frame
all_valid_sheets <- bind_rows(list_xlsx)

# Calculate the number of expert-validated files per species
n_files_expert_validated <- all_valid_sheets %>%
        group_by(BirdNETClass) %>%
        summarise(n_files_validated = n())

# Add the number of expert-validated files per specie to the reshaped data 
valid_stats_long_n_files_expert_validated <- merge(valid_stats_long, n_files_expert_validated, 
                                                   by = "BirdNETClass", sort = FALSE)

# Reorder the levels of the "variable" factor so that "correct" appears first, followed by "unsure" and "incorrect"
valid_stats_long_n_files_expert_validated$variable <- factor(valid_stats_long_n_files_expert_validated$variable, 
                                                        levels = c("unsure", "incorrect", "correct"))

#________________________________________________________
# Plot Validation Results ----
#________________________________________________________

# Create the stacked bar chart for species with 50 validated records
accuracy_plot_50 <- ggplot(subset(valid_stats_long_n_files_expert_validated, n_files_validated == 50), 
                               aes(x = value, y = BirdNETClass, fill = variable)) +
        geom_bar(stat = "identity", width = 0.65) +
        scale_fill_manual(values = c("correct" = "forest green", "incorrect" = "red", "unsure" = "azure4")) +
        labs(x = "Accuracy", y = "Species", fill = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), axis.text.y = element_text(vjust = 0.3, size = 7.5),
              legend.position = "none") +
        geom_vline(xintercept = 0.60, linetype = "dashed", color = "black", size = .5) +
        scale_y_discrete(limits=rev) # Reverse the y axis

# Create the stacked bar chart for species with < 50 validated records
accuracy_plot_under50 <- ggplot(subset(valid_stats_long_n_files_expert_validated, n_files_validated < 50), 
                                aes(x = value, y = BirdNETClass, fill = variable)) +
        geom_bar(stat = "identity", width = 0.65) +
        scale_fill_manual(values = c("correct" = "forest green", "incorrect" = "red", "unsure" = "azure4")) +
        labs(x = "Accuracy", y = "Species", fill = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), axis.text.y = element_text(vjust = 0.3, size = 7.5),
              legend.position = "none") +
        geom_vline(xintercept = 0.60, linetype = "dashed", color = "black", size = .5) +
        scale_y_discrete(limits=rev) # Reverse the y axis

# Save the plot to file
ggsave(
        filename = "accuracy_plot.jpg",
        plot = grid.arrange(accuracy_plot_50, accuracy_plot_under50, ncol = 2),
        units = "in",
        width = 7.5,
        height = 12,
        dpi = 330
)

#________________________________________________________
# Generate the Final Species List ----
#________________________________________________________

# Split the species names in the "expertLabel" column of the "all_valid_sheets" 
# data frame into separate columns based on a delimiter ", ". The "expertLabel" 
# column should contain species names in a comma-separated format, such as 
# "Turdus merula, Sylvia borin, Corvus cornix". 
# If a different delimiter is used, modify the 'delim' argument accordingly.
processed_valid_sheets <- separate_wider_delim (all_valid_sheets, expertLabel,
                                          delim = ", ", # The delimiter between labels in the "expertLabel" column 
                                          names_sep = "_", # Create new column names by separating with '_'
                                          too_few = "align_start", # This ensures that all rows have the same number of columns after splitting
                                          #cols_remove = FALSE # Optionally, remove columns
)

# Extract unique species labeled by the expert from the columns starting with 
# "expertLabel_" in "processed_valid_sheets" and remove NAs.
species_labeled_by_expert <- unique(na.omit(unlist(processed_valid_sheets[, grepl("^expertLabel_", colnames(processed_valid_sheets))])))

# Combine the species labeled by the expert with those correctly identified by 
# BirdNET, keeping only unique species.
final_species_list <- unique(c(species_labeled_by_expert, valid_stats[valid_stats$correct != 0,]$BirdNETClass))

#________________________________________________________
# Calculate Misclassification Probabilities ----
#________________________________________________________

# Add a new column "n_expertLabels" to the "processed_valid_sheets" data frame
# (for weighting labels later if they are more than one for the same record)
processed_valid_sheets  <- processed_valid_sheets  %>%
        mutate(n_expertLabels = rowSums(!is.na(select(., matches("^expertLabel_\\d+")))),
               expertValidated = TRUE # Label all records as "expertValidated"
)
        
# Reshape the processed validation sheets to be in a long format
processed_valid_sheets_long <- melt(processed_valid_sheets ,
                                    id.vars = c("BirdNETClass","n_expertLabels"),
                                    measure.vars = grep("^expertLabel_\\d+", colnames(processed_valid_sheets), value = TRUE), # The columns with the expert labels
                                    value.name = "expertLabel",
                                    na.rm = TRUE
)

# Summarize the data by grouping by "BirdNETClass", "expertLabel", and "n_expertLabels"
# and calculating the count of occurrences for each combination
processed_valid_sheets_long_summarize <- processed_valid_sheets_long %>%
        group_by(BirdNETClass, expertLabel, n_expertLabels) %>%
        summarise(count = n(), .groups = "drop") %>%
        
        # Weight the count based on "n_expertLabels".
        # For example, a count of 10 (Species A being Species B 10 times) when the
        # "n_expertLabels" is 2 should be 5.
        # The weighted_count gives a normalized estimation of how often a certain
        # label occurs, considering how many labels there were in total.
        mutate(weighted_count = count/n_expertLabels) %>%
        group_by(BirdNETClass, expertLabel) %>%
        summarise(final_weighted_count = sum(weighted_count), .groups = "drop")

# Add the number of the files that were validated per species by the expert to 
# "processed_valid_sheets_long_summarize" and calculate the probability of a 
# species classified by BirdNET being another species.
processed_valid_sheets_long_summarize <- merge(processed_valid_sheets_long_summarize, 
                                               n_files_expert_validated,
                                               by = "BirdNETClass", all.x = TRUE) %>%
        mutate(probability = final_weighted_count/n_files_validated)

# Summarize the misclassification probabilities for each BirdNETClass
species_accuracy_and_misclassification <- processed_valid_sheets_long_summarize %>%
        
        # Step 1: Group the data by BirdNETClass
        # This groups all rows belonging to the same BirdNETClass together so that summarization happens for each group separately.
        group_by(BirdNETClass) %>%
        
        # Step 2: Combine all expert labels and their corresponding probabilities into a single string.
        # For each BirdNETClass, this concatenates each "expertLabel" along with its rounded probability.
        # The format of the resulting string is: 
        # "expertLabel #1 (probability #1), expertLabel #2 (probability #2), ..., expertLabel #n (probability #n)".
        summarise(
                misclassificationProbabilities = sort(
                        paste0(
                                expertLabel,
                                " (", round(probability, 2), ")",
                                collapse = ", " #  Combine all into one string, separated by commas
                                )
                        )
                )

# Add a column with the BirdNET classification accuracy of each species based on the random validated sample by the expert
species_accuracy_and_misclassification <- merge(species_accuracy_and_misclassification, valid_stats[, c("correct","BirdNETClass")],
                                       by = "BirdNETClass", all.y = TRUE) %>%
        rename(BirdNETClassAccuracy = correct) %>%
        mutate(BirdNETClassAccuracy = round(BirdNETClassAccuracy, digits = 2)) %>%
        relocate("BirdNETClass", "BirdNETClassAccuracy",  "misclassificationProbabilities")

# Add metadata as attributes to the table
attr(species_accuracy_and_misclassification$BirdNETClass, "description") <- "The BirdNET original classification"
attr(species_accuracy_and_misclassification$BirdNETClassAccuracy, "description") <- "The calculated BirdNET accuracy based on the random sample validated by an expert ornithologist"
attr(species_accuracy_and_misclassification$misclassificationProbabilities, "description") <- "The list of true species and their respective probabilities based on the ornithologist's suggestion"

#________________________________________________________
# Save the Final Reclassification Summary Table ----
#________________________________________________________

write.csv(species_accuracy_and_misclassification, file = file.path(output_path, "species_accuracy_and_misclassification.txt"), row.names = FALSE)
