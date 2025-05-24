#________________________________________________________
# Script Overview: Random Forest Predictive Model ----
#________________________________________________________

# This script trains and evaluates a Random Forest (RF) binary classifier to
# estimate the probability that non-expert-validated records are correct, based 
# on the expert-validated dataset. The model uses BirdNET confidence scores, 
# species names, and whether the detection is isolated (i.e., preceded or followed
# by a detection of the same species in a 9-s window) as predictors. The output 
# is a trained predictive classifier object.

# Key steps in the script:

# 1. Identify isolated detections in the  formatted BirdNET results dataset
# 2. Prepare the training dataset by combining the necessary attributes
# 3. Train a RF model using repeated cross-validation
# 4. Evaluate the model using ROC curves
# 5. Visualize the performance of the RF model

# Required Objects:

# The script relies on the following R objects produced in the "analyze_audio_files.R" and 
# "accuracy_and_plot.R" scripts:

# 1. formatted_birdnet_results
# 2. processed_valid_sheets: A data frame containing the results of the expert validation
# 3. species_accuracy_and_misclassification

# Please ensure these objects are sourced or loaded before running this script.

#________________________________________________________
# Load Necessary Libraries ----
#________________________________________________________

library(caret)
library(ranger)
library(pROC)
library(reshape2)
library(progress)
library(dplyr)

#________________________________________________________
# 1. Identify Isolated Detections in the "formatted_birdnet_results" ----
#________________________________________________________

# Split the formatted BirdNET results data frame by species and site, and add a 
# new datetime column (datetime_start)
formatted_birdnet_results_split <- formatted_birdnet_results %>%
        mutate(datetime_start = datetime + start) %>%
        split(., list(formatted_birdnet_results$scientific_name, formatted_birdnet_results$siteID))

# Remove empty data frames (nrow = 0) from the list
formatted_birdnet_results_split <- Filter(function(x) nrow(x) > 0, formatted_birdnet_results_split)

# Initialize progress bar
pb <- progress_bar$new(
        format = "Processing [:bar] :percent (:elapsed s)",
        total = length(formatted_birdnet_results_split), 
        clear = FALSE, 
        width = 60
)

# Initialize list to store results for isolated detections
isolated_detections_list <- list()

# Loop through each species-site data frame
for (i in seq_along(formatted_birdnet_results_split)) {
        pb$tick()  # Update progress bar
        
        # Identify isolated detections within each species-site data frame
        isolated_detections_list[[i]] <- formatted_birdnet_results_split[[i]] %>%
                arrange(datetime_start) %>%
                mutate(
                        # Check if each detection is surrounded by 3s neighbors
                        isSurrounded = (lag(datetime_start, 1) == datetime_start - 3) & 
                                (lead(datetime_start, 1) == datetime_start + 3),
                        
                        # Replace NAs values with FALSE 
                        isSurrounded = ifelse(is.na(isSurrounded), FALSE, isSurrounded),
                        
                        # Invert to find isolated detections (those not surrounded)
                        isIsolated = !isSurrounded
                )
}

# Assign names to the list based on species and site identifiers
names(isolated_detections_list) <- names(formatted_birdnet_results_split)

# Combine the isolated detections results into a single data frame
formatted_birdnet_results_isolated_detections <- bind_rows(isolated_detections_list)

#any(is.na(formatted_birdnet_results_isolated_detections$isIsolated)) 

# How many records are isolated detections?
# (sum(formatted_birdnet_results_isolated_detections$isIsolated == TRUE)/nrow(formatted_birdnet_results_isolated_detections))*100

#________________________________________________________
# 2. Prepare the Training Dataset ----
#________________________________________________________

# Add "confidence" and "isIsolated" columns to the "processed_valid_sheets" data frame
processed_valid_sheets_with_conf_isIsolated <- merge(formatted_birdnet_results_isolated_detections[, c("recordingID", "confidence", "isIsolated")], 
                                                     processed_valid_sheets, 
                                                     by = "recordingID", all.y = TRUE)

# Create the dataset for model training by including only records for species 
# with accuracy > 0 and keeping the necessary columns for model training, i.e.,
# "confidence", "isIsolated", "BirdNETClass", and "correct"
species_model_train <- species_accuracy_and_misclassification[species_accuracy_and_misclassification$BirdNETClassAccuracy > 0, ]$BirdNETClass

expert_validated_training_dataset <- processed_valid_sheets_with_conf_isIsolated %>%
        filter(BirdNETClass %in% species_model_train) %>%
        select (confidence, isIsolated, BirdNETClass, correct) %>%
        mutate(correct = ifelse(is.na(correct), 0, correct)) # Replace NA values in "correct" with 0

# Check the structure of the training dataset
str(expert_validated_training_dataset)

#________________________________________________________
# 3. Train a RF Predictive Model ----
#________________________________________________________

# Convert relevant columns into factors for model training
expert_validated_training_dataset$BirdNETClass <- as.factor(expert_validated_training_dataset$BirdNETClass)
expert_validated_training_dataset$isIsolated <- as.factor(expert_validated_training_dataset$isIsolated)
expert_validated_training_dataset$correct <- as.factor(ifelse(expert_validated_training_dataset$correct == 1, "yes", "no"))

# Check the structure of the dataset
str(expert_validated_training_dataset)
levels(expert_validated_training_dataset$correct) # The level of the factor we want
# to predict (i.e., "yes" which is the positive class) should be the second

# Specify the training rules (e.g., resampling method, performance metrics, etc.)
# using caret's trainControl
set.seed(123)

cv_ctrl <- trainControl(
        method = "repeatedcv",     # Repeated cross-validation
        number = 5,                # 5 folds
        repeats = 5,               # Repeat it 5 times
        classProbs = TRUE,         # Enable probability output
        summaryFunction = twoClassSummary, # Calculates performance metrics for binary classification problems (i.e., ROC, Sensitivity, Specificity)
        verboseIter = TRUE,        # Show progress information during the cross-validation process
        savePredictions = TRUE     # Ensures that all the predictions made during the cross-validation process (including predictions for each fold and each resample) are stored in the modelObject$pred object.
)

# Display the cross-validation control parameters
print(cv_ctrl)

# Train RF model with ranger, testing different hyperparameters. 
# The final model is trained on the entire dataset.
rf_cv_model <- train(
        correct ~ confidence + isIsolated + BirdNETClass,
        data = expert_validated_training_dataset,
        method = "ranger",     # Use the ranger package to train a RF model (can handle categorical variables with many classes as in BirdNETClass).
        trControl = cv_ctrl,   # The training control rules
        metric = "ROC",        # Optimize based on AUC (we use the ROC for model evaluation and choice because it's best for binary classification)
        tuneLength = 3         # Automatically selects different combinations of hyperparameters (e.g., mtry, splitrule) and evaluates them using cross-validation. The optimal combination is selected based on the performance metric set (ROC in this case).
)

#________________________________________________________
# 4. Model  Results ----
#________________________________________________________
# The output represents the performance metrics (i.e., ROC, Sens, Spec) for a 
# specific combination of tuning parameters (mtry, splitrule), averaged over the
# cross-validated sets (5-fold repeated 5 times).

# Print the model results
print(rf_cv_model)

# Print the final trained model
rf_cv_model$finalModel

#________________________________________________________
# 5. Evaluate the Model's Performance ----
#________________________________________________________

# Extract the prediction results of all the cross-validated datasets (5 folds, 
# 6 combinations of parameters, and 5 repeats)
cv_predictions <- rf_cv_model$pred

# Filter this cross-validated datasets to keep only the ones with best combinations
# from "print(rf_cv_model)" and split by folds and repeats
cv_predictions_filtered <- cv_predictions %>%
        filter(mtry == 2 & splitrule == "gini") %>%
        group_by(Resample) %>%
        group_split()

#________________________________________________________
# 6. Calculate the ROC curve for each of the 25 cross validated datsets and save ----
# the results in a list. This calculates the sensitivity and specificity for each
# cross-validated dataset at different thresholds
#________________________________________________________

# Create a list to store ROC curves for each fold
roc_curves <- list()

# Loop through the filtered cross-validation data frames and calculate ROC curves
for (i in seq_along(cv_predictions_filtered)) {
        df_cv <- cv_predictions_filtered[[i]]
        roc_curves[[i]] <- roc(response = df_cv$obs, # The true, known class labels ("yes" or "no", for binary classification)
                               predictor = df_cv$yes # The predicted probabilities from the model, not the predicted class. This must be numeric and represents the model's confidence that the class is "yes".
        )
}

# Plot all ROC curves using ggroc (this combines them in one ggplot)
roc_plot <- ggroc(roc_curves, colour = "black", size = 0.25) + # ggroc accepts lists
        theme(legend.position = "none") +
        labs(
                x = "Specificity",
                y = "Sensitivity"
        )

# Save ROC plot to file
ggsave(
        filename = "roc_plot.jpg",
        plot = roc_plot,
        units = "in",
        width = 6,
        height = 4,
        dpi = 330
)

#________________________________________________________
# 7. Extract Performance Metrics from ROC Curves ----
#________________________________________________________

## 7.1. Extract the best threshold/cutoff based on Youden’s J statistic (optimal ----
# threshold that balances both sensitivity and specificity)
roc_best_thresholds  <- list()
for (i in seq_along(roc_curves)) {
        roc_best_thresholds [[i]] <- coords(roc_curves[[i]], x = "best", # Which points on the ROC curve to extract metrics for ("best" for a cutoff that balances sensitivity and specificity)
                                       best.method = "youden",
                                       ret = c("threshold", "sensitivity", "specificity") # Performance metrics to extract from the ROC curve
        ) 
        roc_best_thresholds [[i]]$fold <- i # Add fold identifier for merging all dfs later
}

# Combine results into a single data frame
best_thresholds_df <- bind_rows(roc_best_thresholds)

# Calculate the mean of the best thresholds
mean_best_threshold <- mean(best_thresholds_df$threshold, na.rm = TRUE)
print(mean_best_threshold)

## 7.2. Extract performance metrics for all thresholds on the ROC curve ----
roc_all_thresholds <- list()
for (i in seq_along(roc_curves)) {
        roc_all_thresholds[[i]] <- coords(roc_curves[[i]], x = "all", # Which points on the ROC curve to extract metrics for ("all" for all points)
                                  ret = c("threshold", "sensitivity", "specificity") # Performance metrics to extract from the ROC curve
        ) 
        roc_all_thresholds[[i]]$fold <- i # Add fold identifier for merging all dfs later
}

# Combine all thresholds into one data frame
all_thresholds_df <- bind_rows(roc_all_thresholds)

# Find the best threshold that maximizes specificity (a strict cutoff; only predicts "yes" when the model is very confident)
best_specificity_thresholds <- all_thresholds_df %>%
        filter(!is.infinite(threshold)) %>% # Remove -Inf thresholds
        group_by(fold) %>% # For each cross-validation fold
        # Find all thresholds where specificity is at its maximum in that fold (can be more than one) and take the minimum of those tied-thresholds
        summarize(best_specificity_threshold = min(threshold[specificity == max(specificity)])) %>% 
        pull(best_specificity_threshold)

# Calculate the average threshold that maximizes specificity
mean_best_specificity_threshold <- mean(best_specificity_thresholds, na.rm = TRUE)
print(mean_best_specificity_threshold)

#________________________________________________________
# 8. Visualize Threshold Performance Metrics ----
#________________________________________________________

# Melt the "all_thresholds_df" into a long format for visualization
all_thresholds_df_long <- melt(
        all_thresholds_df,
        id.vars = c("threshold", "fold"),                # Keep threshold and fold as is
        measure.vars = c("sensitivity", "specificity"),  # These go into long format
        variable.name = "sensitivity_specificity",       # New column with "sensitivity" or "specificity"
        value.name = "value"                             # Their corresponding values
)

# Plot the sensitivity and specificity across thresholds
sens_spec_plot <- ggplot(all_thresholds_df_long, aes(x = threshold, y = value, color = sensitivity_specificity, 
                                                     group = interaction(fold, sensitivity_specificity))) +
        geom_line(alpha = 0.4) +
        labs(
                x = "Probability threshold/cutoff",
                y = "Sensitivity/Specificity"
        ) +
        scale_color_discrete(
                name = NULL,
                labels = c("sensitivity" = "Sensitivity", "specificity" = "Specificity")  # Custom labels
        ) +
        geom_vline(xintercept = round(mean_best_threshold, 2), linetype = "dashed", color = "black") + # Best averaged threshold that balances sensitivity and specificity
        geom_vline(xintercept = round(mean_best_specificity_threshold, 2), linetype = "dashed", color = "black") + # Best threshold that maximizes specificity
        scale_x_continuous(
                breaks = c(round(mean_best_threshold, 2), round(mean_best_specificity_threshold, 2), seq(0, 1, by = 0.1)),  # Add recommended thresholds as breaks along with default ones
                labels = scales::label_number()  # Ensures the labels are formatted as numbers
        )

# Save sensitivity-specificity plot to file
ggsave(
        filename = "sens_spec_plot.jpg",
        plot = sens_spec_plot,
        units = "in",
        width = 8,
        height = 5.2,
        dpi = 330
)



