# Introduction

This repository contains R scripts for analyzing audio files using the BirdNET classifier ([Kahl et al., 2021](https://www.sciencedirect.com/science/article/pii/S1574954121000273?via%3Dihub)), validating the results, visualizing classification accuracy, and generating a curated occurrence dataset in the Darwin Core standard.

# Scripts Overview

1. **`analyze_audio_files.R`**: Analyzes raw audio files using the BirdNET classifier and formats the output with additional attributes (column headers) for easier data manipulation.
2. **`retrieve_sample_audio.R`**: Function that retrieves and samples audio files based on different criteria for manual expert validation.
3. **`accuracy_and_plot.R`**: Calculates and visualizes BirdNET classification accuracy, following [Sethi et al., (2024)](https://www.pnas.org/doi/10.1073/pnas.2315933121), for each species based on expert manual validation.
4. **`RF_probability_modelling.R`**: Trains and evaluates a Random Forest model to predict the probability of correctness for non-validated records (i.e., the likelihood that the BirdNET classification is correct).
5. **`create_final_dataset.R`**: Produces and exports the final occurrence dataset in Darwin Core Standard.

# Directory Structure

The recommended directory structure for the above scripts to run as expected is as follows:

```plaintext
 Project/
 ├── README.md 
 ├── config.yaml              # Configuration file with paths and parameters
 ├── script/                  
 └── data/
     ├── species_data/
     │   ├── species_list.txt          # Custom/Global species list: "<scientific name>_<common name>"
     │   └── species_taxon_data.xlsx   # Taxon attributes (e.g., from Artportalen)
     ├── site_metadata.csv    # CSV containing site metadata (e.g., lat, long, site ID, site name)
     ├── raw_audio/           # Directory containing raw WAV audio files
     │   └── Survey-001/               # Survey/Round/Event ID
     │       └── Site-001/                      # Site ID                    
     │           └── 20240421/                           # Day/Date
     │               └── 20240421_083800.wav
     ├── BirdNET_raw_results/ # Raw CSV results from the BirdNET analysis
     │   └── Survey-001/
     │       └── Site-001/
     │           └── 20240421/
     │               └── 20240421_083800.BirdNET.results.r.csv
     ├── validation_data/     # Sample audio files for expert manual validation
     │   └── Carduelis_carduelis/      # Each species has a separate folder
     │       ├── Carduelis carduelis.xlsx      # Validation sheet
     │       └── Survey-001_Site-001_20240421_072400_start_sec_18_end_sec_21_confidence_0.8658_Cardueli_carduelis.wav
     └── output/              # Final results generated from processing
         ├── occurrence.txt
         └── species_accuracy_and_misclassification.txt
```

# About the Resulting Dataset 

The dataset produced using the above scripts includes **239,570 occurrence records** of **61 species** from April 21 to June 16, 2024, across 30 sites in central Gothenburg, Sweden. 

The dataset is available to download from https://zenodo.org/records/15350316 and a detailed documentation, including URI identifiers, definitions, and examples for all attributes (column headers), is available at https://smog-chalmers.github.io/BirdMonitoringGothenburg/. 

Depending on the use case, users may want to filter the dataset based on the [occurrenceProbability](https://smog-chalmers.github.io/BirdMonitoringGothenburg/#occurrenceProbability) attribute using the following recommended thresholds (see the paper for more details) to retain only non-validated records that meet the specified criteria:

- **Balancing sensitivity and specificity** (an optimal threshold determined based on [Youden's J statistic](https://acsjournals.onlinelibrary.wiley.com/doi/10.1002/1097-0142(1950)3:1%3C32::AID-CNCR2820030106%3E3.0.CO;2-3)):

    ```r
    occurrence %>% filter(occurrenceProbability >= 0.74)
    ```
    
-  **Maximizing specificity** (a stricter threshold that minimizes false positives):

    ```r
    occurrence %>% filter(occurrenceProbability >= 0.82)
    ```
    
# Citation

If you use this repository in your work, please cite it using the metadata available under 'Cite this repository' in the 'About' section. 

For the dataset, please cite as:

```bibtex
@misc{eldesoky2025birdspecies,
  author = {Eldesoky, A. H. and Gil, J. and Kindvall, O. and Stavroulaki, I. and Jonasson, L. and Bennet, D. and Yang, W. and Martínez, A. and Lichter, R. and Petrou, F. and Berghauser Pont, M.},
  title = {A bird species occurrence dataset from passive audio recordings across dense urban areas in Gothenburg, Sweden},
  year = {2025},
  note = {Data set},
  publisher = {Zenodo},
  doi = {10.5281/zenodo.15350316},
  url = {https://doi.org/10.5281/zenodo.15350316}
}
```
