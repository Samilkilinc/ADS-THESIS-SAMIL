# Causalcasemix

This project investigates how changes in case-mix affect the discrimination and calibration of diagnostic and prognostic prediction models.

## Structure of the Project

### Data

- **raw**: Contains the raw data files.
- **processed**: Contains the processed data files.

### Scripts

- **1a_process_ari.R**: Processes the raw ARI data.
- **1a_process_news.R**: Processes the raw NEWS data.

### Metadata

- **ari-description.qmd**: Explores the ARI dataset.
- **news-description.qmd**: Explores the NEWS dataset.
- **metadata_ari.json**: Metadata file for the ARI dataset.
- **metadata_news.json**: Metadata file for the NEWS dataset.

### Results

- **results_1trainenv.csv**: Contains the overall results.
- **graphics**
  - **arrow_plot_trainenv_int.pdf**: Arrow plot for calibration intercept.
  - **arrow_plot_trainenv_slope.pdf**: Arrow plot for calibration slope.
  - **ari**: AUC and calibration plots for each environment and task in the ARI dataset.
  - **news**: AUC and calibration plots for each environment and task in the NEWS dataset.

## Get Started

### Download Data

To reproduce the results, download and store the data correctly:

1. Download `ari.zip` from the WHO ARI Multicentre Study of clinical signs and etiologic agents at [http://hbiostat.org/data](http://hbiostat.org/data) courtesy of the Vanderbilt University Department of Biostatistics.
2. Download the full dataset from [Dryad](https://doi.org/10.5061/dryad.d22q6vh).

Store the raw data files under `data/raw`.

### Scripts

1. **ari-description.qmd** and **news-description.qmd**: Explore the datasets.
2. **1a_process_ari.R** and **1a_process_news.R**:
   - Process the raw data.
   - Store the new processed data under `data/processed`.
   - Create metadata files for both datasets: `metadata_ari.json` and `metadata_news.json`, stored under `data/metadata`.
3. **2_fitmodels.R**: Fit logistic regression models using the task information in the metadata files and evaluate with AUC and calibration plots stored in `results/graphics`. Running the last function in the file creates an overall results table: `results_1trainenv.csv`.
4. **3_plot_results.R**: Use the results table `results_1trainenv.csv` to plot arrows from the discrimination and calibration scores of the training set to the different test sets (`arrow_plot_trainenv_int.pdf` and `arrow_plot_trainenv_slope.pdf`). Calculate the mean values of the results table.

## References

1. **Vanderbilt University Department of Biostatistics**: WHO ARI Multicentre Study data. [Link](http://hbiostat.org/data)
2. **Eckart, Andreas et al. (2018)**: Combination of the National Early Warning Score (NEWS) and inflammatory biomarkers for early risk stratification in emergency department patients: results of a multi-national, observational study. [Dryad](https://doi.org/10.5061/dryad.d22q6vh)
