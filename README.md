# Global-Tuberculosis-Analysis
Analysis of tuberculosis incidence and mortality trends in Egypt from 2000 to 2024.

## Project Overview

This project examines trends in tuberculosis (TB) incidence and mortality in Egypt between 2000 and 2024, using estimates published by the World Health Organization (WHO).

The analysis explores changes in Egypt's TB burden over time and places the country's progress in a wider global and regional context. Comparisons are made across WHO regions and between Egypt and the Eastern Mediterranean Region (EMR). The project also examines country rankings for TB incidence and mortality in 2024.

The analysis was conducted using R and Quarto, with visualisations generated to present the findings.

## Objectives

The main objectives of this project are to:

- Examine trends in TB incidence and mortality in Egypt from 2000 to 2024.
- Compare TB incidence and mortality trends across WHO regions.
- Compare Egypt's TB burden with the rest of the Eastern Mediterranean Region.
- Identify countries with the highest estimated TB incidence and mortality rates in 2024.
- Assess Egypt's global and regional rankings for TB incidence and mortality.

## Data Sources

The analysis uses WHO tuberculosis burden estimates.

The project includes the following datasets:

- `TB_burden_countries.csv`: Country-level TB burden data.
- `TB_data_dictionary.csv`: Data dictionary describing the variables included in the dataset.

The study period covers 2000–2024. The analysis focuses on estimated TB incidence and mortality rates, expressed per 100,000 population.

## Repository Structure

The project is organised as follows:

```text
Global_Tuberculosis_Analysis/
│
├── tuberculosis_analysis.Rproj
├── tuberculosis_analysis.qmd
├── tuberculosis_analysis.R
├── README.md
│
├── data/
│   └── raw/
│       ├── TB_burden_countries.csv
│       └── TB_data_dictionary.csv
│
├── figures/
│   ├── 01_global_tb_trends.png
│   ├── 02_regional_tb_comparison.png
│   ├── 03_regional_tb_mortality_trends.png
│   ├── 04_regional_tb_incidence_trends.png
│   ├── 05_regional_tb_incidence_percentage_change.png
│   ├── 06_regional_tb_mortality_percentage_change.png
│   ├── 07_top10_countries_tb_incidence.png
│   ├── 08_top10_countries_tb_mortality.png
│   ├── 09_egypt_global_tb_mortality_ranking.png
│   ├── 10_egypt_global_tb_incidence_ranking.png
│   ├── 11_emr_tb_mortality_ranking.png
│   ├── 12_emr_tb_incidence_ranking.png
│   ├── 13_egypt_tb_trends.png
│   └── 14_egypt_vs_emr_tb_comparison.png
│
└── output/
    └── tuberculosis_analysis.html
```

## Methods and Analysis

The analysis was performed using R, with data processing, statistical summaries, and visualisation techniques.

The main stages include:

1. **Data preparation:** Importing the WHO datasets, examining their structure, and preparing the data for analysis.
2. **Global analysis:** Exploring global TB incidence and mortality trends over the study period.
3. **Regional analysis:** Comparing trends and percentage changes across WHO regions.
4. **Country comparisons:** Identifying the ten countries with the highest estimated TB incidence and mortality rates in 2024.
5. **Egypt-specific analysis:** Examining Egypt's incidence and mortality trends from 2000 to 2024.
6. **Ranking analysis:** Assessing Egypt's position globally and within the EMR in 2024.
7. **Regional comparison:** Comparing Egypt's TB burden with the corresponding estimates for the EMR, excluding Egypt.

Country rankings were calculated using the `min_rank()` function, with higher TB rates receiving higher priority in the ranking.

## Key Findings

The analysis shows a substantial decline in Egypt's estimated TB incidence and mortality rates between 2000 and 2024.

- TB incidence in Egypt decreased from approximately 21 to 10 cases per 100,000 population.
- TB mortality decreased from approximately 1.5 to 0.4 deaths per 100,000 population.
- In 2024, Egypt ranked 151st globally for TB incidence and 162nd for TB mortality.
- Within the EMR, Egypt ranked 16th out of 22 countries for both incidence and mortality, according to the ranking method used in this analysis.
- Egypt's estimated TB burden remained substantially lower than that of the EMR excluding Egypt.

Overall, the findings indicate considerable progress in reducing Egypt's TB burden, although continued efforts are needed to sustain these improvements.

## Tools and Technologies

- **R:** Data cleaning, processing, analysis, and visualisation.
- **RStudio:** Development environment for the project.
- **Quarto:** Preparation and rendering of the analytical report.
- **ggplot2:** Data visualisation.
- **WHO datasets:** Source of tuberculosis burden estimates.

## How to Reproduce the Analysis

To reproduce the analysis:

1. Clone or download this repository.
2. Open `tuberculosis_analysis.Rproj` in RStudio.
3. Ensure that the required R packages are installed.
4. Check that the datasets are available in `data/raw/`.
5. Open `tuberculosis_analysis.qmd` in RStudio.
6. Render the Quarto document to generate the HTML report.

The R script, `tuberculosis_analysis.R`, is also included separately for reference and reproducibility.

## Limitations

This analysis is based on WHO estimates rather than directly observed counts of all TB cases and deaths. Estimates are subject to uncertainty and may be revised as new data become available.

The analysis is descriptive and does not establish causal relationships between specific interventions and changes in TB incidence or mortality.

## Acknowledgements

The author acknowledges the World Health Organization (WHO) for providing the tuberculosis burden estimates used in this analysis.
