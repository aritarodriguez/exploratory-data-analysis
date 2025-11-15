# exploratory-data-analysis

# README – Exploratory Data Analysis, Missing Values, and Outliers in R

This script performs a **complete exploratory data analysis (EDA)** on a simulated dataset, including detection and treatment of missing values, outlier identification, visualizations, and correlation analysis.

## Script Contents
1. **Library loading** (`tidyverse`, `ggplot2`, `corrplot`, `VIM`, `naniar`).
2. **Generation of simulated data** with missing values and outliers.
3. **Initial exploration** (`str`, `summary`).
4. **Missing value detection**:
   - Count per variable (`miss_var_summary`)
   - Visualization (`gg_miss_var`, `aggr`)
5. **Handling missing values**:
   - Removing incomplete rows
   - Mean imputation
6. **Outlier detection** using the IQR rule and a custom function.
7. **Visualizations**:
   - Boxplots and histograms (base R and ggplot2)
   - Scatterplots with regression lines
8. **Correlation and covariance**:
   - Numeric matrices
   - Visualization with `corrplot`
9. **Group analysis** (grades by gender + statistical summary).

   ```r
   install.packages(c("tidyverse", "ggplot2", "corrplot", "VIM", "naniar"))
