This repository contains the code I wrote to analyze the 2008 DHS dataset. The focus of my analysis was to determine whether having an emigrant household member influences teenage pregnancy. The R scripts include a data preparation file that lists all the libraries I use and outlines my method for reducing the dataset to only the necessary columns. 

The analysis includes basic statistics, such as the percentage of teenagers who have ever been pregnant. The logistic regression script contains all the relevant logistic regressions. I applied sampling weights as specified by the DHS program, which provides comprehensive documentation for their data https://www.dhsprogram.com/data/.

Please note that the data for this project is not included in the repository, as it must be requested directly from the DHS survey. It is typically possible to obtain a dataset for legitimate research purposes, and they also offer some test datasets.

# Colombia DHS EHM - Teenage Pregnancy
This is a secondary analysis of the DHS Colombia data, focusing on the relationship between having emigrant household members and teenage pregnancy.

## Data Sources
- DHS Colombia Survey Data
- Household Recode (HR): COHR72FL.DTA
- Individual Women's Recode (IR): COIR72FL.DTA
- Household Member Recode (PR): COPR72FL.DTA

### Data Access
- Data obtained from the DHS Program
- Symlinked in the `data/raw/` directory
- Data can be requested from the DHS program.