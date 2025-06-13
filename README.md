# Live repository for the materials of the manuscript "Adolescents use social information more flexibly than adults during exploration"

## Overview of the repository

# BEFORE RUNNING CODE

The code is organized as a RStudio Project folder. This makes it easier to manage packages and relative paths. (see <https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects> for more info). It also means that to run the code as is, you should do it in RStudio, and load the project from the file "GBC" in the main folder.

# Folder structure

General content of each folder:

-   data: the full dataset for the experiments (data_social_all_participants_08-2024.csv)

-   scripts: scripts used to analyse the data. the main scripts are:

    -   scripts/behavioral_analyses: scripts for running the regression models
    -   scripts/model_analyses: scripts for running the computational modeling analyses

-   tables: demographics information, summary of model fitting output.

-   modelfits: output files of the regression models.

### Data origin

The adolescent dataset was collected in Dutch high-schools as part of the ERC-funded project Social Smart. Data was obtained using online experiments administered through computer tablets.

The adult dataset was collected on Prolific.com.

For any questions regarding the datasets, and the analyses conducted here, you can contact Andrea Gradassi at [andrea.gradassi\@gmail.com](mailto:andrea.gradassi@gmail.com){.email}.

If you don't have RStudio installed, you will have to change the paths to match those on your local machine.
