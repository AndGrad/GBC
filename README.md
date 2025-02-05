# Live repository for the materials of the article "Adolescents use social information more than adults when searching for hidden rewards"

## Overview of the repository

# BEFORE RUNNING CODE
The code is organized as a RStudio Project folder. This makes it easier to manage packages and relative paths. (see https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects for more info). It also means that to run the code as is, you should do it
in RStudio, and load the project from the file "GBC" in the main folder.

# How to interact with this repository

This repository was structured to maximize modularity and ease of access
for different use cases. There is some redundancy in the code, but it
should be easier to find specific things that you are looking for.

For any questions, you can reach out to: andrea.gradassi@gmail.com.

General content of each folder:

- data: the full dataset for the experiments, both in .csv and .rda format (data/full_dataset.csv, data/full_dataset.rda). subfolders contain convenience dataset derived from the full_data file, used for plots or other analysis

- scripts: scripts used to analyse the data.

- tables: demographics information, summary of model fitting output.

- modelfits: output files of the regression models. 

### I want to double check the whole analysis pipeline.

You should go into the folder scripts/analyses/

**Nice plots! how do I make them?**

Each figure has its own associated file, they are in the folder
scripts/plots.

### Data

The adolescent dataset was collected in Dutch high-schools as part of the ERC-funded project Social Smart.
Data was obtained using online experiments administered through computer tablets. 

The adult dataset was collected on Prolific.com.

For any questions regarding the datasets, and analyses conducted here, you can contact Andrea Gradassi at andrea.gradassi@gmail.com.

If you don't have RStudio installed, you will have to change the paths to match those on your local machine.

