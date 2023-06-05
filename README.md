# Data cleaning workflows

This work was done as a collaboration between [Atlas of Living Australia](https://www.ala.org.au/) (ALA) and [Invertebrates Australia](https://invertebratesaustralia.org/) (IA) and was funded by the Australia Research Data Commons as part of the [Bushfire Data Challenges program](https://ardc.edu.au/program/bushfire-data-challenges/).

The goal of this collaboration was to streamline data retrieval and data cleaning of biodiversity data from data infrastructures, like the ALA.

The upgraded workflows can be found in `workflows/`

The original code from IA's project can be found at [this repository](https://github.com/payalbal/nesp_bugs/) and in `original_scripts/`

The key data cleaning steps and files in this repo:

1.	Clean AFD checklist (i.e. species name list): 
  *	[Remove improper names](https://github.com/AtlasOfLivingAustralia/data_cleaning_workflows/blob/develop/functions/remove_improper_names_v2.R)
  * Remove invasive species (species on GRIIS list)
  * Remove marine species (using World Register of Marine Species, WoRMS)
  * Identifying duplicates in AFD
  
1.1. [Excluding invasive species using GRIIS v1.6](https://github.com/AtlasOfLivingAustralia/data_cleaning_workflows/blob/develop/workflow/griis.Rmd)
  
1.2. [Querying WoRMS database + excluding marine species](https://github.com/AtlasOfLivingAustralia/data_cleaning_workflows/blob/develop/workflow/worrms.Rmd)
  * Query WoRMS API in console
  * Query WoRMS API as RStudio background job
  * Marine species exclusion

2.	[Download and clean ALA data](https://github.com/AtlasOfLivingAustralia/data_cleaning_workflows/blob/develop/workflow/download_ALA_data.Rmd)
  * Configure `{galah}` settings
  * Retrieve counts of occurrences 
  * Filter data by assertions
  * Download assertion data
  * Download occurrence records using RStudio background jobs
      * Download as .parquets using {purrr}
  * Clean downloaded ALA data
      * Removing records where there is missing data in coordinate fields
      * Remove duplicates in coordinate fields
      * Identify and remove coordinate values that are in the ocean

## ALA team

- Margot Schneider - Project Officer
- Dr. Fonti Kar - Data Analyst
- Dr. Martin Westgate - Project Lead

## IA team

- Dr. Payal Bal
- Dr. Jess Marsh
- Hannah Smart

  
