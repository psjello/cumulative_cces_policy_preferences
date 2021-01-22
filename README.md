# Cumulative CCES Policy Preferences

This repository contains files used to build the **Cumulative CCES Policy Preferences** data set, along with its accompanying guide.  The Cumulative CCES Policy Preferences data set compiles various policy preference question items from Cooperative Congressional Election Study (CCES) respondents over time.  This represents an effort to track, rename, recode, and append together responses to 43 policy preference question items from individual CCES survey data sets ranging from 2006 to 2019. 


# Build instructions

To build the Cumulative CCES Policy Preferences data set, first download each individual [CCES Common Content data set](https://cces.gov.harvard.edu/) from 2006 to 2019.  Additionally, renaming and relocating each data set such that their path and file name reads `20__/cces__`, is also required, with __ being the last 2 digits of the survey year that prefixes the respective file format suffix of .dta, .sav or .tab.

After this initial downloading, recoding, renaming and appending policy preference question items from each individual CCES survey revolves around the crosswalk `/output/preferences-crosswalk_wide-withwording.csv`, and coding found in the script `/code/cleaning_merging.R`.  The crosswalk attaches a commonalized variable name for a question item to its respective unique variable name found in each individual CCES survey, along with the unique question item wording found in that survey year.  (Building of this crosswalk uses files in `/input` and coding in `/code/widelong.R`.)

The cleaning, merging and appending question item responses within the script `/code/cleaning_merging.R` follows this process:

* Load an individual CCES Common Content data set;
* Select the policy preference variables by their unique, year-specific variable names, listed in the column `y20__` found in the `/output/preferences-crosswalk_wide-withwording.csv` crosswalk;
* Rename these variables as the commonalized variable names listed in the crosswalk column `q_code_common`;
* Attach a variable named `year` containing the respective survey year;
* Append this subsetted and renamed data to previous years.
