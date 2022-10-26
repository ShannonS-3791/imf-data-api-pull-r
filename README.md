# Download Metadata and Data from the International Monetary Fund's (IMF) Data API

<strong>Description</strong>: Download metadata and data for selected datasets, indicators, and countries from the IMF's data API <http://data.imf.org/>.<br>
<strong>Date</strong>: 2022-10-26<br>
<strong>License</strong>: GPL (>= 3)<br>

### Acknowledgments and Notes
Some of this code is derived from the imfr package. Please see the full package at https://github.com/christophergandrud/imfr, especially to explore IMF data (i.e. pull list of databases, available indicators, etc.)

The code here, however, has been modified to expand the use of metadata especially and also to address some issues, such as the missing 'type' when pulling the PCTOT data using the imfr package.

Please note, however, that this code has primarily been tested on the CPI, PCPS, AFFREO, and PCTOT datasets, so it may encounter issues with other datasets

## Purpose

This code aims to allow users to select the datasets + indicators + countries they wish to pull from the IMF's API. After pulling this, users can join the 'Compact' data (which is simply the indicator code, country iso2code, and observation value) with information about the the indicator and database.

It is not quite as 'user-ready' as a typical r package, however aims to save some time for users who are ready to pull IMF data but have not had the time to explore it.

## How to Use this Code

### 1. Create Table of Desired Indicators
An example is saved in this repository as "Final IMF Indicators.csv". Users should create a similar file specifying the dataset and indicator codes they wish to pull. Save the file under the same name, or alternatively update the 'Selected IMF Processing.r' code to read in the new file as imf_Indicators_Full

### 2. Store functions
Open the 'IMF API Functions.r' file and run it in order to store the various functions in the environment

### 3. Run functions to save outputs as dataframes

in the 'Selected Processing' file, run the three steps of ```df <- getIMF...``` , which will run these functions and save 3 different dataframe: dataset codes with their names, metadata for indicators, and indicator data.

### 4. Join and Process Data
Join the three files and process so the data is clean + in working order. I plan on adding some of my illustrative steps at a later date.
