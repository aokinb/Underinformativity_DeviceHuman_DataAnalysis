# Underinformativity_DeviceHuman_DataAnalysis

This repository is organized into 3 folders: 'Code', 'Data Frames', and 'Loop and Merge'. The contents of each of these folders is described below:

1. Code:

- The 'Code' folder contains 2 files: 'Underinformativity_DeviceHuman_Analysis.R' and 'Underinformativity_DeviceHuman_Processing.R'.
- The 'Processing' file loads the csv file downloaded from Qualtrics and cleans the data set.
- The 'Analysis' file starts with the cleaned data set from the 'Processing' file. This file contains the code used for the data visualization and the statistical modelling.
- These 2 files are meant to be run sequentially (the 'Processing' file should be run first and the 'Analysis' file should be run second).

2. Data Frames:

- The 'Data Frames' folder contains 2 files: 'Underinformativeness TopDownCongruencyGradience (Winter 2022)_ March 3, 2022_00.46.csv' and 'Underinformativity_DeviceHuman.csv'.
- The first file is the original csv file downloaded from Qualtrics.
- The second file ('Underinformativity_DeviceHuman.csv') is the cleaned data set created from the 'Processing' R code (see above).

3. Loop and Merge:

- The 'Loop and Merge' folder contains 6 files. These files were used for counterbalancing purposes in Qualtrics.
- These files are needed in the 'Processing' R code. 

- Note that the R code loads these files from the working directory of the first author. 
- Thus, in order to run all of the scripts, the user will have to determine where the files are located on their own computer and change the code accordingly. 
