## EPAR Text Analysis Tools

The repository contains scripts to help the Reasearch Assistants in completing some aspects of the Portfolio Review process. 
Scripts are written in the language R.

Software Requirements:
R, RStudio installed on the machine.

Tools included in the repository include:
1. Basic Grant Info Scraper
2. Map Generator (Country of Intervention)
3. Graph Generator

Background: The scripts take a set of word documents (with grant info) from a pre-specified directory, and outputs the result in the 'current working directory'. RAs need to specify paths within the code by editing the script.
Comments in R are marked with a hashtag (#), i.e. meant for readability and not executed. 
Comments within the code are meant to facilitate understanding of the operation.

Paths follow the standard directory tree structure format:  
"R:/Project/EPAR/Working Files/Sample Folder/"

After editing the paths in the script to relevant directories, the code needs to be 'Run' in RStudio. 
RStudio throws out errors in the console marked with 'red', and normal code execution is in 'blue'. A red stop sign is diplayed on the top-right of the console while code is being executed in the background.

For more information, have a look at the PPT for scraping [here](https://github.com/rohitgupta91/epar_text/blob/master/EPAR%20Portfolio%20Scraper.pptx).
