## EPAR Text Analysis Tools

The repository contains scripts to help the Research Assistants complete some aspects of the Portfolio Review process. 
Scripts are written in the R language.

Software Requirements:
R, RStudio installed on the machine.

Tools included in the repository include:
1. Basic Grant Info Scraper
2. Map Generator (Country of Intervention)
3. Graph Generator

Background: The scripts take a set of word documents (grant info) from a pre-specified directory, and outputs the relevant results (csv file, images etc.) in the 'current working directory'. RAs need to specify paths within the code by editing the script.
Comments in R are marked with a hashtag (#), i.e. meant for readability/understanding and not execution. 

Paths follow the standard directory tree structure format:  
"R:/Project/EPAR/Working Files/Sample Folder/"

After editing the paths in the script to relevant directories on the drive, the code needs to be 'Run' in RStudio. 
RStudio throws out red-colored errors in the console, and normal code execution is in blue/black. A red stop sign is diplayed on the top-right of the console while code is executed in the background.

For more information, have a look at the PPT for scraping [here](https://github.com/rohitgupta91/epar_text/blob/master/EPAR%20Portfolio%20Scraper.pptx).
