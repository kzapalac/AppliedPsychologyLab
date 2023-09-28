# Project description
When I worked in the UT Applied Psychology Lab, valuable time was spent cleaning semesterly datasets by hand and quality data was being lost, 
so I set out to automate and upgrade this process. I established rules for removing duplicates, matched pre- and post-course surveys, and created
reusable, well-documented code.

## File descriptions:
- DataCleaningReportS23.Rmd: Semesterly datasets can be cleaned using this document by making minor changes to the code, such as specifying
  new file names and additional variables included in the dataset (variables are mostly consistent each semester).
- DataCleaningReportS23.pdf: Output from knitting the Rmd file. Reports on the intermediate steps in the data cleaning process and can be used
  to check the data was cleaned properly. 
- data_cleaning_functions.R: Functions I created specifically for use with the semesterly UT Applied Psychology lab datasets.
  There were many similarities between semesterly datasets, so reusable functions expedited future research processes, such as removing duplicates and
  matching pre- and post-course surveys.
