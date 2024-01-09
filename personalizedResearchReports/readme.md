# Project Description
In the UT Applied Psychology Lab, we provided personalized research reports to teachers each semester. This was previously a very time consuming 
process because each teacher's results were computed individually and there was no standardized report format. 
I simplified and standardized the creation of personalized teacher research reports using an R Markdown document 
in which teacher name could be input once to receive a teacher’s personalized report.

## File descriptions:
- **FinalDemoResearchReport.pdf:** Example of a research report given to a teacher. This document contains aggregate data for Spring 2023 rather than
  any individual teacher's data to maintain privacy. In practice, a teacher would receive a report containing their students' baseline results and
  either their students' changes after taking the course or the entire samples' changes if their sample was not large enough to detect changes
  with enough power.
- **ResearchReportS23.Rmd:** Document used to produce calculate personalized reports. Someone only needs to change the teacher's name (and potentially the
  file names of cleaned datasets if it's a new semester) at the top of the document to create a personalized report.
- **UnformattedDemoResearchReportS23.pdf:** Direct output from knitting the Rmd file. This was pasted into a Word document and formatted nicer. Additional
  explanation of the results may have been necessary if a teacher's sample was used. Ideally the formatting would have been done directly in R markdown,
  but I didn't have time to learn how to do this.
- **data_cleaning_functions.R:** used to clean the data previously. Just included for redundancy in case someone realized they needed to perform additional cleaning at this stage in the process.
- **research_report_functions.R:** Functions used to summarize baseline results and changes across time, including standardized figures to display changes
  across time. Since datasets were similar across semesters, these functions could be used to analyze semesterly data quicker.
