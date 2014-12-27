download-nhanes
===============

R function to download and format NHANES data. The user can specify desired data by year and by section 
(demog, dietary, exam, lab, questionnaire).

When complete, the function will download the data from the CDC FTP site, merge it, compute multiyear
weights, and return a weighted survey object ready for analysis.
