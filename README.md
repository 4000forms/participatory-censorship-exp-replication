# Replicating and reviewing “Participatory Censorship in Authoritarian Regimes" experiment 1 by Tony Yang, from the Comparative Political Studies (CPS) Dataverse.

**Author:** Carmen Wu
**Date:** August 2026

## Files in this Repository
- ‘Markdown.Rmd’ - Full markdown of my replication process with a summary of the authors’ findings and measurement, replicating OLS regressions, diagnosing the main model, and evaluating robustness while extending their analysis. 
- ‘Markdown.html’ - Knitted html file.
- ‘README.md’ - This README summary. See more in Markdown.
- ‘01_Main_Manuscript.R’ - The authors’ original code script for the main analysis.
- ‘02_Online_Appendix.R’ - The authors’ original code script for the appendix.

## Brief overview
The author seeks to investigate the prevalence of public participation in the censorship process in determining levels of popular support for the censorship apparatus in authoritarian regimes using an original survey, hypothesising that ordinary users who frequently report and flag political and non-political online content they disapprove of are likely to have higher levels of support for government censorship.
I will be replicating the key findings and performing diagnostics. See more in Markdown.


## Brief outline
- Setup and introduction
- Summarising the study and main findings
- 6 OLS regression models, reporting coefficients
- Sensitivity analysis
- Checking for violation of OLS assumptions
- Introducing robust estimators
- Checking calibration weighting, replicating table B1
- Investigating age moderation
- Causal mediation analysis
- Conclusion on overall robustness
- Session information and package versions
