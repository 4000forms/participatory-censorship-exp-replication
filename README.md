# Replicating and reviewing “Participatory Censorship in Authoritarian Regimes" experiment 1 by Tony Yang, from the Comparative Political Studies (CPS) Dataverse.

**Author:** Carmen Wu
**Date:** August 2026

## Files in this Repository

- ‘Markdown.Rmd’ - Full markdown of my replication process with a summary of the authors’ findings and measurement, replicating OLS regressions, diagnosing the main model, and evaluating robustness while extending their analysis. 
- ‘Markdown.html’ - Knitted html file. *Recommend download directly*.
- ‘Data/’ - Folder containing plots created.
- ‘README.md’ - This README summary. See more in Markdown.
- ‘01_Main_Manuscript.R’ - The authors’ original code script for the main analysis.
- ‘02_Online_Appendix.R’ - The authors’ original code script for the appendix.

## Brief outline

The author seeks to investigate the prevalence of public participation in the censorship process in determining levels of popular support for the censorship apparatus in authoritarian regimes using an original survey, hypothesising that ordinary users who frequently report and flag political and non-political online content they disapprove of are likely to have higher levels of support for government censorship.
I will be replicating the key findings and performing diagnostics. See more in Markdown.

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

## Conclusion section

Regressing Censor_Support on Participation controlling for the demographic and predisposition covariates returned a significant positive coefficient (**B = 0.084, p = 0.0028, Adjusted R^2 = 0.106**), showing the positive association between additional levels of public participation and unit increase in support for censorship hypothesized by the author. 

Sensitivity analysis using Econ_Ideology as the benchmark covariate confirmed that the significant result was largely robust to OVB (**RV = 0.0879**) under a realistic hypothetical confounding scenario, while stress testing revealed that the significance will be overturned by unobserved confounders 3x as strong as Econ_Ideology (**Adjusted CI = [-0.0056, 0.0977]**).

No independent variables are found to be multicollinear (**All VIFs < 1.5**).

The model passed Ramsey's error specification test (**RESET p = 0.106**), confirming that the linearity assumption was not violated; The curved loess line observed in the fitted-values/residuals scatter plot was ascribed to data sparsity at the extremes, and gaps created by the *discrete* nature of our ordinal/binary variables. 

A density plot with normal curve for comparison revealed that the residual distribution was bimodal - with residuals clustered at two peaks, while the quantile-quantile plot showed sizable tails creating a visible S-shape indicative of leptokurtosis, although I avoided declaring this as a *statistical* non-normal error due to the examinations being purely ocular. 

There was no statistical evidence that the model violated the constant variance assumption (**Breusch-Pagan Chisq = 1.057, p = 0.304**), and the spread level plot trend-lines showed no severe, systematic heteroscedastic pattern; However the funnel shape - collectively with the speculated abnormalities - showed a notable variance problem which I attempted to salvage using type 3 heteroscedasticity-consistent standard errors. Upon introducing HC3s, however, the significance of our estimated coefficient was killed (**p = 0.0729**), revealing the main model to be very sensitive to standard error type.

Large dfbetas exceeding the recommended threshold were located and flagged (**max: case950 -0.676**); although the estimated Participation coefficient changed only marginally once they were removed (**B = 0.0855**). Similarly an influence plot revealed large hat-values stretching the scale (**max: case994 0.126**) and huge studentised residuals (**case404 5.522, case494 4.071, case507 4.902, case1097 4.785**), despite the estimated coefficient decreasing only by a margin after the influential observations were omitted (**B = 0.0794**). This sign of remarkable robustness was ascribed to the use of calibration weights, which diluted the influence of over-sampled observations with high raw leverage. Upon closer examination of the survey re-weighting and replicating table B1, it was confirmed that the post-stratified sample distribution largely met the calibration benchmarks taken from the China internet census as the author intended.

As part of an extended exploratory analysis I found a significant positive age moderation (**B = 0.0706, p = 0.0170**), indicating that the effect of Participation in the censorship process on support was greater among older respondents compared to younger ones - a pattern left to the exploration of future research. Whilst the positive association remained under the use of HC3s, significance was similarly killed, showing that the moderated model too was fragile.

Regressing the selected mediator Responsibility_Gov on Participation controlling for all demographic and predisposition covariates first returned a significant negative association between higher levels of censorship participation and perceived government responsibility for censorship (**B = -0.112, p = 0.0225**). 
Conducting causal mediation analysis using the author's original specification (Quasi-Bayesian, assumed normal sample distribution by the mediation package default) returned a significant negative indirect effect (**ACME = -0.00631, p = 0.040, CI = [-0.014, -0.0003]**), while adopting a more rigorous approach providing wider, more precise confidence intervals (Nonparametric Bootstrap, no distributional assumptions, CIs derived from 5000 simulations) revealed that the re-estimated indirect effect was no longer significant (**ACME = -0.00617, p = 0.286, CI = [-0.0191, 0.0058]**), showing that the mediation effect was not robust to CI method - its significance wholly depending on an imprecise distributional assumption. 

Overall our replicated analysis confirm the author's crucial findings and show that the main OLS regression was **robust**.
