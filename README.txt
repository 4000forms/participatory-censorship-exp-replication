## ----------------------------------------------------------------------------------------------------------
## 	Replication Files: 
##	"Participatory Censorship in Authoritarian Regimes"
##	Author: Tony Zirui Yang
##	Date:  October 18, 2024
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## 	General Information:
## ----------------------------------------------------------------------------------------------------------
##
## 	1. Computational Requirements: 
##
##	   There are no specific computational requirements to run the code. 
##	   Any standard personal computer (multi-core) with R installed will 
##         be able to reproduce the results presented in the paper using the
##	   replication codes.
##	   
## 	2. R version and Operating System:
##	   
##	   R version 4.4.1 (2024-06-14)
##         Operating System: macOS 14.6.1 (23G93)
##	   
## 	3. Required R-packages: 
##		
##	   Listed in each file
##
## 	4. Code: 
##	   
## 	   * Code that reproduces the main results (Figures, 
##	     Tables, and numbers reported) in the paper:
##
##		- "./01_Main_Manuscript.R"
##
## 	   * Code that reproduces all results presented in the Appendix:
##
##		- "./02_Online_Appendix.R"
##
## 	6. Data: 
##	   
## 	   * Data for Study 1 (Observational Study):
##
##		- "./Study1.csv"
##
## 	   * Data for Study 2 (Experimental Study):
##
##		- "./Study2.csv"
##
##
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## 	Codebook:
## ----------------------------------------------------------------------------------------------------------
##
## 	1. "./Study1.csv"
##
##	ObsNo: Assigned number for each participant. 
##	Female: 0 - Male; 1 - Female.
##	Age_Group: 1 - <= 19 year-old; 2 - 20-29 year-old; 3 - 30-39 year-old;
##		   4 - 40-49 year-old; 5 - >= 50 year-old.
##	Education: 1 - <= Junior High School; 2 - Senior High School; 3 - 3-year College; 
##		   4 - 4-year College; 5 - >= Postgraduate.
## 	Region:    1 - East; 2 - South; 3 - North; 4 - Central; 5 - Northeast; 6 - West.
##	Political_Interest:  Six-point Scale; 1 - little interest; 6 - strong interest.
##	Econ_Ideology:  Five-point Likert Scale; 
##		   1 - strongly pro-market; 5 - strongly pro-state.
##	Pol_Ideology:  Five-point Likert Scale; 
##		   1 - strongly pro-democracy; 5 - strongly pro-regime.
##	Party_Member: 0 - Not a CCP member; 1 - CCP member.
##	Urban:     0 - Rural; 1 - Urban.
##	Participation:  Five-point scale of participation in censorship;
##		   1 - Never participated; 2 - Once or twice only;
##		   3 - Once every few months; 4 - Once per month; 5 - Multiple per month.
##	Participate_Binary:  Binary measure of the previous variable;
##		   0 - Never participated; 1 - Participated.
##	Participate_Pol:  Five-point scale of participation in political censorship;
##		   1 - Never participated; 2 - Once or twice only;
##		   3 - Once every few months; 4 - Once per month; 5 - Multiple per month.
##	Participate_Ent:  Five-point scale of participation in non-political censorship;
##		   1 - Never participated; 2 - Once or twice only;
##		   3 - Once every few months; 4 - Once per month; 5 - Multiple per month.
##	Participate_Ina:  Five-point scale of participation in censorship of inappropriate content;
##		   1 - Never participated; 2 - Once or twice only;
##		   3 - Once every few months; 4 - Once per month; 5 - Multiple per month.
##	Censor_Support:  Five-point Likert Scale;
##		   1 - strongly disagree with censorship;
##		   5 - strongly agree with censorship.
##	Censor_Pol_Support: Five-point Likert Scale;
##		   1 - strongly disagree with censorship of political content; 
##		   5 - strongly agree with censorship of political content. 
##	Censor_Ent_Support: Five-point Likert Scale;
##		   1 - strongly disagree with censorship of non-political content; 
##       	   5 - strongly agree with censorship of non-political content.
##	Responsibility_Gov:  11-point Scale;
##		   0 - the government is NOT responsible for censorship;
##		   10 - the government is fully responsible for censorship;
##	Gov_Assess:  Five-point Likert Scale;
##		   1 - strongly disagree that the central government works for the people;
##		   5 - strongly agree that the central government works for the people.
##	calibWeight: weights for each observation.##
##
##
## 	2. "./Study2.csv"
##
##	ObsNo: Assigned number for each participant. 
##	Group:     0 - Control Group; 1 - Treatment Group 1; 2 - Treatment Group 2.
##	Female:    0 - Male; 1 - Female.
##	Age_Group: 1 - <= 19 year-old; 2 - 20-29 year-old; 3 - 30-39 year-old;
##		   4 - 40-49 year-old; 5 - >= 50 year-old.
##	Education: 1 - <= Junior High School; 2 - Senior High School;
##		   3 - 3-year College; 4 - >= 4-year College.
## 	Region:    1 - East; 2 - South; 3 - North; 4 - Central; 5 - Northeast; 6 - West.
##	Ideology:  Five-point Likert Scale; 
##		   1 - strongly pro-market; 5 - strongly pro-state.
##	PartyMember: 0 - Not a CCP member; 1 - CCP member.
##	Nationalism:  Five-point Likert Scale; 
##		   1 - Not nationalistic at all; 5 - strongly nationalistic.
##	Pol_Interest:  Five-point Scale; 1 - little interest; 5 - strong interest.
##	Social_Media:  Five-point Scale;
##		   1 - rarely use social media; 5 - use social media a lot.
##	Foreign:  Five-point Scale;
##		   0 - no foreign connection at all; 5 - many foreign connections.
##	ReportClick: 0 - did not click any report in the experiment;
##		     1 - clicked report in the experiment.
##	ReportAnti:  0 - did not click report of any anti-regime post in the experiment;
##		     1 - clicked report of anti-regime post in the experiment.
##	ReportPro:   0 - did not click report of any pro-regime post in the experiment;
##		     1 - clicked report of pro-regime post in the experiment.
##	ReportClickNumber: number of click(s) of report in the experiment;
##	Censor_Support:  Five-point Likert Scale;
##		   1 - strongly disagree with censorship;
##		   5 - strongly agree with censorship.
##	Censor_Pol: Five-point Likert Scale;
##		   1 - strongly disagree with censorship of political content; 
##		   5 - strongly agree with censorship of political content. 
##	Censor_Ent: Five-point Likert Scale;
##		   1 - strongly disagree with censorship of non-political content; 
##       	   5 - strongly agree with censorship of non-political content.
##	Regime_Satisfaction:  Five-point Likert Scale;
##		   1 - strongly dissatisfied with China;
##		   5 - strongly satisfied with China.
##	Regime_Assessment:  Five-point Likert Scale;
##		   1 - strongly disagree that the government works for the people;
##		   5 - strongly agree that the government works for the people.
##	Regime_Trust:  Five-point Likert Scale;
##		   1 - no trust in the government at all;
##		   5 - high trust in the government.
##
## ----------------------------------------------------------------------------------------------------------
                       


















