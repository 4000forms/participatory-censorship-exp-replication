####################################################
# Participatory Censorship in Authoritarian Regimes
# Replication Files: Main Manuscript
# Author: Tony Zirui Yang
# Date: October 18, 2024
####################################################


## Clear the working environment
rm(list=ls())

## Load packages
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(mediation)
library(gridExtra)
library(ivreg)

## Set Working Directory
setwd("~/Desktop/CPS_Replications")

## Load Data
Study1 <- read_csv("Study1.csv")
Study2 <- read_csv("Study2.csv")


#########################################################################
### Figure 1: Distribution of Self-Report Participation in Censorship
#########################################################################

# Full Sample
Freq <- data.frame("Participation" = c(1:5),
                   "Group" = rep("Full_Sample",5),
                   "n" = as.vector(xtabs(calibWeight ~ Participation, Study1)),
                   "Proportion" = as.vector(xtabs(calibWeight ~ Participation, Study1)/
                                              sum(xtabs(calibWeight ~ Participation, Study1))))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participation, Age_Group, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Age_Group = recode(Age_Group, `1` = "Age:<20", `2` = "Age:20-29",
                            `3` = "Age:30-39", `4` = "Age:>40"))%>%
  rename("Group" = "Age_Group")

# By Gender
Freq_Female <- Study1 %>%
  count(Participation, Female, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Female = recode(Female, `0` = "Male", `1` = "Female"))%>%
  rename("Group" = "Female")

# By Education Level
Freq_Edu <- Study1 %>%
  mutate(Education = recode(Education, `5` = 4))%>%
  count(Participation, Education, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Education = recode(Education, `1` = "Junior High", `2` = "Senior High",
                            `3` = "3Y College", `4` = "4Y College"))%>%
  rename("Group" = "Education")

# Combine
Freq_All <- rbind(Freq, Freq_Female, Freq_Age, Freq_Edu)
Freq_All <- Freq_All %>%
  mutate(Participation = recode(Participation, `1` = "1. Never",
                                `2` = "2. Once or twice only",
                                `3` = "3. Once every few months",
                                `4` = "4. Once per month",
                                `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participation, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) + #,
  # "", "Liberal", "Moderate", "Conservative")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_01.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches



#########################################################################################
## On page 10, I claim that:
## "The vast majority of participating respondents (around 90%) reported inappropriate
## content online, including pornography and vulgar language. A significant proportion
## of them also reported political content, with about 50% of participating respondents
## (or 25% of all respondents) reporting having flagged political content."
## "around one-third of participating respondents reported censorship of entertainment
## and cultural content."
#########################################################################################

# Percentage of the participanting respondents reported inappropriate content
paste(round(nrow(Study1[Study1$Participate_Binary == 1 & Study1$Participate_Ina > 1,])
            /nrow(Study1[Study1$Participate_Binary == 1,]),3)*100,"%", sep ="")
# Percentage of the participanting respondents reported political content
paste(round(nrow(Study1[Study1$Participate_Binary == 1 & Study1$Participate_Pol > 1,])
            /nrow(Study1[Study1$Participate_Binary == 1,]),3)*100,"%", sep ="")
# Percentage of the participanting respondents reported entertainment content
paste(round(nrow(Study1[Study1$Participate_Binary == 1 & Study1$Participate_Ent > 1,])
            /nrow(Study1[Study1$Participate_Binary == 1,]),3)*100,"%", sep ="")


############################################################################################
### Table 1: Correlation between Participation in Censorship and Support for Censorship
############################################################################################


## OLS Regressions
## Support for Censorship
lm.all.1 <- lm(Censor_Support ~ Participation + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.all.2 <- lm(Censor_Support ~ Participation + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Political Content
lm.pol.1 <- lm(Censor_Pol_Support ~ Participation + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.pol.2 <- lm(Censor_Pol_Support ~ Participation + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Non-Political Content
lm.non.1 <- lm(Censor_Ent_Support ~ Participation + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.non.2 <- lm(Censor_Ent_Support ~ Participation + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

stargazer(lm.all.1, lm.all.2, lm.pol.1, lm.pol.2, lm.non.1, lm.non.2,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Participation", "Constant"))


############################################################################################
### Table 2: Correlation between Specific Types of Participation and Support for Censorship
############################################################################################

## Participation of Political Censorship on General Support for Censorship
lm.pol.all <- lm(Censor_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)
## Participation of Political Censorship on Support for Political Censorship
lm.pol.pol <- lm(Censor_Pol_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)
## Participation of Political Censorship on Support for Non-Political Censorship
lm.pol.non <- lm(Censor_Ent_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)
## Participation of Non-Political Censorship on General Support for Censorship
lm.non.all <- lm(Censor_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)
## Participation of Non-Political Censorship on Support for Political Censorship
lm.non.pol <- lm(Censor_Pol_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)
## Participation of Non-Political Censorship on Support for Non-Political Censorship
lm.non.non <- lm(Censor_Ent_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                   Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                 data = Study1, weights = calibWeight)

stargazer(lm.pol.all, lm.non.all, lm.pol.pol, lm.non.pol, lm.pol.non, lm.non.non,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Participate_Pol","Participate_Ent", "Constant"))



#########################################################################################
## On page 15, I claim that:
## "Using OLS regression models with all relevant covariates and adjusted by sample 
## weights, I find higher levels of participation in censorship are indeed significantly 
## and negatively associated with perceived government responsibility (β = −0.112,
##  p = 0.023). Similarly, causal mediation analysis demonstrates that perceived
##  government responsibility has a significant and negative causal mediation effect 
## on the relationship between participation and support for censorship."
#########################################################################################

## Government Responsibility OLS
lm.res <- lm(Responsibility_Gov ~ Participation + Female + Age_Group + Education + Urban +
               Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
             data = Study1, weights = calibWeight)
# Extract the summary and focus on "Participation"
round(summary(lm.res)$coefficients["Participation", ],3)

## Government Responsibility Causal Mediation
mediation <- na.omit(Study1[,c("Responsibility_Gov","Censor_Support","Participation","calibWeight")])
set.seed(1013)
# Fit the mediation model
med.fit <- lm(Responsibility_Gov ~ Participation,
              data = mediation, weights = calibWeight)
y.fit <- lm(Censor_Support ~ Responsibility_Gov + Participation,
            data = mediation, weights = calibWeight)
med.out <- mediate(med.fit, y.fit, treat = "Participation", mediator = "Responsibility_Gov")

# Extract and print the ACME (Average Causal Mediation Effect)
summary(med.out)



#######################################################################################
## On page 15, I claim that:
## "Using OLS models, I indeed find no statistically significant relationships
## between participation in censorship and regime support (β = −0.012, p = 0.632)."
#######################################################################################

## Regime Support: Assessment of Government
lm.assess <- lm(Gov_Assess ~ Participation + Female + Age_Group + Education + Urban +
                  Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
                data = Study1, weights = calibWeight)
round(summary(lm.assess)$coefficients["Participation", ],3)




#########################################################################
### Figure 2: Difference in Means of the Outcome Variables
#########################################################################

# Difference in mean in censorship support between control and treatment 1
lm_cen_01 <- lm(Censor_Support ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,1)),])
# Difference in mean in censorship support between control and treatment 2
lm_cen_02 <- lm(Censor_Support ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,2)),])
# Difference in mean in political censorship support between control and treatment 1
lm_pol_01 <- lm(Censor_Pol ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,1)),])
# Difference in mean in political censorship support between control and treatment 2
lm_pol_02 <- lm(Censor_Pol ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,2)),])
# Difference in mean in non-political censorship support between control and treatment 1
lm_non_01 <- lm(Censor_Ent ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,1)),])
# Difference in mean in non-political censorship support between control and treatment 2
lm_non_02 <- lm(Censor_Ent ~ as.factor(Group), data = Study2[(Study2$Group %in% c(0,2)),])

# Create a dataframe for Figure 2 
Figure2 <- data.frame(Group = rep(c("T1","T2"),3),
                      Estimates = c(coef(summary(lm_cen_01))["as.factor(Group)1","Estimate"],
                                    coef(summary(lm_cen_02))["as.factor(Group)2","Estimate"],
                                    coef(summary(lm_pol_01))["as.factor(Group)1","Estimate"],
                                    coef(summary(lm_pol_02))["as.factor(Group)2","Estimate"],
                                    coef(summary(lm_non_01))["as.factor(Group)1","Estimate"],
                                    coef(summary(lm_non_02))["as.factor(Group)2","Estimate"]),
                      Std_Error = c(coef(summary(lm_cen_01))["as.factor(Group)1","Std. Error"],
                                    coef(summary(lm_cen_02))["as.factor(Group)2","Std. Error"],
                                    coef(summary(lm_pol_01))["as.factor(Group)1","Std. Error"],
                                    coef(summary(lm_pol_02))["as.factor(Group)2","Std. Error"],
                                    coef(summary(lm_non_01))["as.factor(Group)1","Std. Error"],
                                    coef(summary(lm_non_02))["as.factor(Group)2","Std. Error"]))
Figure2$min <- Figure2$Estimates - (qnorm(0.975) * Figure2$Std_Error)
Figure2$max <- Figure2$Estimates + (qnorm(0.975) * Figure2$Std_Error)

pd = position_dodge2(width = 0.3, reverse = TRUE)
# Panel 1 of Figure 2: Support for Censorship
p2_1 <- ggplot(Figure2[1:2,], aes(x=Group, y = Estimates)) + 
  # Point estimates
  geom_point(aes(x = Group, 
                 y = Estimates),position = pd, size = 2.5, show.legend = FALSE) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + # Line at 0
  # Confidence intervals
  geom_linerange(aes(x = Group, 
                     ymin = min,
                     ymax = max),
                 lwd = 1/2, show.legend = FALSE) + 
  ggtitle("\nSupport for Censorship") +
  labs(y="Difference in Means", x="") +  # Labels
  theme_bw() + 
  scale_y_continuous(limits = c(-0.05,0.25)) +
  scale_x_discrete() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size = 12)) # Nicer theme 

# Panel 2 of Figure 2: Support for Political Censorship
p2_2 <- ggplot(Figure2[3:4,], aes(x=Group, y = Estimates)) + 
  # Point estimates
  geom_point(aes(x = Group, 
                 y = Estimates),position = pd, size = 2.5, show.legend = FALSE) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + # Line at 0
  # Confidence intervals
  geom_linerange(aes(x = Group, 
                     ymin = min,
                     ymax = max),
                 lwd = 1/2, show.legend = FALSE) + 
  ggtitle("Support for Censorship\n of Political Content") +
  labs(y="Difference in Means", x="") +  # Labels
  theme_bw() + 
  scale_y_continuous(limits = c(-0.05,0.25)) +
  scale_x_discrete() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size = 12)) # Nicer theme 

# Panel 3 of Figure 2: Support for Non-Political Censorship
p2_3 <- ggplot(Figure2[5:6,], aes(x=Group, y = Estimates)) + 
  # Point estimates
  geom_point(aes(x = Group, 
                 y = Estimates),position = pd, size = 2.5, show.legend = FALSE) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + # Line at 0
  # Confidence intervals
  geom_linerange(aes(x = Group, 
                     ymin = min,
                     ymax = max),
                 lwd = 1/2, show.legend = FALSE) + 
  ggtitle("Support for Censorship\n of Non-Political Content") +
  labs(y="Difference in Means", x="") +  # Labels
  theme_bw() + 
  scale_y_continuous(limits = c(-0.05,0.25)) +
  scale_x_discrete() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size = 12)) # Nicer theme 

# Combining the 3 panels
p2 <- grid.arrange(p2_1, p2_2, p2_3, ncol=3)
# Save Figure 2
ggsave(p2, file = "Figures_Paper/Figure_02.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 3.1) # The height of the plot in inches



##########################################################################################
## On page 19, I claim that:
## "In treatment group 1, 43% of the respondents clicked the “Report” buttons on the
## simulated social media page, while 64% of the respondents in treatment group 2 did so."
##########################################################################################

# Percentage of respondents in treatment group 1 who clicked report
sprintf("%.0f%%", 100 * mean(Study2$ReportClick[Study2$Group == 1]))

# Percentage of respondents in treatment group 2 who clicked report
sprintf("%.0f%%", 100 * mean(Study2$ReportClick[Study2$Group == 2]))


##########################################################################################
## On page 19, I claim that:
## "respondents who were given the opportunity to report simulated posts expressed
## higher support for censorship in general (β = 0.067, p = 0.065) and censorship of 
## political content (β = 0.066, p = 0.076)"
##########################################################################################

# Support for censorship in general
round(summary(lm_cen_01)$coef[2,1],3)
round(summary(lm_cen_01)$coef[2,4],3)

# Support for censorship of political content
round(summary(lm_pol_01)$coef[2,1],3)
round(summary(lm_pol_01)$coef[2,4],3)


##########################################################################################
## On page 20, I claim that:
## "respondents who were given the option to report and encouraged to flag online content
## showed significantly higher levels of support for government censorship in general
## (β = 0.153, p < 0.001), as well as support for censorship of political content
## (β = 0.134, p < 0.001) and non-political content (β = 0.090, p = 0.015) in particular."
##########################################################################################

# Support for censorship in general
round(summary(lm_cen_02)$coef[2,1],3)
round(summary(lm_cen_02)$coef[2,4],3)

# Support for censorship of political content
round(summary(lm_pol_02)$coef[2,1],3)
round(summary(lm_pol_02)$coef[2,4],3)

# Support for censorship of non-political content
round(summary(lm_non_02)$coef[2,1],3)
round(summary(lm_non_02)$coef[2,4],3)



####################################################################################
## Table 4: Complier Average Causal Effects (CACE) of Participating in Censorship
## on Support for Censorship
####################################################################################

# Support for censorship
cen_iv <- ivreg(Censor_Support ~ ReportClick | as.factor(Group), data = Study2)
# Support for censorship with covariates
cen_iv_cov <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest +
                      Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)
# Support for political censorship
pol_iv <- ivreg(Censor_Pol ~ ReportClick | as.factor(Group), data = Study2)
# Support for political censorship with covariates
pol_iv_cov <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest +
                      Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)
# Support for non-political censorship
non_iv <- ivreg(Censor_Ent ~ ReportClick | as.factor(Group), data = Study2)
# Support for non-political censorship with covariates
non_iv_cov <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest +
                      Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)

stargazer(cen_iv,cen_iv_cov,pol_iv,pol_iv_cov,non_iv,non_iv_cov,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportClick", "Constant"))



