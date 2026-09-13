####################################################
# Participatory Censorship in Authoritarian Regimes
# Replication Files: Online Appendix
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
library(sensemakr)
library(rstatix)
library(ivreg)
library(ivdesc)

## Set Working Directory
setwd("~/Desktop/CPS_Replications")

## Load Data
Study1 <- read_csv("Study1.csv")
Study2 <- read_csv("Study2.csv")


##################################################################################
# Table B1: Descriptive Statistics of the Original and Weighted Survey Sample
##################################################################################

# Gender
b1.1 <- paste(round(rev(table(Study1$Female)/nrow(Study1)) * 100, 1), "%", sep = "")
# Gender: weighted
b1.2 <- paste(round(rev(sapply(names(table(Study1$Female)), function(value) {
  weighted.mean(Study1$Female == value, w = Study1$calibWeight, na.rm = T)
  })) * 100, 1), "%", sep = "")

# Location
b1.3 <- paste(round(table(Study1$Urban)/nrow(Study1) * 100, 1), "%", sep = "")
# Location: weighted
b1.4 <- paste(round(sapply(names(table(Study1$Urban)), function(value) {
  weighted.mean(Study1$Urban == value, w = Study1$calibWeight, na.rm = T)
}) * 100, 1), "%", sep = "")

# Region
# Combining South and Central; North and Northeast
Study1$Region_recode <- ifelse(Study1$Region == 1, 1,
                               ifelse(Study1$Region %in% c(2, 4), 2,
                                      ifelse(Study1$Region %in% c(3, 5), 3,
                                             ifelse(Study1$Region == 6, 4, NA))))
b1.5 <- paste(round(table(Study1$Region_recode)/nrow(Study1) * 100, 1), "%", sep = "")
# Region: weighted
b1.6 <- paste(round(sapply(names(table(Study1$Region_recode)), function(value) {
  weighted.mean(Study1$Region_recode == value, w = Study1$calibWeight, na.rm = T)
}) * 100, 1), "%", sep = "")

# Age Group
# Combining age groups into 4 categories
Study1$Age_Group4 <- ifelse(Study1$Age_Group %in% c(4,5), 4, Study1$Age_Group) 
b1.7 <- paste(round(table(Study1$Age_Group4)/nrow(Study1) * 100, 1), "%", sep = "")
# Age Group: weighted
b1.8 <- paste(round(sapply(names(table(Study1$Age_Group4)), function(value) {
  weighted.mean(Study1$Age_Group4 == value, w = Study1$calibWeight, na.rm = T)
}) * 100, 1), "%", sep = "")


# Education
# Combining Education into 2 categories
Study1$Education2 <- ifelse(Study1$Education %in% c(1,2), 1, 2)
b1.9 <- paste(round(table(Study1$Education2)/nrow(Study1) * 100, 1), "%", sep = "")
# Education: weighted
b1.10 <- paste(round(sapply(names(table(Study1$Education2)), function(value) {
  weighted.mean(Study1$Education2 == value, w = Study1$calibWeight, na.rm = T)
}) * 100, 1), "%", sep = "")

b1.sample   <- c(b1.1,b1.3,b1.5,b1.7,b1.9)
b1.weighted <- c(b1.2,b1.4,b1.6,b1.8,b1.10)

# China Internet Census
# Gender
b1.11 <- paste(c(0.473,0.527) * 100, "%", sep = "")
# Location
b1.12 <- paste(c(0.282,0.718) * 100, "%", sep = "")
# Region
b1.13 <- paste(c(0.311,0.282,0.222,0.185) * 100, "%", sep = "")
# Age Group
b1.14 <- paste(c(0.216,0.268,0.235,0.281) * 100, "%", sep = "")
# Edcation
b1.15 <- paste(c(0.798,0.202) * 100, "%", sep = "")

b1.census <- c(b1.11,b1.12,b1.13,b1.14,b1.15)


table.B1 <- cbind(b1.sample,b1.weighted,b1.census)

colnames(table.B1) <- c("Original Survey Sample", "Weighted Survey Sample", "China Internet Census")

rownames(table.B1) <- c("Gender: Female", "Gender: Male",
                        "Location: Rural", "Location: Urban",
                        "Region: East", "Region: South & Central", "Region: North & Northeast", "Region: West",
                        "Age: <=19", "Age: 20-29", "Age: 30-39", "Age: >=40",
                        "Education: <= High School", "Education: >= College")
table.B1


###########################################################################################
# Figure C1: Distribution of Self-Report Participation in Censorship of Political Content
###########################################################################################

# Full Sample
Freq <- data.frame("Participate_Pol" = c(1:5),
                   "Group" = rep("Full_Sample",5),
                   "n" = as.vector(xtabs(calibWeight ~ Participate_Pol, Study1)),
                   "Proportion" = as.vector(xtabs(calibWeight ~ Participate_Pol, Study1)/
                                              sum(xtabs(calibWeight ~ Participate_Pol, Study1))))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participate_Pol, Age_Group, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Age_Group = recode(Age_Group, `1` = "Age:<20", `2` = "Age:20-29",
                            `3` = "Age:30-39", `4` = "Age:>40"))%>%
  rename("Group" = "Age_Group")

# By Gender
Freq_Female <- Study1 %>%
  count(Participate_Pol, Female, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Female = recode(Female, `0` = "Male", `1` = "Female"))%>%
  rename("Group" = "Female")

# By Education Level
Freq_Edu <- Study1 %>%
  mutate(Education = recode(Education, `5` = 4))%>%
  count(Participate_Pol, Education, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Education = recode(Education, `1` = "Junior High", `2` = "Senior High",
                            `3` = "3Y College", `4` = "4Y College"))%>%
  rename("Group" = "Education")

# Combine
Freq_All <- rbind(Freq, Freq_Female, Freq_Age, Freq_Edu)
Freq_All <- Freq_All %>%
  mutate(Participate_Pol = recode(Participate_Pol, `1` = "1. Never", `2` = "2. Once or twice only",
                                  `3` = "3. Once every few months", `4` = "4. Once per month", `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participate_Pol, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship\nof Political Content",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_C1.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches




###############################################################################################
# Figure C2: Distribution of Self-Report Participation in Censorship of Entertainment Content
###############################################################################################

# Full Sample
Freq <- data.frame("Participate_Ent" = c(1:5),
                   "Group" = rep("Full_Sample",5),
                   "n" = as.vector(xtabs(calibWeight ~ Participate_Ent, Study1)),
                   "Proportion" = as.vector(xtabs(calibWeight ~ Participate_Ent, Study1)/
                                              sum(xtabs(calibWeight ~ Participate_Ent, Study1))))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participate_Ent, Age_Group, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Age_Group = recode(Age_Group, `1` = "Age:<20", `2` = "Age:20-29",
                            `3` = "Age:30-39", `4` = "Age:>40"))%>%
  rename("Group" = "Age_Group")

# By Gender
Freq_Female <- Study1 %>%
  count(Participate_Ent, Female, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Female = recode(Female, `0` = "Male", `1` = "Female"))%>%
  rename("Group" = "Female")

# By Education Level
Freq_Edu <- Study1 %>%
  mutate(Education = recode(Education, `5` = 4))%>%
  count(Participate_Ent, Education, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Education = recode(Education, `1` = "Junior High", `2` = "Senior High",
                            `3` = "3Y College", `4` = "4Y College"))%>%
  rename("Group" = "Education")

# Combine
Freq_All <- rbind(Freq, Freq_Female, Freq_Age, Freq_Edu)
Freq_All <- Freq_All %>%
  mutate(Participate_Ent = recode(Participate_Ent, `1` = "1. Never", `2` = "2. Once or twice only",
                                  `3` = "3. Once every few months", `4` = "4. Once per month", `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participate_Ent, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship\nof Entertainment Content",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_C2.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches



#########################################################################################
# Figure C3: Distribution of Self-Report Participation in Censorship: Unweighted Sample
#########################################################################################

# Full Sample
Freq <- Study1 %>%
  count(Participation) %>%
  filter(complete.cases(.))%>%
  mutate(Group = "Full_Sample")%>%
  mutate(Proportion = prop.table(n))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participation, Age_Group)%>%
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
  mutate(Participation = recode(Participation, `1` = "1. Never", `2` = "2. Once or twice only",
                                `3` = "3. Once every few months", `4` = "4. Once per month", `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participation, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_C3.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches


#############################################################################################################
# Figure C4: Distribution of Self-Report Participation in Censorship of Political Content: Unweighted Sample
#############################################################################################################

# Full Sample
Freq <- Study1 %>%
  count(Participate_Pol) %>%
  filter(complete.cases(.))%>%
  mutate(Group = "Full_Sample")%>%
  mutate(Proportion = prop.table(n))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participate_Pol, Age_Group)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Age_Group = recode(Age_Group, `1` = "Age:<20", `2` = "Age:20-29",
                            `3` = "Age:30-39", `4` = "Age:>40"))%>%
  rename("Group" = "Age_Group")

# By Gender
Freq_Female <- Study1 %>%
  count(Participate_Pol, Female, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Female = recode(Female, `0` = "Male", `1` = "Female"))%>%
  rename("Group" = "Female")

# By Education Level
Freq_Edu <- Study1 %>%
  mutate(Education = recode(Education, `5` = 4))%>%
  count(Participate_Pol, Education, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Education = recode(Education, `1` = "Junior High", `2` = "Senior High",
                            `3` = "3Y College", `4` = "4Y College"))%>%
  rename("Group" = "Education")

# Combine
Freq_All <- rbind(Freq, Freq_Female, Freq_Age, Freq_Edu)
Freq_All <- Freq_All %>%
  mutate(Participate_Pol = recode(Participate_Pol, `1` = "1. Never", `2` = "2. Once or twice only",
                                  `3` = "3. Once every few months", `4` = "4. Once per month", `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participate_Pol, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship\nof Political Content",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_C4.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches



####################################################################################################################
# Figure C5: Distribution of Self-Report Participation in Censorship of Entertainment Con- tent: Unweighted Sample
####################################################################################################################

# Full Sample
Freq <- Study1 %>%
  count(Participate_Ent) %>%
  filter(complete.cases(.))%>%
  mutate(Group = "Full_Sample")%>%
  mutate(Proportion = prop.table(n))

# By Age Grup
Freq_Age <- Study1 %>%
  mutate(Age_Group = recode(Age_Group, `5` = 4))%>%
  count(Participate_Ent, Age_Group)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Age_Group = recode(Age_Group, `1` = "Age:<20", `2` = "Age:20-29",
                            `3` = "Age:30-39", `4` = "Age:>40"))%>%
  rename("Group" = "Age_Group")

# By Gender
Freq_Female <- Study1 %>%
  count(Participate_Ent, Female, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Female = recode(Female, `0` = "Male", `1` = "Female"))%>%
  rename("Group" = "Female")

# By Education Level
Freq_Edu <- Study1 %>%
  mutate(Education = recode(Education, `5` = 4))%>%
  count(Participate_Ent, Education, wt = calibWeight)%>%
  filter(complete.cases(.))%>%           
  mutate(Proportion = prop.table(n))%>%
  mutate(Education = recode(Education, `1` = "Junior High", `2` = "Senior High",
                            `3` = "3Y College", `4` = "4Y College"))%>%
  rename("Group" = "Education")

# Combine
Freq_All <- rbind(Freq, Freq_Female, Freq_Age, Freq_Edu)
Freq_All <- Freq_All %>%
  mutate(Participate_Ent = recode(Participate_Ent, `1` = "1. Never", `2` = "2. Once or twice only",
                                  `3` = "3. Once every few months", `4` = "4. Once per month", `5` = "5. Multiple per month"))

## Plotting
ggplot(Freq_All, aes(fill=Participate_Ent, y=Proportion, x=Group)) + 
  geom_bar(position=position_fill(reverse = TRUE), stat="identity")+
  scale_fill_manual("Participation in Censorship\nof Entertainment Content",
                    values = c("grey81","grey61","grey41","grey21","gray1"))+
  scale_x_discrete(limits = c("", "Full_Sample",
                              "", "Male","Female",
                              "", "Age:<20", "Age:20-29", "Age:30-39", "Age:>40",
                              "", "Junior High", "Senior High", "3Y College", "4Y College")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(file = "Figures_Paper/Figure_C5.pdf",   # The directory you want to save the file in
       width = 8.8, # The width of the plot in inches
       height = 6.5) # The height of the plot in inches



#########################################################################################
## Table D1: Correlation between Participation in Censorship and Support for Censorship
## Using the Five Point Measure of Participation
#########################################################################################

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


stargazer(lm.all.1,lm.all.2,lm.pol.1,lm.pol.2,lm.non.1,lm.non.2,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"))




#########################################################################################
## Table D2: Correlation between Specific Types of Participation in Censorship and 
## Support for Cen- sorship Using the Five Point Measure of Participation
#########################################################################################

## Support for Censorship
lm.all.3 <- lm(Censor_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)
lm.all.4 <- lm(Censor_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Political Content
lm.pol.3 <- lm(Censor_Pol_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)
lm.pol.4 <- lm(Censor_Pol_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Non-Political Content
lm.non.3 <- lm(Censor_Ent_Support ~ Participate_Pol + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)
lm.non.4 <- lm(Censor_Ent_Support ~ Participate_Ent + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

stargazer(lm.all.3,lm.all.4,lm.pol.3,lm.pol.4,lm.non.3,lm.non.4,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"))



##########################################################################################
## Table D3: Correlation between Participation in Censorship and Support for Censorship
## Using Binary Measure of Participation
##########################################################################################

## OLS Regressions
## Support for Censorship
lm.all.5 <- lm(Censor_Support ~ Participate_Binary + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.all.6 <- lm(Censor_Support ~ Participate_Binary + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Political Content
lm.pol.5 <- lm(Censor_Pol_Support ~ Participate_Binary + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.pol.6 <- lm(Censor_Pol_Support ~ Participate_Binary + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)

## Support for Censorship of Non-Political Content
lm.non.5 <- lm(Censor_Ent_Support ~ Participate_Binary + Female + Age_Group + Education + Urban,
               data = Study1, weights = calibWeight)
lm.non.6 <- lm(Censor_Ent_Support ~ Participate_Binary + Female + Age_Group + Education + Urban +
                 Party_Member  + Pol_Ideology + Econ_Ideology + Pol_Interest,
               data = Study1, weights = calibWeight)


stargazer(lm.all.5,lm.all.6,lm.pol.5,lm.pol.6,lm.non.5,lm.non.6,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"))


#################################################################################
## Table D4: Sensitivity of the Main Regression Model to Unobserved Confounders
#################################################################################

all_sensitivity <- sensemakr(model = lm.all.2, treatment = "Participation",
                             benchmark_covariates = "Econ_Ideology", kd = 1:3)
ovb_minimal_reporting(all_sensitivity, format = "latex")




#######################################################
## Table E2: Balance Table (Group Mean & F -test)
#######################################################

## Group Mean
Balance <- Study2 %>%
  group_by(Group) %>%
  get_summary_stats(Female,Age,Education,PartyMember,
                    Ideology,Region,Nationalism,Pol_Interest,Social_Media,Foreign,
                    type = "mean_sd") %>%
  arrange(variable)

Table.E2 <- matrix(as.vector(Balance$mean), ncol = 3, byrow = TRUE)

## F-tests
p_value <- c(Study2 %>% anova_test(Female ~ Group) %>% pull(p),
             Study2 %>% anova_test(Age ~ Group) %>% pull(p),
             Study2 %>% anova_test(Education ~ Group) %>% pull(p),
             Study2 %>% anova_test(PartyMember ~ Group) %>% pull(p),
             Study2 %>% anova_test(Ideology ~ Group) %>% pull(p),
             Study2 %>% anova_test(Region ~ Group) %>% pull(p),
             Study2 %>% anova_test(Nationalism ~ Group) %>% pull(p),
             Study2 %>% anova_test(Pol_Interest ~ Group) %>% pull(p),
             Study2 %>% anova_test(Social_Media ~ Group) %>% pull(p),
             Study2 %>% anova_test(Foreign ~ Group) %>% pull(p))

Table.E2 <- cbind(Table.E2, p_value)

colnames(Table.E2) <- c("Control", "Treatment 1", "Treatment 2", "p-value")
rownames(Table.E2) <- c("Female","Age Group","Education","Party Member",
                            "Economic Ideology","Region","Nationalism","Political Interest",
                            "Social Media Usage","Foreign Connection")  

Table.E2



#########################################################################
## Table E3: Randomization Check: Using Covariates to Predict Treatment
#########################################################################


# Randomization Check

Treat1_Control <- lm(Group ~ Female + Age + Education + PartyMember + 
                     Ideology + Region + Nationalism + Pol_Interest 
                   + Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])
Treat2_Treat1 <- lm(Group ~ Female + Age + Education + PartyMember + 
                     Ideology + Region + Nationalism + Pol_Interest 
                   + Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

stargazer(Treat1_Control,Treat2_Treat1,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"))



#################################################################################################
## Table F1: The Effect of Providing the Opportunity to Participate on Support for Censorship
#################################################################################################


lm.f1.1 <- lm(Censor_Support ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f1.2 <- lm(Censor_Support ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest +
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

lm.f1.3 <- lm(Censor_Pol ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f1.4 <- lm(Censor_Pol ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

lm.f1.5 <- lm(Censor_Ent ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f1.6 <- lm(Censor_Ent ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

stargazer(lm.f1.1,lm.f1.2,lm.f1.3,lm.f1.4,lm.f1.5,lm.f1.6,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Group", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest","Social_Media",
                   "Foreign","Constant"),
          covariate.labels = c("Treatment 1", "Female", "Age Group", "Education",
                               "Party Member", "Ideology", "Nationalism", "Political Interest",
                               "Social Media", "Foreign","Constant"))





##########################################################################
## Table F2: Intention-To-Treat Effect of the Encouragement Treatment
##########################################################################


lm.f2.1 <- lm(Censor_Support ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f2.2 <- lm(Censor_Support ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest +
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

lm.f2.3 <- lm(Censor_Pol ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f2.4 <- lm(Censor_Pol ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

lm.f2.5 <- lm(Censor_Ent ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f2.6 <- lm(Censor_Ent ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

stargazer(lm.f2.1,lm.f2.2,lm.f2.3,lm.f2.4,lm.f2.5,lm.f2.6,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Group", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest","Social_Media",
                   "Foreign","Constant"),
          covariate.labels = c("Treatment 1", "Female", "Age Group", "Education",
                               "Party Member", "Ideology", "Nationalism", "Political Interest",
                               "Social Media", "Foreign","Constant"))




##########################################################################
## Table F3: Complier Average Causal Effects (CACE) of Participating in
## Censorship on Support for Censorship
##########################################################################


# Support for censorship
cen_iv <- ivreg(Censor_Support ~ ReportClick | as.factor(Group), data = Study2)
# Support for censorship with covariates
cen_iv_cov <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)
# Support for political censorship
pol_iv <- ivreg(Censor_Pol ~ ReportClick | as.factor(Group), data = Study2)
# Support for political censorship with covariates
pol_iv_cov <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)
# Support for non-political censorship
non_iv <- ivreg(Censor_Ent ~ ReportClick | as.factor(Group), data = Study2)
# Support for non-political censorship with covariates
non_iv_cov <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClick | as.factor(Group), data = Study2)

stargazer(cen_iv,cen_iv_cov,pol_iv,pol_iv_cov,non_iv,non_iv_cov,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportClick", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))




##########################################################################
## Table F4: Complier Average Causal Effects (CACE) of Participating in
## Censorship on Support for Censorship
##########################################################################


# Support for censorship
cen_iv_N <- ivreg(Censor_Support ~ ReportClickNumber | as.factor(Group), data = Study2)
# Support for censorship with covariates
cen_iv_cov_N <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClickNumber | as.factor(Group), data = Study2)
# Support for political censorship
pol_iv_N <- ivreg(Censor_Pol ~ ReportClickNumber | as.factor(Group), data = Study2)
# Support for political censorship with covariates
pol_iv_cov_N <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClickNumber | as.factor(Group), data = Study2)
# Support for non-political censorship
non_iv_N <- ivreg(Censor_Ent ~ ReportClickNumber | as.factor(Group), data = Study2)
# Support for non-political censorship with covariates
non_iv_cov_N <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportClickNumber | as.factor(Group), data = Study2)

stargazer(cen_iv_N,cen_iv_cov_N,pol_iv_N,pol_iv_cov_N,non_iv_N,non_iv_cov_N,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportClickNumber", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))





###############################################################################################
## Table F5: Complier Average Causal Effects (CACE) of Treatment 1 on Support for Censorship
###############################################################################################


# Support for censorship
cen_iv_1 <- ivreg(Censor_Support ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])
# Support for censorship with covariates
cen_iv_cov_1 <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media +
                      Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])
# Support for political censorship
pol_iv_1 <- ivreg(Censor_Pol ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])
# Support for political censorship with covariates
pol_iv_cov_1 <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + 
                      Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])
# Support for non-political censorship
non_iv_1 <- ivreg(Censor_Ent ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])
# Support for non-political censorship with covariates
non_iv_cov_1 <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + 
                      Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(0,1),])

stargazer(cen_iv_1,cen_iv_cov_1,pol_iv_1,pol_iv_cov_1,non_iv_1,non_iv_cov_1,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportClick", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))




###############################################################################################
## Table F6: Complier Average Causal Effects (CACE) of Treatment 2 on Support for Censorship
###############################################################################################


# Support for censorship
cen_iv_2 <- ivreg(Censor_Support ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])
# Support for censorship with covariates
cen_iv_cov_2 <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                        Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media +
                        Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])
# Support for political censorship
pol_iv_2 <- ivreg(Censor_Pol ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])
# Support for political censorship with covariates
pol_iv_cov_2 <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                        Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + 
                        Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])
# Support for non-political censorship
non_iv_2 <- ivreg(Censor_Ent ~ ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])
# Support for non-political censorship with covariates
non_iv_cov_2 <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                        Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + 
                        Foreign | ReportClick | as.factor(Group), data = Study2[Study2$Group %in% c(1,2),])

stargazer(cen_iv_2,cen_iv_cov_2,pol_iv_2,pol_iv_cov_2,non_iv_2,non_iv_cov_2,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportClick", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))




###########################################################################################################
## Table F7: Complier Average Causal Effects (CACE) of Reporting Anti-Regime Posts on Support for Censorship
###########################################################################################################


# Support for censorship
cen_iv_anti <- ivreg(Censor_Support ~ ReportAnti | as.factor(Group), data = Study2)
# Support for censorship with covariates
cen_iv_cov_anti <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportAnti | as.factor(Group), data = Study2)
# Support for political censorship
pol_iv_anti <- ivreg(Censor_Pol ~ ReportAnti | as.factor(Group), data = Study2)
# Support for political censorship with covariates
pol_iv_cov_anti <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportAnti | as.factor(Group), data = Study2)
# Support for non-political censorship
non_iv_anti <- ivreg(Censor_Ent ~ ReportAnti | as.factor(Group), data = Study2)
# Support for non-political censorship with covariates
non_iv_cov_anti <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                      Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportAnti | as.factor(Group), data = Study2)

stargazer(cen_iv_anti,cen_iv_cov_anti,pol_iv_anti,pol_iv_cov_anti,non_iv_anti,non_iv_cov_anti,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportAnti", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))



###########################################################################################################
## Table F8: Complier Average Causal Effects (CACE) of Reporting Pro-Regime Posts on Support for Censorship
###########################################################################################################


# Support for censorship
cen_iv_pro <- ivreg(Censor_Support ~ ReportPro | as.factor(Group), data = Study2)
# Support for censorship with covariates
cen_iv_cov_pro <- ivreg(Censor_Support ~ Female + Age + Education + PartyMember + 
                           Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportPro | as.factor(Group), data = Study2)
# Support for political censorship
pol_iv_pro <- ivreg(Censor_Pol ~ ReportPro | as.factor(Group), data = Study2)
# Support for political censorship with covariates
pol_iv_cov_pro <- ivreg(Censor_Pol ~ Female + Age + Education + PartyMember + 
                           Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportPro | as.factor(Group), data = Study2)
# Support for non-political censorship
non_iv_pro <- ivreg(Censor_Ent ~ ReportPro | as.factor(Group), data = Study2)
# Support for non-political censorship with covariates
non_iv_cov_pro <- ivreg(Censor_Ent ~ Female + Age + Education + PartyMember + 
                           Ideology + factor(Region) + Nationalism + Pol_Interest + Social_Media + Foreign | ReportPro | as.factor(Group), data = Study2)

stargazer(cen_iv_pro,cen_iv_cov_pro,pol_iv_pro,pol_iv_cov_pro,non_iv_pro,non_iv_cov_pro,
          style = "APSR", omit.stat = c("LL", "ser", "f", "rsq", "adj.rsq"),
          keep = c("ReportPro", "Female","Age","Education","PartyMember",
                   "Ideology","Nationalism","Pol_Interest",
                   "Social_Media","Foreign", "Constant"))



###########################################################################################################
## Figure F1: Profiling Compliers, Never-Takers, and Always-Takers Using Treatment Groups 1 & 2
###########################################################################################################

complier.dat <- Study2[Study2$Group %in% c(1,2),] %>% 
  dplyr::select(ReportClick,Group,Female,Age,Education,
                PartyMember,Ideology,Nationalism,Social_Media,Foreign) %>% 
  gather(var,val,-ReportClick,-Group)

set.seed(0618)
complier.dat$Group <- complier.dat$Group - 1
res <- complier.dat %>% group_by(var) %>%  
  do(m=ivdesc(X=.$val,D=.$ReportClick,Z=.$Group, bootn=1000, boot=TRUE))

# Combine 
res <- unnest(res)
resA <- res %>% filter(var=="Female") %>% 
  dplyr::select(group,pi,pi_se) %>% 
  mutate(var="Proportion") %>% 
  rename(mu=pi,mu_se=pi_se)
resB <- res %>% 
  dplyr::select(var,group,mu,mu_se)
res <- bind_rows(resA,resB) %>% 
  mutate(lo=mu+mu_se*qnorm(0.025), 
         hi=mu+mu_se*qnorm(0.975))
glabso <- rev(c("sample", "co", "nt", "at"))
flabso <- c("Proportion",
            "Age","Female","Education","PartyMember",
            "Foreign","Nationalism","Ideology","Social_Media")
res <- ungroup(res) %>% 
  mutate(var=factor(var, levels=flabso),
         group=factor(group, levels=glabso))
res[1,c(2,3,5,6)] <- NA

complier.plot <- ggplot(res, aes(y = group, x = mu)) + 
  geom_point() +
  facet_wrap("var", scale="free_x",
             labeller = as_labeller(c("Proportion" = "Proportion",
                                      "Age" = "Age Group",
                                      "Education" = "Education",
                                      "Female" = "Female",
                                      "Foreign" = "Foreign Connection",
                                      "Ideology" = "Economic Ideology",
                                      "Nationalism" = "Nationalism",
                                      "PartyMember" = "Party Member",
                                      "Social_Media" = "Social Media Exposure"
             ))) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0) +
  scale_x_continuous(name = "Mean Proportion & 95% Confidence Interval") +
  scale_y_discrete(name = "",
                   labels = c("Always-taker",
                              "Never-taker",
                              "Complier",
                              "Sample")) + 
  theme_bw() +
  theme(text = element_text(size=8))

complier.plot
ggsave(complier.plot, file = "Figures_Paper/Figure_F1.pdf",   # The directory you want to save the file in
       width =5, # The width of the plot in inches
       height = 4) # The height of the plot in inches




#################################################################################################
## Table F9: The Effect of Providing the Opportunity to Participate on Regime Support
#################################################################################################


lm.f9.1 <- lm(Regime_Satisfaction ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f9.2 <- lm(Regime_Satisfaction ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest +
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

lm.f9.3 <- lm(Regime_Assessment ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f9.4 <- lm(Regime_Assessment ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

lm.f9.5 <- lm(Regime_Trust ~ Group, data = Study2[Study2$Group %in% c(0,1),])
lm.f9.6 <- lm(Regime_Trust ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(0,1),])

stargazer(lm.f9.1,lm.f9.2,lm.f9.3,lm.f9.4,lm.f9.5,lm.f9.6,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Group","Constant"),
          covariate.labels = c("Treatment 1","Constant"))





#################################################################################################
## Table F10: Intention-To-Treat Effect of Encouragement Treatment on Regime Support
#################################################################################################


lm.f10.1 <- lm(Regime_Satisfaction ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f10.2 <- lm(Regime_Satisfaction ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest +
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

lm.f10.3 <- lm(Regime_Assessment ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f10.4 <- lm(Regime_Assessment ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

lm.f10.5 <- lm(Regime_Trust ~ Group, data = Study2[Study2$Group %in% c(1,2),])
lm.f10.6 <- lm(Regime_Trust ~ Group + Female + Age + Education + PartyMember + 
                Ideology + factor(Region) + Nationalism + Pol_Interest + 
                Social_Media + Foreign, data = Study2[Study2$Group %in% c(1,2),])

stargazer(lm.f10.1,lm.f10.2,lm.f10.3,lm.f10.4,lm.f10.5,lm.f10.6,
          style = "APSR", omit.stat=c("LL","ser","f", "rsq"),
          keep = c("Group","Constant"),
          covariate.labels = c("Treatment 1","Constant"))


