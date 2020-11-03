# Developing an R package just for the IAM Performance Measurement 2020 Survey 
# would have been an overkill. To keep things simpler, I chose to store reusable
# functions in this R script and load it from GitHub with the devtools package.

# Some will notice I don't load libraries. Instead, I prefer to use the unambiguous
# syntax *package::function*. This makes the code slightly harsher to read but this
# is a price I am pleased to pay. 

# Set console to English
Sys.setenv(LANG = "en");

#print("Loading the IAMPerf2020 environment...");

# Packages
if(!require("RCurl")) install.packages("RCurl");

# Configuration options
iamperf2020_data_url = "https://raw.githubusercontent.com/Open-Measure/Open-Data/master/IAMPerf2020-Dataset/";

# Load the data
iamperf2020_survey <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Survey.csv")));
iamperf2020_questions <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Questions.csv")));
iamperf2020_q09_countries <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q09_Countries.csv")));
iamperf2020_q10_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q10_Roles.csv")));
iamperf2020_q11_experience_fields <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q11_ExperienceFields.csv")));
iamperf2020_q11_experience_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q11_ExperienceLevels.csv")));
iamperf2020_q13_org_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q13_OrgRoles.csv")));
iamperf2020_q14_org_targets <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q14_OrgTargets.csv")));
iamperf2020_q17_industrial_sectors <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q17_IndustrialSectors.csv")));
iamperf2020_q18_org_sizes <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q18_OrgSizes.csv")));
iamperf2020_q20_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q20_Domains.csv")));
iamperf2020_q20_team_dedication <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q20_TeamDedication.csv")));
iamperf2020_q21_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q21_Domains.csv")));
iamperf2020_q21_centralization <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q21_Centralization.csv")));
iamperf2020_q22_reporting_lines <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q22_ReportingLines.csv")));
iamperf2020_q23_goals <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q23_Goals.csv")));
iamperf2020_q23_priorities <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q23_Priorities.csv")));
iamperf2020_q24_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q24_Domains.csv")));
iamperf2020_q24_maturity_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q24_CapabilityMaturityLevels.csv")));
iamperf2020_q26_indicator_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q26_IndicatorRoles.csv")));
iamperf2020_q27_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q27_AgreementLevels.csv")));
iamperf2020_q27_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q27_BestPractices.csv")));
iamperf2020_q28_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q28_AgreementLevels.csv")));
iamperf2020_q28_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q28_BestPractices.csv")));
iamperf2020_q29_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q29_AgreementLevels.csv")));
iamperf2020_q29_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q29_BestPractices.csv")));
iamperf2020_q30_automation_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q30_IndicatorAutomation.csv")));
iamperf2020_q31_coverage_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q31_CoverageLevels.csv")));
iamperf2020_q31_dimensions <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q31_Dimensions.csv")));
iamperf2020_q32_leading_vs_lagging_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q32_LeadingVSLaggingLevels.csv")));
iamperf2020_q38_polar_question_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q38_PolarQuestionLevels.csv")));
iamperf2020_q39_polar_question_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q39_PolarQuestionLevels.csv")));
iamperf2020_q40_polar_question_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_Q40_PolarQuestionLevels.csv")));
iamperf2020_duplicate_responses <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020_DuplicateResponses.csv")));

# Clean the dataset from the duplicate entries identified manually.
# Look at the Justification column for more information on the reason
# why these records should be removed from the dataset.
iamperf2020_survey = iamperf2020_survey[
  !(
    iamperf2020_survey$ResponseID %in% 
    iamperf2020_duplicate_responses$DuplicateResponseID),
  ];

# Q9: Apply nicely labeled and properly unordered factors.
iamperf2020_survey$Q9 = factor(iamperf2020_survey$Q9, levels = iamperf2020_q09_countries$CountryCode, labels = iamperf2020_q09_countries$CountryISO2, ordered = FALSE, exclude = NA);
iamperf2020_survey$Q9B = factor(iamperf2020_survey$Q9B, levels = iamperf2020_q09_countries$CountryCode, labels = iamperf2020_q09_countries$CountryISO2, ordered = FALSE, exclude = NA);

# Q10: Apply labeled and properly unordered factors.
for(column_counter in 1:nrow(iamperf2020_q10_roles)){
  current_column = as.character(iamperf2020_q10_roles[column_counter, "X"]);
  current_levels = c(1); # Single level :-)
  current_labels = as.character(iamperf2020_q10_roles[column_counter, "Title"]);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = FALSE, exclude = NA);
};

# Q11: Experience. 
# Apply labeled and properly unordered factors.
# Integer values 0 (no experience) ... 10+ (veteran) = number of years.
for(column_counter in 1:nrow(iamperf2020_q11_experience_fields)){
  current_column = as.character(iamperf2020_q11_experience_fields[column_counter, "X"]);
  current_levels = iamperf2020_q11_experience_levels$X;
  current_labels = as.character(iamperf2020_q11_experience_levels$Title);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = TRUE, exclude = NA);
};

# Q12 Textual Information

# Q13 Organization
for(column_counter in 1:nrow(iamperf2020_q13_org_roles)){
  current_column = as.character(iamperf2020_q13_org_roles[column_counter, "X"]);
  current_levels = c(1); # Single level :-)
  current_labels = as.character(iamperf2020_q13_org_roles[column_counter, "Title"]);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = FALSE, exclude = NA);
};

# Q14 Org Target
iamperf2020_survey$Q14 = factor(
  iamperf2020_survey$Q14, 
  levels = as.integer(iamperf2020_q14_org_targets$X), 
  labels = as.character(iamperf2020_q14_org_targets$Title), 
  ordered = FALSE, exclude = NA);

# Q15 Last Work Year
# Retrieve and sort the unique years from Q15 answers
q15_years = sort(unique(iamperf2020_survey$Q15[!is.na(iamperf2020_survey$Q15)]));
iamperf2020_survey$Q15 = factor(
  iamperf2020_survey$Q15, 
  labels = q15_years, 
  ordered = TRUE, exclude = NA);

# Q16: Textual Information

# Q17: Industrial Sector
for(column_counter in 1:nrow(iamperf2020_q17_industrial_sectors)){
  current_column = as.character(iamperf2020_q17_industrial_sectors[column_counter, "X"]);
  current_levels = c(1); # Single level :-)
  current_labels = as.character(iamperf2020_q17_industrial_sectors[column_counter, "Title"]);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = FALSE, exclude = NA);
};

# Q18: Organization Size
iamperf2020_survey$Q18 = factor(
  iamperf2020_survey$Q18, 
  levels = as.integer(iamperf2020_q18_org_sizes$X), 
  labels = as.character(iamperf2020_q18_org_sizes$Title), 
  ordered = TRUE, exclude = NA);

# Q19: Textual Information (not a question)

# Q20: Team Dedication.
iamperf2020_survey$Q20R1 = factor(iamperf2020_survey$Q20R1, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R2 = factor(iamperf2020_survey$Q20R2, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R3 = factor(iamperf2020_survey$Q20R3, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R4 = factor(iamperf2020_survey$Q20R4, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R5 = factor(iamperf2020_survey$Q20R5, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R6 = factor(iamperf2020_survey$Q20R6, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);

# Q21: Centralization.
iamperf2020_survey$Q21R1 = factor(iamperf2020_survey$Q21R1, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q21R2 = factor(iamperf2020_survey$Q21R2, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q21R3 = factor(iamperf2020_survey$Q21R3, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q21R4 = factor(iamperf2020_survey$Q21R4, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q21R5 = factor(iamperf2020_survey$Q21R5, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q21R6 = factor(iamperf2020_survey$Q21R6, levels = iamperf2020_q21_centralization$X, labels = iamperf2020_q21_centralization$Title, ordered = TRUE, exclude = NA);
# In the original dataset, the factor order (column X) is counter-intuitive:
# Centralized < Balanced < De-centralized
# Inverse the sequence:
iamperf2020_survey$Q21R1 = factor(iamperf2020_survey$Q21R1,levels(iamperf2020_survey$Q21R1)[3:1]);
iamperf2020_survey$Q21R2 = factor(iamperf2020_survey$Q21R2,levels(iamperf2020_survey$Q21R2)[3:1]);
iamperf2020_survey$Q21R3 = factor(iamperf2020_survey$Q21R3,levels(iamperf2020_survey$Q21R3)[3:1]);
iamperf2020_survey$Q21R4 = factor(iamperf2020_survey$Q21R4,levels(iamperf2020_survey$Q21R4)[3:1]);
iamperf2020_survey$Q21R5 = factor(iamperf2020_survey$Q21R5,levels(iamperf2020_survey$Q21R5)[3:1]);
iamperf2020_survey$Q21R6 = factor(iamperf2020_survey$Q21R6,levels(iamperf2020_survey$Q21R6)[3:1]);

# Q22: IAM Manager Reporting Line.
for(column_counter in 1:nrow(iamperf2020_q22_reporting_lines)){
  current_column = as.character(iamperf2020_q22_reporting_lines[column_counter, "X"]);
  current_levels = c(1); # Single level :-)
  current_labels = as.character(iamperf2020_q22_reporting_lines[column_counter, "Title"]);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = FALSE, exclude = NA);
};

# Q23: Apply nicely labeled and properly ordered factors.
iamperf2020_survey$Q23R1 = factor(iamperf2020_survey$Q23R1, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R2 = factor(iamperf2020_survey$Q23R2, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R3 = factor(iamperf2020_survey$Q23R3, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R4 = factor(iamperf2020_survey$Q23R4, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R5 = factor(iamperf2020_survey$Q23R5, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R6 = factor(iamperf2020_survey$Q23R6, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R7 = factor(iamperf2020_survey$Q23R7, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R8 = factor(iamperf2020_survey$Q23R8, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R9 = factor(iamperf2020_survey$Q23R9, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R10 = factor(iamperf2020_survey$Q23R10, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q23R11 = factor(iamperf2020_survey$Q23R11, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);

# Q24: Maturity Levels
iamperf2020_survey$Q24R1 = factor(iamperf2020_survey$Q24R1, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q24R2 = factor(iamperf2020_survey$Q24R2, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q24R3 = factor(iamperf2020_survey$Q24R3, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q24R4 = factor(iamperf2020_survey$Q24R4, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q24R5 = factor(iamperf2020_survey$Q24R5, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q24R6 = factor(iamperf2020_survey$Q24R6, levels = iamperf2020_q24_maturity_levels$X, labels = iamperf2020_q24_maturity_levels$Title, ordered = TRUE, exclude = NA);

# Q25: Only textual information
# N/A

# Q26: Indicator Roles
for(column_counter in 1:nrow(iamperf2020_q26_indicator_roles)){
  current_column = as.character(iamperf2020_q26_indicator_roles[column_counter, "X"]);
  current_levels = c(1); # Single level :-)
  current_labels = as.character(iamperf2020_q26_indicator_roles[column_counter, "Title"]);
  iamperf2020_survey[,current_column] = factor(
    iamperf2020_survey[,current_column], 
    levels = current_levels, 
    labels = current_labels, 
    ordered = FALSE, exclude = NA);
};

# Q27: Indicator Design: Best Practices
iamperf2020_survey$Q27R1 = factor(iamperf2020_survey$Q27R1, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R2 = factor(iamperf2020_survey$Q27R2, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R3 = factor(iamperf2020_survey$Q27R3, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R4 = factor(iamperf2020_survey$Q27R4, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R5 = factor(iamperf2020_survey$Q27R5, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R6 = factor(iamperf2020_survey$Q27R6, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R7 = factor(iamperf2020_survey$Q27R7, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R8 = factor(iamperf2020_survey$Q27R8, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q27R9 = factor(iamperf2020_survey$Q27R9, levels = iamperf2020_q27_agreement_levels$X, labels = iamperf2020_q27_agreement_levels$Title, ordered = TRUE, exclude = NA);

# Q28: Indicator Framework: Best Practices
iamperf2020_survey$Q28R1 = factor(iamperf2020_survey$Q28R1, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q28R2 = factor(iamperf2020_survey$Q28R2, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q28R3 = factor(iamperf2020_survey$Q28R3, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q28R4 = factor(iamperf2020_survey$Q28R4, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q28R5 = factor(iamperf2020_survey$Q28R5, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q28R6 = factor(iamperf2020_survey$Q28R6, levels = iamperf2020_q28_agreement_levels$X, labels = iamperf2020_q28_agreement_levels$Title, ordered = TRUE, exclude = NA);

# Q29: Indicator Communication: Best Practices
iamperf2020_survey$Q29R1 = factor(iamperf2020_survey$Q29R1, levels = iamperf2020_q29_agreement_levels$X, labels = iamperf2020_q29_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q29R2 = factor(iamperf2020_survey$Q29R2, levels = iamperf2020_q29_agreement_levels$X, labels = iamperf2020_q29_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q29R3 = factor(iamperf2020_survey$Q29R3, levels = iamperf2020_q29_agreement_levels$X, labels = iamperf2020_q29_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q29R4 = factor(iamperf2020_survey$Q29R4, levels = iamperf2020_q29_agreement_levels$X, labels = iamperf2020_q29_agreement_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q29R5 = factor(iamperf2020_survey$Q29R5, levels = iamperf2020_q29_agreement_levels$X, labels = iamperf2020_q29_agreement_levels$Title, ordered = TRUE, exclude = NA);

# Q30 Indicator Automation
# Warning: the original levels are ordered as 1: automated, 2: partially, 3: manual.
# Here, thanks to the pre-ordering in the CSV file, I reorder the factor to 3: manual, 2: partially, 1: automated
# When charting the data, going from "negative" to "positive" is in effect more natural.
iamperf2020_survey$Q30 = factor(iamperf2020_survey$Q30, levels = iamperf2020_q30_automation_levels$X, labels = iamperf2020_q30_automation_levels$Short, ordered = TRUE, exclude = NA);

# Q31 Indicator Measurement Dimensions.
iamperf2020_survey$Q31R1 = factor(iamperf2020_survey$Q31R1, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R2 = factor(iamperf2020_survey$Q31R2, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R3 = factor(iamperf2020_survey$Q31R3, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R4 = factor(iamperf2020_survey$Q31R4, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R5 = factor(iamperf2020_survey$Q31R5, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R6 = factor(iamperf2020_survey$Q31R6, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R7 = factor(iamperf2020_survey$Q31R7, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R8 = factor(iamperf2020_survey$Q31R8, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R9 = factor(iamperf2020_survey$Q31R9, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q31R10 = factor(iamperf2020_survey$Q31R10, levels = iamperf2020_q31_coverage_levels$X, labels = iamperf2020_q31_coverage_levels$Title, ordered = TRUE, exclude = NA);

# Q32 Lead vs lag indicators
iamperf2020_survey$Q32 = factor(iamperf2020_survey$Q32, levels = iamperf2020_q32_leading_vs_lagging_levels$X, labels = iamperf2020_q32_leading_vs_lagging_levels$Title, ordered = TRUE, exclude = NA);

# Q33 - Top 3 Indicators
# N/A

# Q34 - Negative - Have you observed counter-productive...
# N/A

# Q35 - IndNumber - How many IAM performance indicators...
# N/A

# Q36 - Difficulty - What barriers or difficulties...
# N/A

# Q37 - Advise - What would be your key advises to someone...
# N/A

# Q38 - Bench1 - Did your organization benchmarked its...
iamperf2020_survey$Q38 = factor(iamperf2020_survey$Q38, levels = iamperf2020_q38_polar_question_levels$X, labels = iamperf2020_q38_polar_question_levels$Title, ordered = FALSE, exclude = NA);

# Q39 - Standard - Do you believe your organization...
iamperf2020_survey$Q39 = factor(iamperf2020_survey$Q39, levels = iamperf2020_q39_polar_question_levels$X, labels = iamperf2020_q39_polar_question_levels$Title, ordered = FALSE, exclude = NA);

# Q40 - Bench2 - Do you believe your organization would benefit...
iamperf2020_survey$Q40 = factor(iamperf2020_survey$Q40, levels = iamperf2020_q40_polar_question_levels$X, labels = iamperf2020_q40_polar_question_levels$Title, ordered = FALSE, exclude = NA);

# Q41 - Else - Would you like to share with us...
# N/A


#print("IAMPerf2020 environment loaded.")

# References
#
# I list here a bunch of articles that have been inspiring or helpful while writing this notebook.
#
# * https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github
# * https://www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf

