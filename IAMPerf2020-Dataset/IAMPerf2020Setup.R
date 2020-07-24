# Developing an R package just for the IAM Performance Measurement 2020 Survey 
# would have been an overkill. To keep things simpler, I chose to store reusable
# functions in this R script and load it from GitHub with the devtools package.

# Some will notice I don't load libraries. Instead, I prefer to use the unambiguous
# syntax *package::function*. This makes the code slightly harsher to read but this
# is a price I am pleased to pay. 

print("Loading the IAMPerf2020 environment...");

# Packages
if(!require("RCurl")) install.packages("RCurl");
if(!require("htmltools")) install.packages("htmltools");
if(!require("scales")) install.packages("scales");
if(!require("likert")) install.packages("likert");
#if(!require("HH")) install.packages("HH");
if(!require("ggplot2")) install.packages("ggplot2");
if(!require("RColorBrewer")) install.packages("RColorBrewer");

# Configuration options
iamperf2020_data_url = "https://raw.githubusercontent.com/Open-Measure/Open-Data/master/IAMPerf2020-Dataset/";

# Load the data
iamperf2020_survey <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020.csv")));
iamperf2020_q9_countries <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q9Countries.csv")));
iamperf2020_q10_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q10Roles.csv")));
iamperf2020_q11_experience_fields <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q11ExperienceFields.csv")));
iamperf2020_q11_experience_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q11ExperienceLevels.csv")));
iamperf2020_q13_org_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q13OrgRoles.csv")));
iamperf2020_q14_org_targets <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q14OrgTargets.csv")));
iamperf2020_q17_industrial_sectors <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q17IndustrialSectors.csv")));
iamperf2020_q18_org_sizes <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q18OrgSizes.csv")));
iamperf2020_q20_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q20Domains.csv")));
iamperf2020_q20_team_dedication <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q20TeamDedication.csv")));
iamperf2020_q21_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q21Domains.csv")));
iamperf2020_q21_centralization <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q21Centralization.csv")));
iamperf2020_q22_reporting_lines <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q22ReportingLines.csv")));
iamperf2020_q23_goals <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q23Goals.csv")));
iamperf2020_q23_priorities <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q23Priorities.csv")));
iamperf2020_q24_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q24Domains.csv")));
iamperf2020_q24_maturity_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q24MaturityLevels.csv")));
iamperf2020_q26_indicator_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q26IndicatorRoles.csv")));
iamperf2020_q27_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q27AgreementLevels.csv")));
iamperf2020_q27_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q27BestPractices.csv")));
iamperf2020_q28_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q28AgreementLevels.csv")));
iamperf2020_q28_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q28BestPractices.csv")));
iamperf2020_q29_agreement_levels <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q29AgreementLevels.csv")));
iamperf2020_q29_best_practices <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q29BestPractices.csv")));

# Q9: Apply nicely labeled and properly unordered factors.
iamperf2020_survey$Q9 = factor(iamperf2020_survey$Q9, levels = iamperf2020_q9_countries$CountryCode, labels = iamperf2020_q9_countries$CountryISO2, ordered = FALSE, exclude = NA);
iamperf2020_survey$Q9B = factor(iamperf2020_survey$Q9B, levels = iamperf2020_q9_countries$CountryCode, labels = iamperf2020_q9_countries$CountryISO2, ordered = FALSE, exclude = NA);

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

#Q13 Organization
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

#Q14 Org Target
iamperf2020_survey$Q14 = factor(
  iamperf2020_survey$Q14, 
  levels = as.integer(iamperf2020_q14_org_targets$X), 
  labels = as.character(iamperf2020_q14_org_targets$Title), 
  ordered = FALSE, exclude = NA);

#Q15 Last Work Year
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

# Q30:

# Q31:



## Small trick for good-looking chart labels
# When labelling charts with ratios, such as percentages, naive number rounding 
# naturally yield incorrect sums (e.g. the sum of label percentages is 99.9% or 
# 100.1 instead of 100%). This may surprise readers. To avoid this, the Largest 
# Remainder Method may be applied. Values will be slightly incorrect (this is 
# unavoidable because of the rounding) but the same will be correct.  
rounded_ratios_with_largest_remainder = function(
  int_values, 
  target_sum = 100, # Default for percentages
  digits = 2){
  parties = paste0("p", 1:length(int_values)); # Arbitraty party names.
  inflated_target = target_sum * 10 ^ digits; # Largest remainder method is designed to work with integer values. Because we want numbers with n digits, we need to inflate our numbers temporarily.
  election = electoral::seats_lr(
    parties = parties, 
    votes = int_values,
    n_seats = inflated_target,
    method = "hare");
  deflated_election = election / 10 ^ digits;
  return(deflated_election);
};

plot_pie_flavour_1 = function(
  plot_data, # An factorized data vector 
  title = NULL,
  sub_title = NULL,
  axis_label = NULL
  ){
  
  # Temp solution
  fill_colors = c("#ff6600", "#dddddd", "#0066ff");
  
  data_count = table(plot_data);
  data_count_label = paste("(", data_count, ")", sep = "");
  data_group = names(data_count);
  data_frequency = prop.table(data_count);
  data_frequency_label = paste(rounded_ratios_with_largest_remainder(data_frequency, digits = 1), "%");
  data_label = paste(data_frequency_label, data_count_label, sep = "\n");
  
  # Tweak things for the pie chart to look good.
  y_position = cumsum(data_frequency) - 0.5 * data_frequency;
  
  # The amazing thing about drawing pie charts with ggplot2 is that it is
  # extremely complex and will take you 3-4 hours to get it right. If you
  # would rather use the native pie chart from R, you would get the same result
  # in... 5 minutes. So why put oneself through this? Well... learning 
  # ggplot2 is expected to be more rewarding in the long-run. Let's hope it 
  # will really be.
  pie_plot = ggplot2::ggplot(
    data = NULL, 
    ggplot2::aes(
      x = "",
      y = data_frequency,
      fill = data_group,
      title,
      sub_title)
  ) +
    ggplot2::scale_y_continuous() +
    ggplot2::geom_bar(width = 1, stat = "identity") + 
    # ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::scale_fill_brewer(palette = "Paired") + 
    ggplot2::geom_text(
      ggplot2::aes(label = data_label),
      position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust=0.5),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank()) +
    ggplot2::coord_polar(theta="y") +
    ggplot2::ylab("") +
    ggplot2::labs(fill = axis_label) + 
    ggplot2::ggtitle(
      title,
      subtitle = sub_title);
  
  return(pie_plot);
}

if(!require("ggrepel")) install.packages("ggrepel");
plot_bars = function(
  title = NULL,
  subtitle = NULL,
  axis_x_title = NULL,
  axis_y_title = NULL,
  remove_na = TRUE,
  data_series_1, # A factorized vector or dataframe. 
  series_name_1 = "",
  data_series_2 = NULL,
  series_name_2 = "",
  data_series_3 = NULL,
  series_name_3 = "",
  data_series_4 = NULL,
  series_name_4 = "",
  legend_title = "Legend"
){
  
  get_plot_data = function(data_series, series_name = ""){
    data_series = as.vector(unlist(data_series)); # Coerce to a single vector.
    if(remove_na) data_series_1 = data_series[!is.na(data_series)]; # Remove NAs if applicable.
    data_table = table(data_series);
    data_count_label = paste("(", data_table, ")", sep = "");
    data_category = names(data_table);
    data_frequency = prop.table(data_table);
    data_frequency_label = paste(rounded_ratios_with_largest_remainder(data_frequency, digits = 1), "%");
    data_label = paste(data_frequency_label, data_count_label, sep = " ");
    data_count = as.vector(data_table);
    return(
      data.frame(
        category = data_category,
        count = data_count,
        label = data_label,
        group = rep(series_name, length(data_count))
      ));
  }
  
  plot_data = get_plot_data(data_series_1, series_name_1);
  if(!is.null(data_series_2)){
    plot_data = rbind(plot_data, get_plot_data(data_series_2, series_name_2));
  };

  plot_object = ggplot2::ggplot(
    data = plot_data, 
    ggplot2::aes(
      y = reorder(category, count),
      x = count,
      fill = group)
    ) +
    scale_fill_brewer(palette = "Accent") +
    ggplot2::geom_bar(
      #ggplot2::aes(),
      stat="identity",
      position = ggplot2::position_dodge(width = .75),
      colour = "black"
    ) + 
    ggplot2::geom_text(
      ggplot2::aes(label = label), 
      hjust = -0.5, 
      size = 3,
      position = position_dodge(width = 1),
      inherit.aes = TRUE
    ) +
    #ggrepel::geom_label_repel(
    #  direction = "x",
    #  nudge_x = 1,
    #  ggplot2::aes(alpha = .8, fill = "#ffffff", vjust = 0)
    #) +
    #ggplot2::geom_label(
    #  position = "fill",
    #  hjust = -.25, 
    #  size = 3
    #  ) +
  #) + ggplot2::geom_label(
  #  ggplot2::aes(label = label, size = .4), 
  #  position = ggplot2::position_dodge(width = 1), 
  #  hjust = -0.1,
  #  fill = "white",
  #  alpha = .8
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = legend_title,
      x = axis_x_title,
      y = axis_y_title
  ) + 
      ggplot2::theme(legend.position = "bottom");
  return(plot_object);
}

plot_stack_count = function(
  title = NULL,
  subtitle = NULL,
  axis_x_title = NULL,
  axis_y_title = NULL,
  remove_na = TRUE,
  data_series = NULL, # data.frame with columns "group", "category", "count", "label"
  legend_title = NULL
){
  
  plot_object = ggplot2::ggplot(
    data = plot_data, 
    ggplot2::aes(
      y = reorder(group, category),
      x = count,
      label = label,
      fill = category),
  ) + 
    ggplot2::scale_fill_brewer(palette = "Accent") +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_stack(),
      colour = "black"
  ) + 
    ggplot2::geom_label(
      stat = "identity",
      #inherit.aes = TRUE,
      #fill = "white",
      position = ggplot2::position_stack(),
      show.legend = FALSE
  ) + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    fill = legend_title,
    x = axis_x_title,
    y = axis_y_title
  ) + ggplot2::theme(legend.position = "bottom");
  
  return(plot_object);
}

plot_upset = function(
  title = NULL, 
  subtitle = NULL, 
  caption,
  data_frame){
  # Uses the UpSetR package to plot an Upset graph.
  # This is ideal for the visualization of categorical data (or sets)
  # when the number of categories is no longer supporder by
  # Venn and Euler diagrams.
  #
  # Inputs:
  # data_frame: expects a dataframe composed of columns,
  # where every column is a category with a single level,
  # where the factor is labelled with a "friendly" category name, 
  # and where NA means "not selected".
  #  
  # References:
  # - https://academic.oup.com/bioinformatics/article/33/18/2938/3884387
  # - https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html
  #
  #if(!require("ggplot2")) install.packages("ggplot2");
  #if(!require("viridis")) install.packages("viridis");
  #if(!require("UpSetR")) install.packages("UpSetR");
  if(!require("processx")) install.packages("processx");
  if(!require("devtools")) install.packages("devtools");
  devtools::install_github("hms-dbmi/UpSetR")
  #if(!require("ggstatsplot")) install.packages("ggstatsplot");
  
  # Retrieve the category full names from the factor labels.
  friendly_categories = as.character(unlist(lapply(data_frame, levels))); 
  
  # Convert our data to a matrix of 0s and 1s.
  data_frame = as.data.frame(ifelse(!is.na(data_frame),1,0));
  
  # Remove survey answers that have no option selected.
  data_frame = data_frame[rowSums(data_frame) > 0,];
  
  # UpSetR::upset expects that column names are equal to friendly category names.
  colnames(data_frame) = friendly_categories;
  
  # Remove categories that have never been used.
  data_frame = data_frame[,colSums(data_frame) > 0];
  
  # Reduce categories to active categories,
  # and sort categories by frequencies.
  friendly_categories = names(sort(colSums(data_frame), decreasing = FALSE));
  
  # Plot the UpSet diagram.
  plot_object = UpSetR::upset(
    data = data_frame, 
    sets = friendly_categories,
    nintersects = NA,
    sets.bar.color = RColorBrewer::brewer.pal(
      n = length(friendly_categories), 
      name = "Accent"),
    matrix.color = "black",
  #  order.by = "freq",
    mb.ratio = c(0.3, 0.7),
    keep.order = TRUE,
    set_size.show	= TRUE)

  # Was required in CRAN version of UpSetR:  
  #upset_plot = ggplot2::last_plot();
  # Reference: https://github.com/hms-dbmi/UpSetR/pull/100
  
  return(plot_object);
}

print("IAMPerf2020 environment loaded.")

# References
#
# I list here a bunch of articles that have been inspiring or helpful while writing this notebook.
#
# * https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github
# * https://www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf

