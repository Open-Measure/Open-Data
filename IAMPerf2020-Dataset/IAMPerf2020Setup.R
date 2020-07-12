# Developing an R package just for the IAM Performance Measurement 2020 Survey 
# would have been an overkill. To keep things simpler, I chose to store reusable
# functions in this R script and load it from GitHub with the devtools package.

# Some will noti# ce I don't load libraries. Instead, I prefer to use the unambiguous
# syntax *package::function*. This makes the code slightly harsher to read but this
# is a price I am pleased to pay. 

print("Loading the IAMPerf2020 environment...");

# Packages
if(!require("RCurl")) install.packages("RCurl");
if(!require("ggplot2")) install.packages("ggplot2");
if(!require("RColorBrewer")) install.packages("RColorBrewer");

# Configuration options
iamperf2020_data_url = "https://raw.githubusercontent.com/Open-Measure/Open-Data/master/IAMPerf2020-Dataset/";

# Load the data
iamperf2020_survey <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020.csv")));
iamperf2020_q9_countries <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q9Countries.csv")));
iamperf2020_q10_roles <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q10Roles.csv")));
iamperf2020_q20_domains <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q20Domains.csv")));
iamperf2020_q20_team_dedication <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q20TeamDedication.csv")));
iamperf2020_q23_goals <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q23Goals.csv")));
iamperf2020_q23_priorities <- read.csv (text = RCurl::getURL(paste0(iamperf2020_data_url, "IAMPerf2020Q23Priorities.csv")));

# Q9: Apply nicely labeled and properly unordered factors.
iamperf2020_survey$Q9 = factor(iamperf2020_survey$Q9, levels = iamperf2020_q9_countries$CountryCode, labels = iamperf2020_q9_countries$CountryISO2, ordered = FALSE, exclude = NA);
iamperf2020_survey$Q9B = factor(iamperf2020_survey$Q9B, levels = iamperf2020_q9_countries$CountryCode, labels = iamperf2020_q9_countries$CountryISO2, ordered = FALSE, exclude = NA);

# Q10: Apply nicely labeled and properly unordered factors.
# Values will be NA or 1 (= I hold this role/position).
# Factor labels are configured as the name of the role/position.
# Q19A18 = Other (see Q19_O for the corresponding text values in the non-anonymized dataset).
# Q19A19 = I don't know.
# Because there are 19 columns, I use a loop to apply the configuration dynamically.
for(q10_count in 1:nrow(iamperf2020_q10_roles)){
  q10_column = as.character(iamperf2020_q10_roles[q10_count, "X"]);
  q10_levels = c(1); # Single level :-)
  q10_labels = as.character(iamperf2020_q10_roles[q10_count, "Title"]);
  iamperf2020_survey[,q10_column] = factor(
    iamperf2020_survey[,q10_column], 
    levels = q10_levels, 
    labels = q10_labels, 
    ordered = FALSE, exclude = NA);
};

# Q20: Apply nicely labeled and properly ordered factors.
iamperf2020_survey$Q20R1 = factor(iamperf2020_survey$Q20R1, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R2 = factor(iamperf2020_survey$Q20R2, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R3 = factor(iamperf2020_survey$Q20R3, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R4 = factor(iamperf2020_survey$Q20R4, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R5 = factor(iamperf2020_survey$Q20R5, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);
iamperf2020_survey$Q20R6 = factor(iamperf2020_survey$Q20R6, levels = iamperf2020_q20_team_dedication$X, labels = iamperf2020_q20_team_dedication$Title, ordered = TRUE, exclude = NA);

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

print("IAMPerf2020 environment loaded.")

# References
#
# I list here a bunch of articles that have been inspiring or helpful while writing this notebook.
#
# * https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github
# * https://www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf

