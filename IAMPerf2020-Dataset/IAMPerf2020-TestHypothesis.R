
if(!require("stats")) install.packages("stats");
if(!require("Partiallyoverlapping")) install.packages("Partiallyoverlapping");
test_hypothesis_ordinal_greater = function(
  original_hypothesis,
  h0,
  ha,
  sample_1,
  sample_2
) {
  
  cat("Original hypothesis:", original_hypothesis, "\n\n");
  # This transformation is acceptable because the Wilcoxon test accepts ordinal variables.
  cat("H0:", h0, "\n");
  cat("Ha:", ha, "\n\n"); 
 
  # "Naive" approache with Wilcoxon test. Later discarded.
  #cat("WILCOXON SIGNED RANK SUM TEST (WSRST) \n\n");
  ## Execute the Wilcoxon test.
  #wilcox_test_outcome = wilcox.test(
  #  x = as.numeric(sample_1), 
  #  y = as.numeric(sample_2),
  #  paired = TRUE, 
  #  #exact = FALSE, # An exact p-value cannot be computed with zeroes
  #  conf.int = TRUE,
  #  conf.level = .95,
  #  alternative = "greater" # See Ha above.
  #  );
  ## Print the test report.
  #cat("Execute the statistical test using the 'wilcox.test' function of the 'stats' R package:\n")
  #print(wilcox_test_outcome, row.names = FALSE);
  ## Enrich the report with user-friendly outputs.
  #if(wilcox_test_outcome$p.value < .05)
  #  { 
  #    cat("The test is statistically significant.\n\n"); 
  #  } else {
  #    cat("The test is not statistically significant.\n\n");
  #  };
  
  cat("PARTIALLY OVERLAPPING SAMPLES T-TEST as per Derrick et al. (2017)\n\n")
  # Bibliography:
  # Derrick, B., White, P., 2018. Methods for comparing the responses from a Likert question, with paired observations and independent observations in each of two samples 10.
  cat("Execute the statistical test using the 'Partover.test' function of the 'Partiallyoverlapping' R package:\n")
  
  derrick_test_outcome = Partiallyoverlapping::Partover.test(
    x1 = as.numeric(sample_1), 
    x2 = as.numeric(sample_2),
    alternative = "greater",
    conf.level = .95,
    stacked = TRUE
    );
  
  #print(derrick_test_outcome);
  # Print a user-friendly version of the test outcome:
  cat("The value of the t-statistic:", derrick_test_outcome$statistic, "\n");
  cat("The degrees of freedom for the test statistic:", derrick_test_outcome$parameter, "\n");
  cat("The p-value for the test:", derrick_test_outcome$p.value, "\n");
  cat("The estimated difference in the means:", derrick_test_outcome$estimate, "\n");
  cat("A confidence interval for the mean difference appropriate to the specified alternative hypothesis:", derrick_test_outcome$conf.int, "\n\n");
  if(derrick_test_outcome$p.value < .05)
    { 
      cat("The test is statistically significant.\n\n"); 
    } else {
      cat("The test is not statistically significant.\n\n");
    };
  
}

# **********************************************
# * Hypothesis IAM CMM is normally distributed *
# **********************************************

# Question 24

prepare_data_hypothesis_iam_cmm_normal = function(){
  
  plot_data = data.frame();
  
  for(column_index in 1:nrow(iamperf2020_q24_domains)){
    
    facet_data = prepare_data_barchart_with_single_column_coercion(
      iamperf2020_survey[,as.character(iamperf2020_q24_domains$X[column_index])], 
      ordering_option = "level");
    
    facet_data$facet = iamperf2020_q24_domains$Title[column_index];
    facet_data$series = facet_data$category; # "Capability Maturity Level";
    facet_data$value = facet_data$count;

    plot_data = rbind(
      plot_data,
      facet_data
      );
    
  }

  plot_data = plyr::arrange(plot_data, facet, category, series);
    
  return(plot_data);
}

plot_hypothesis_cmm_normal = function(){
  
  plot_object = plot_barchart_gradients_dodged_series(
    title = "Distributions of Capability Maturity Levels for IAM and its Sub-Domains",
    subtitle = "This faceted bar chart shows the distributions of capability maturity levels for IAM and its sub-domains",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "Capability Maturity Levels",
    plot_data = prepare_data_hypothesis_iam_cmm_normal(), 
    legend_title = "Legend",
    faceted = TRUE
  );
  
  return(plot_object);
}

save_plot(
  plot_object = plot_hypothesis_cmm_normal(),
  file_name = "IAMPerf2020-Hypothesis-CMM-Normal-BarChart.png",
  width = 11);

# **************************************
# * hypothesis_csp_greater_pammaturity *
# **************************************

prepare_data_hypothesis_csp_greater_pammaturity = function(){
  
  # Retrieve the boolean vector of CSP participants.
  csp_filter = iamperf2020_survey$Q17A5 == "Cloud Services";
  
  # Retrieve the vector PAM/TAM Capability Maturity Levels.
  pamtam_cmm_factor = iamperf2020_survey$Q24R6;
  
  # Prepare the two samples.
  csp_factor = ifelse(csp_filter, as.character(pamtam_cmm_factor), NA);
  csp_factor = factor(csp_factor, levels = levels(pamtam_cmm_factor), ordered = TRUE);
  all_factor = pamtam_cmm_factor;
  
  prepared_data = data.frame(
    csp_factor = csp_factor,
    all_factor = all_factor
    );

  return(prepared_data);
}

plot_hypothesis_csp_greater_pammaturity = function(){
  
  plot_data = prepare_data_hypothesis_csp_greater_pammaturity();
  
  csp_series = prepare_data_barchart_with_single_column_coercion(
    plot_data$csp_factor, ordering_option = "level");
  csp_series$series = "Cloud Service Providers";
  csp_series$value = csp_series$frequency;

  all_series = prepare_data_barchart_with_single_column_coercion(
    plot_data$all_factor, ordering_option = "level");
  all_series$series = "All organizations";
  all_series$value = all_series$frequency;

  plot_data = 
    rbind(
      csp_series,
      all_series
    );
  
  plot_data = plyr::arrange(plot_data, category, series);
  
  plot_object = plot_barchart_gradients_dodged_series(
    title = "Distributions of Cloud Service Providers vs All Organizations PAM/TAM Capability Maturity Levels",
    subtitle = "This bar chart shows both the PAM/TAM capability maturity levels of Cloud Service Providers and of all organizations",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "PAM/TAM Capability Maturity Levels",
    plot_data = plot_data, 
    legend_title = "Legend"
  );
  
  return(plot_object);
}

save_plot(
  plot_object = plot_hypothesis_csp_greater_pammaturity(),
  file_name = "IAMPerf2020-Hypothesis-CSP-Greater-PAMMaturity-BarChart.png",
  width = 11);

test_hypothesis_csp_greater_pammaturity = function(){
  
  original_hypothesis = "Cloud Service Providers have a higher PAM/TAM Capability Maturity Level than average organizations.";
  h0 = "Cloud Service Providers do not have a higher PAM/TAM Capability Maturity Level than average organizations.";
  ha = "Cloud Service Providers have a higher PAM/TAM Capability Maturity Level than average organizations."; 
  
  test_hypothesis_ordinal_greater(
    original_hypothesis = original_hypothesis,
    h0 = h0,
    ha = ha,
    sample_1 = prepare_data_hypothesis_csp_greater_pammaturity()$csp_factor,
    sample_2 = prepare_data_hypothesis_csp_greater_pammaturity()$all_factor
    );
}

test_hypothesis_csp_greater_pammaturity();

# **************************************
# * hypothesis_csp_greater_fimmaturity *
# **************************************

prepare_data_hypothesis_csp_greater_fimmaturity = function(){
  
  # Retrieve the boolean vector of CSP participants.
  csp_filter = iamperf2020_survey$Q17A5 == "Cloud Services";
  
  # Retrieve the vector 3rd Party IAM Capability Maturity Levels.
  fim_cmm_factor = iamperf2020_survey$Q24R3;
  
  # Prepare the two samples.
  csp_factor = ifelse(csp_filter, as.character(fim_cmm_factor), NA);
  csp_factor = factor(csp_factor, levels = levels(fim_cmm_factor), ordered = TRUE);
  
  all_factor = fim_cmm_factor;
  
  prepared_data = data.frame(
    csp_factor = csp_factor,
    all_factor = all_factor
  );
  
  return(prepared_data);
}

plot_hypothesis_csp_greater_fimmaturity = function(){
  
  plot_data = prepare_data_hypothesis_csp_greater_fimmaturity();
  
  csp_series = prepare_data_barchart_with_single_column_coercion(
    plot_data$csp_factor, ordering_option = "level");
  csp_series$series = "Cloud Service Providers";
  csp_series$value = csp_series$frequency;
  
  all_series = prepare_data_barchart_with_single_column_coercion(
    plot_data$all_factor, ordering_option = "level");
  all_series$series = "All organizations";
  all_series$value = all_series$frequency;
  
  plot_data = 
    rbind(
      csp_series,
      all_series
    );
  
  plot_data = plyr::arrange(plot_data, category, series);
  
  plot_object = plot_barchart_gradients_dodged_series(
    title = "Distributions of Cloud Service Providers vs All Organizations 3rd Party IAM Capability Maturity Levels",
    subtitle = "This bar chart shows both the 3rd Party IAM capability maturity levels of Cloud Service Providers and of all organizations",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "3rd Party IAM Capability Maturity Levels",
    plot_data = plot_data, 
    legend_title = "Legend"
  );
  
  return(plot_object);
}

save_plot(
  plot_object = plot_hypothesis_csp_greater_fimmaturity(),
  file_name = "IAMPerf2020-Hypothesis-CSP-Greater-FIMMaturity-BarChart.png",
  width = 11);

test_hypothesis_csp_greater_fimmaturity = function(){
  
  original_hypothesis = "Cloud Service Providers have a higher 3rd Party IAM Capability Maturity Level than average organizations.";
  h0 = "Cloud Service Providers do not have a higher 3rd Party IAM Capability Maturity Level than average organizations.";
  ha = "Cloud Service Providers have a higher 3rd Party IAM Capability Maturity Level than average organizations."; 
  
  test_hypothesis_ordinal_greater(
    original_hypothesis = original_hypothesis,
    h0 = h0,
    ha = ha,
    sample_1 = prepare_data_hypothesis_csp_greater_fimmaturity()$csp_factor,
    sample_2 = prepare_data_hypothesis_csp_greater_fimmaturity()$all_factor
  );
}

test_hypothesis_csp_greater_fimmaturity();

# *************************************
# * hypothesis_fs_greater_iammaturity *
# *************************************

prepare_data_hypothesis_fs_greater_iammaturity = function(){
  
  # Retrieve the boolean vector of FS (Financial Services) participants.
  fs_filter = iamperf2020_survey$Q17A22 == "Financial Services";
  
  # Retrieve the vector PAM/TAM Capability Maturity Levels.
  iam_cmm_factor = iamperf2020_survey$Q24R1;

  # Prepare the two samples.
  fs_factor = ifelse(fs_filter, as.character(iam_cmm_factor), NA);
  fs_factor = factor(fs_factor, levels = levels(iam_cmm_factor), ordered = TRUE);
  all_factor = iam_cmm_factor;
  
  prepared_data = data.frame(
    fs_factor = fs_factor,
    all_factor = all_factor
  );
  
  return(prepared_data);
}

plot_hypothesis_fs_greater_iammaturity = function(){
  
  raw_data = prepare_data_hypothesis_fs_greater_iammaturity();
  
  fs_series = prepare_data_barchart_with_single_column_coercion(
    raw_data$fs_factor, ordering_option = "level");
  fs_series$series = "Financial Services";
  fs_series$value = fs_series$frequency;
  
  all_series = prepare_data_barchart_with_single_column_coercion(
    raw_data$all_factor, ordering_option = "level");
  all_series$series = "All organizations";
  all_series$value = all_series$frequency;
  
  plot_data = 
    rbind(
      fs_series,
      all_series
    );
  
  plot_data = plyr::arrange(plot_data, category, series);
  
  plot_object = plot_barchart_gradients_dodged_series(
    title = "Distributions of organizations active in the Financial Services sector vs all Organizations IAM Capability Maturity Levels",
    subtitle = "This bar chart shows both the IAM (general) capability maturity levels of organizations active in the Financial Services sector and of all organizations",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "IAM (General) Capability Maturity Levels",
    plot_data = plot_data, 
    legend_title = "Legend"
  );
  
  return(plot_object);
}

save_plot(
  plot_object = plot_hypothesis_fs_greater_iammaturity(),
  file_name = "IAMPerf2020-Hypothesis-FS-Greater-IAMMaturity-BarChart.png",
  width = 11);

test_hypothesis_fs_greater_iammaturity = function(){
  
  original_hypothesis = "Organizations active in the Financial Services sector have a higher IAM (General) Capability Maturity Level than average organizations.";
  h0 = "Organizations active in the Financial Services sector do not have a higher IAM (General) Capability Maturity Level than average organizations.";
  ha = "Organizations active in the Financial Services sector have a higher IAM (General) Capability Maturity Level than average organizations."; 
  
  test_hypothesis_ordinal_greater(
    original_hypothesis = original_hypothesis,
    h0 = h0,
    ha = ha,
    sample_1 = prepare_data_hypothesis_fs_greater_iammaturity()$fs_factor,
    sample_2 = prepare_data_hypothesis_fs_greater_iammaturity()$all_factor
  );
}

test_hypothesis_fs_greater_iammaturity();
