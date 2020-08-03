
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

prepare_data_hypothesis_csp_greater_maturity = function(){
  
  # Retrieve the boolean vector of CSP participants.
  csp_filter = iamperf2020_survey$Q17A5 == "Cloud Services";
  
  # Retrieve the vector PAM/TAM Capability Maturity Levels.
  pamtam_cmm_factor = iamperf2020_survey$Q24R6;
  
  # Prepare the two samples.
  csp_factor = pamtam_cmm_factor[csp_filter];
  all_factor = pamtam_cmm_factor;
  
  prepared_data = data.frame(
    csp_factor = csp_factor,
    all_factor = all_factor
    );

  return(prepared_data);
}

plot_hypothesis_csp_greater_maturity = function(){
  
  plot_data = prepare_data_hypothesis_csp_greater_maturity();
  
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
  
  plot_data = 
    plyr::arrange(plot_data, category, series);
  
  plot_barchart_gradients_dodged_series(
    title = "Distributions of Cloud Service Providers vs All Organizations PAM/TAM Capability Maturity Levels",
    subtitle = "This bar chart shows both the PAM/TAM capability maturity levels of Cloud Service Providers and of all organizations",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "PAM/TAM Capability Maturity Levels",
    plot_data = plot_data, 
    legend_title = "Legend"
  );
}

save_plot(
  plot_object = plot_hypothesis_csp_greater_maturity(),
  file_name = "IAMPerf2020-Hypothesis-CSP-Greater-PAMMaturity-BarChart.png",
  width = 11);

test_hypothesis_csp_greater_maturity = function(){
  
  original_hypothesis = "Cloud Service Providers have a higher PAM/TAM Capability Maturity Level than average organizations.";
  h0 = "Cloud Service Providers do not have a higher PAM/TAM Capability Maturity Level than average organizations.";
  ha = "Cloud Service Providers have a higher PAM/TAM Capability Maturity Level than average organizations."; 
  
  test_hypothesis_ordinal_greater(
    original_hypothesis = original_hypothesis,
    h0 = h0,
    ha = ha,
    sample_1 = prepare_data_hypothesis_csp_greater_maturity()$csp_factor,
    sample_2 = prepare_data_hypothesis_csp_greater_maturity()$all_factor
    );
}

test_hypothesis_csp_greater_maturity();

# Hypothesis 3: (?????) Organizations active in the financial services sector 
# have a higher IAM Capability Maturity Level than average organizations.

View(iamperf2020_questions);

# Industrial sectors: Q17
View(iamperf2020_q17_industrial_sectors);
# Q17A5	Cloud Services
# Q17A22 Financial Services

financial_services = iamperf2020_survey$Q17A22 == "Financial Services";

# IAM Capability Maturity Level: General: Q24R1
iam_maturity = iamperf2020_survey$Q24R1;

# 3rd Party (FIM) Capability Maturity Level: General: Q24R3


# PAM/TAM Capability Maturity Level: General: Q24R6

plot(pamtam_cmm)
plot(pamtam_cmm[cloud_services])

mean(as.numeric(pamtam_cmm[cloud_services]), na.rm = TRUE)
mean(as.numeric(pamtam_cmm), na.rm = TRUE)

