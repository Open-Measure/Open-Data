
if(!require("stats")) install.packages("stats");
if(!require("Partiallyoverlapping")) install.packages("Partiallyoverlapping");
test_hypothesis_ordinal_greater = function(
  original_hypothesis = NULL,
  h0,
  ha,
  sample_1,
  sample_2,
  alternative = "greater"
) {
  
  cat("PARTIALLY OVERLAPPING SAMPLES T-TEST as per Derrick et al. (2017)\n")
  # Bibliography:
  # Derrick, B., White, P., 2018. Methods for comparing the responses from a Likert question, with paired observations and independent observations in each of two samples 10.
  cat("Execution of the statistical test using the 'Partover.test' function of the 'Partiallyoverlapping' R package.\n");
  
  if(!is.null(original_hypothesis)){
    cat("Original hypothesis:", original_hypothesis, "\n");
  }
  # This transformation is acceptable because the Wilcoxon test accepts ordinal variables.
  cat("H0: ", h0, "\n");
  cat("Ha: ", ha, "\n"); 
 
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
  
  derrick_test_outcome = Partiallyoverlapping::Partover.test(
    x1 = as.numeric(sample_1), 
    x2 = as.numeric(sample_2),
    alternative = alternative,
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
      cat("The test is statistically significant.\n"); 
    } else {
      cat("The test is not statistically significant.\n");
    };
  
}

# **********************************************
# * Hypothesis IAM CMM is normally distributed *
# **********************************************

# Survey Question 24

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

# ****************************************
# * Comparing Team Specialization with Capability Maturity *
# ****************************************

# Team Dedication: Survey Question Q20
# CMM: Survey Question Q24

# ARCHIVED: THIS FUNCTION IS NO LONGER USED
# Approach used to generate Bubble Charts.
prepare_facet_data_hypothesis_association_dedication_cmm = function(
  column_index,
  remove_na = TRUE,
  summarize = TRUE){
  column_1 = paste("Q20R", column_index, sep = ""); # Team Dedication
  column_2 = paste("Q24R", column_index, sep = ""); # Capability Maturity
  
  data_subset = iamperf2020_survey[,c(column_1, column_2)];
  colnames(data_subset) = c("TeamDedication", "CapabilityMaturity");
  
  if(remove_na){
    # Remove NAs
    data_subset = data_subset[
      !is.na(data_subset$TeamDedication) & 
        !is.na(data_subset$CapabilityMaturity),];
  }
  
  if(summarize){
    # Summarizes and counts 
    data_subset = as.data.frame(table(data_subset));
    
    data_count_label = paste("(", data_subset$Freq, ")", sep = "");
    data_count_total = sum(data_subset$Freq);
    data_subset$Ratio = data_subset$Freq / data_count_total;
    data_ratio_label = paste(rounded_ratios_with_largest_remainder(data_subset$Ratio, digits = 1), "%");
    data_subset$Label = paste(data_ratio_label, data_count_label, sep = "\n");
    
    data_subset$Facet = as.character(
      iamperf2020_q24_domains$Title[
        iamperf2020_q24_domains$X == column_2]);
  }

  return(data_subset);
}

# ARCHIVED: THIS FUNCTION IS NO-LONGER USED.
plot_hypothesis_association_dedication_cmm_bubblechart = function(){
  
  plot_data = 
    rbind(
      prepare_facet_data_hypothesis_association_dedication_cmm(1, TRUE, TRUE),
      prepare_facet_data_hypothesis_association_dedication_cmm(2, TRUE, TRUE),
      prepare_facet_data_hypothesis_association_dedication_cmm(3, TRUE, TRUE),
      prepare_facet_data_hypothesis_association_dedication_cmm(4, TRUE, TRUE),
      prepare_facet_data_hypothesis_association_dedication_cmm(5, TRUE, TRUE),
      prepare_facet_data_hypothesis_association_dedication_cmm(6, TRUE, TRUE)
    );
  
  plot_object <- ggplot(
    plot_data, 
    aes(
      x = CapabilityMaturity, 
      y = TeamDedication, 
      label = Label,
      size = Ratio, 
      colour = Ratio
    )
  ) +
    geom_point(
      alpha = 1
    ) +
    geom_text(
      color = "#000000", 
      size = 3, 
      nudge_x = -.43, 
      angle = 90) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position = "none"
    ) +
    facet_wrap(~ Facet, ncol = 2) +
    scale_size(range = c(.1, 24), name = "Frequency") +
    scale_color_viridis(discrete = FALSE, direction = -1) +
    labs(
      subtitle = "This bubble chart shows the association between team dedication and capability maturity ordinal categories.", 
      x = "Capability Maturity", 
      y = "Team Dedication", 
      title = "Association between Team Dedication and Capability Maturity"
    );
  
  return(plot_object);
}

# Enhanced approach used to generate faceted Bar Charts
# along 2 dimensions: domain x specialization.
prepare_data_hypothesis_specializations_cmm_barchart = function(){
  
  plot_data = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    specialization_column = as.character(iamperf2020_q20_domains$X[domain_index]);
    for(specialization_index_dedicated in 1:nrow(iamperf2020_q20_team_dedication)){
      specialization_level = as.character(iamperf2020_q20_team_dedication$Title[specialization_index_dedicated]);
      specialization_filter = iamperf2020_survey[,specialization_column] == specialization_level;
      facet_data = prepare_data_barchart_with_single_column_coercion(
        iamperf2020_survey[
          specialization_filter
          ,domain_column
          ], 
        ordering_option = "level");
      
      facet_data$facet = domain_title;
      facet_data$facet_2 = specialization_level;
      facet_data$series = facet_data$category; #"Capability Maturity Level";
      #facet_data$value = facet_data$count;
      facet_data$value = facet_data$frequency;

      plot_data = rbind(
        plot_data,
        facet_data
      );
      
    }
  }
  
  plot_data = plyr::arrange(plot_data, facet, facet_2, category, series);
  
  # We want ordered factors
  # factor order will be re-used in GGPlot2
  plot_data$facet = factor(
    plot_data$facet,
    levels = c(
      "IAM (general answer)", "Workforce IAM", "PAM / TAM",
      "Customer IAM", "3rd Party IAM", "IoT IAM")
    ); 
  plot_data$facet_2 = factor(
    plot_data$facet_2, 
    levels = c( "Predominantly shared", "Balanced", "Predominantly dedicated"),
    ordered = TRUE
    );
  
  return(plot_data);
}

test_hypothesis_specializations_cmm_derrick = function(){
  
  table_result = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    specialization_column = as.character(iamperf2020_q20_domains$X[domain_index]);
    specialization_index_dedicated = 3;
    specialization_level_dedicated = as.character(iamperf2020_q20_team_dedication$Title[specialization_index_dedicated]);
    specialization_filter_dedicated = iamperf2020_survey[,specialization_column] == specialization_level_dedicated;
    specialization_index_shared = 1;
    specialization_level_shared = as.character(iamperf2020_q20_team_dedication$Title[specialization_index_shared]);
    specialization_filter_shared = iamperf2020_survey[,specialization_column] == specialization_level_shared;
    
    # PREDOMINANTLY DEDICATED
    test_hypothesis_ordinal_greater(
      h0 = paste(
        "The mean capability maturity level of organizations having predominantly specialized / dedicated teams in ", 
        domain_title,
        " is not higher than average.",
        sep = ""),
      ha = paste(
        "The mean capability maturity level of organizations having predominantly specialized / dedicated teams in ", 
        domain_title,
        " is higher than average.",
        sep = ""),
      sample_1 = as.numeric(ifelse(specialization_filter_dedicated, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column])
      );

    cat(".................................................", "\n\n", sep = "");
    
    test_hypothesis_ordinal_greater(
      h0 = paste(
        "The mean capability maturity level of organizations having predominantly polyvalent / shared teams in ", 
        domain_title,
        " is not lower than average.",
        sep = ""),
      ha = paste(
        "The mean capability maturity level of organizations having predominantly polyvalent / shared teams in ", 
        domain_title,
        " is lower than average.",
        sep = ""),
      sample_1 = as.numeric(ifelse(specialization_filter_shared, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "less"
      );
    
    cat(".................................................", "\n\n", sep = "");
    
    
  }
  
}

plot_hypothesis_association_dedication_cmm_barchart = function(){
  
  plot_data = prepare_data_hypothesis_specializations_cmm_barchart();
  
  plot_barchart_gradients_dodged_series(
    title = "Comparing Specialization with Capability Maturity",
    subtitle = "This faceted bar chart shows the capability maturity distributions of survey answers organized by domains and level of specialization",
    axis_x_title = "Domain",
    axis_y_title = "Capability Maturity",
    plot_data = plot_data, # Pre-summarized data with multiple series
    # Data structure: series, category, count, label
    legend_title = "Legend",
    x_lim_min = NULL,
    x_lim_max = NULL,
    faceted = FALSE,
    grid_faceted = TRUE,
    geom_text_angle = 0,
    geom_text_hjust = .5,
    geom_text_vjust = -.3,
    axis_text_x_blank = TRUE
    #ncol = 3
  );
  
}

test_hypothesis_association_dedication_cmm = function(){

  #test_report = "";
  
  for(domain_index in 1 : 6){
    domain_title = as.character(iamperf2020_q21_domains$Title[domain_index]);
    cat(domain_title, "\n");
    test_data = prepare_facet_data_hypothesis_association_dedication_cmm(domain_index, FALSE, FALSE);
    test_subreport = test_kendall_tau(
      test_data$TeamDedication,
      test_data$CapabilityMaturity
    );
    plot(test_data);

    #test_report = paste(
    #  test_report,
    #  paste(domain_title, test_subreport, sep = "\n"),
    #  sep = "\n\n"
    #);
  }
    
  #return(test_report);
}

test_hypothesis_association_dedication_cmm();

# ***************************************************
# * hypothesis centralization association with capability maturity
# ******************************************

prepare_data_hypothesis_centralization_cmm_barchart = function(){
  
  plot_data = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    centralization_column = as.character(iamperf2020_q21_domains$X[domain_index]);
    for(centralization_index_dedicated in 1:nrow(iamperf2020_q21_centralization)){
      centralization_level = as.character(iamperf2020_q21_centralization$Title[centralization_index_dedicated]);
      centralization_filter = iamperf2020_survey[,centralization_column] == centralization_level;
      facet_data = prepare_data_barchart_with_single_column_coercion(
        iamperf2020_survey[
          centralization_filter
          ,domain_column
          ], 
        ordering_option = "level");
      
      facet_data$facet = domain_title;
      facet_data$facet_2 = centralization_level;
      facet_data$series = facet_data$category; #"Capability Maturity Level";
      #facet_data$value = facet_data$count;
      facet_data$value = facet_data$frequency;
      
      plot_data = rbind(
        plot_data,
        facet_data
      );
      
    }
  }
  
  plot_data = plyr::arrange(plot_data, facet, facet_2, category, series);
  
  # We want ordered factors
  # factor order will be re-used in GGPlot2
  plot_data$facet = factor(
    plot_data$facet,
    levels = c(
      "IAM (general answer)", "Workforce IAM", "PAM / TAM",
      "Customer IAM", "3rd Party IAM", "IoT IAM")
  ); 
  plot_data$facet_2 = factor(
    plot_data$facet_2, 
    levels = c( "Decentralized", "Hybrid", "Centralized"),
    ordered = TRUE
  );
  
  return(plot_data);
}

test_hypothesis_centralizations_cmm_derrick = function(){
  
  table_result = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    centralization_column = as.character(iamperf2020_q21_domains$X[domain_index]);
    centralization_index_dedicated = 3;
    centralization_level_dedicated = as.character(iamperf2020_q21_centralization$Title[centralization_index_dedicated]);
    centralization_filter_dedicated = iamperf2020_survey[,centralization_column] == centralization_level_dedicated;
    centralization_index_shared = 1;
    centralization_level_shared = as.character(iamperf2020_q21_centralization$Title[centralization_index_shared]);
    centralization_filter_shared = iamperf2020_survey[,centralization_column] == centralization_level_shared;
    
    # PREDOMINANTLY DEDICATED
    test_hypothesis_ordinal_greater(
      h0 = paste(
        "The mean capability maturity level of organizations having centralized teams in ", 
        domain_title,
        " is not higher than average.",
        sep = ""),
      ha = paste(
        "The mean capability maturity level of organizations having centralized teams in ", 
        domain_title,
        " is higher than average.",
        sep = ""),
      sample_1 = as.numeric(ifelse(centralization_filter_dedicated, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column])
    );
    
    cat(".................................................", "\n\n", sep = "");
    
    test_hypothesis_ordinal_greater(
      h0 = paste(
        "The mean capability maturity level of organizations having predominantly decentralized teams in ", 
        domain_title,
        " is not lower than average.",
        sep = ""),
      ha = paste(
        "The mean capability maturity level of organizations having predominantly decentralized teams in ", 
        domain_title,
        " is lower than average.",
        sep = ""),
      sample_1 = as.numeric(ifelse(centralization_filter_shared, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "less"
    );
    
    cat(".................................................", "\n\n", sep = "");
    
    
  }
  
}

plot_hypothesis_association_centralization_cmm_barchart = function(){
  
  plot_data = prepare_data_hypothesis_centralization_cmm_barchart();
  
  plot_barchart_gradients_dodged_series(
    title = "Comparing Centralization with Capability Maturity",
    subtitle = "This faceted bar chart shows the capability maturity distributions of survey answers organized by domains and level of centralization",
    axis_x_title = "Domain",
    axis_y_title = "Capability Maturity",
    plot_data = plot_data, # Pre-summarized data with multiple series
    # Data structure: series, category, count, label
    legend_title = "Legend",
    x_lim_min = NULL,
    x_lim_max = NULL,
    faceted = FALSE,
    grid_faceted = TRUE,
    geom_text_angle = 0,
    geom_text_hjust = .5,
    geom_text_vjust = -.3,
    axis_text_x_blank = TRUE
    #ncol = 3
  );
  
}



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

test_hypothesis_fs_greater_iammaturity()


