
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

# ******************************
# * Association Q20 versus Q24 *
# ******************************

# Team Dedication: Survey Question Q20
# CMM: Survey Question Q24

# Enhanced approach used to generate faceted Bar Charts
# along 2 dimensions: domain x specialization.
prepare_data_q20_association_q24 = function(){
  
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

plot_q20_association_q24_facetedbarchart = function(){
  
  plot_data = prepare_data_q20_association_q24();
  
  plot_barchart_gradients_dodged_series(
    title = "Specialization versus Capability Maturity",
    subtitle = "This faceted bar chart shows the capability maturity level distributions of survey answers organized by domains and level of specialization",
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

plot_q20_association_q24_facetedbarchart();
#save_plot(
#  plot_object = plot_hypothesis_csp_greater_pammaturity(),
#  file_name = "IAMPerf2020-Q20-Association-Q24-FacetedBarChart.png",
#  width = 11);

test_q20_association_q24_kendall = function(){
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    cat("\n", as.character(iamperf2020_q24_domains[domain_index, c("Title")]), ":\n");
    # Retrieve survey data
    domain_data = data.frame(iamperf2020_survey[paste0("Q24R", domain_index)], iamperf2020_survey[paste0("Q20R", domain_index)]);
    colnames(domain_data) = c("capability_maturity", "specialization");
    # Remove NAs
    domain_data = domain_data[!is.na(domain_data$capability_maturity),];
    domain_data = domain_data[!is.na(domain_data$specialization),];
    print(Kendall::Kendall(domain_data$capability_maturity, domain_data$specialization));
  }
}

test_q20_association_q24_kendall()

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
    
    cat("\n.................................................");
    cat("\n\n", domain_title);
    
    # PREDOMINANTLY DEDICATED / SPECIALIZED
    cat("\n\nSPECIALIZED\n\n");
    test_hypothesis_ordinal_greater(
      sample_1 = as.numeric(ifelse(specialization_filter_dedicated, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "greater"
      );
    
    cat("\n");
    
    test_hypothesis_ordinal_greater(
      sample_1 = as.numeric(ifelse(specialization_filter_dedicated, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "less"
    );

    cat("\n\nPOLYVALENT\n\n");
    test_hypothesis_ordinal_greater(
      sample_1 = as.numeric(ifelse(specialization_filter_shared, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "greater"
    );
    
    cat("\n");
    
    test_hypothesis_ordinal_greater(
      sample_1 = as.numeric(ifelse(specialization_filter_shared, iamperf2020_survey[, domain_column], NA)),
      sample_2 = as.numeric(iamperf2020_survey[, domain_column]),
      alternative = "less"
      );
    
  }
  
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

library(dplyr);
test_hypothesis_fs_greater_iammaturity()



# ******************************
# * Association Q23 versus Q24 *
# ******************************

# IAM Strategic Strength: infered from Survey Question Q23
# CMM: Survey Question Q24

# Enhanced approach used to generate faceted Bar Charts
# along 2 dimensions: domain x specialization.
prepare_data_q23_association_q24 = function(){
  
  priorities_data = prepare_data_q23_priorities(remove_no_goals = FALSE);
  strategic_levels = c("Weak", "Focused"); # We remove Unknown because the sample is far too small
  
  plot_data = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    
    specialization_column = as.character(iamperf2020_q20_domains$X[domain_index]);
    for(specialization_index_dedicated in 1:2){
      strategic_strength = strategic_levels[specialization_index_dedicated];
      strategic_filter = priorities_data[,"Strategy"] == strategic_strength;
      facet_data = prepare_data_barchart_with_single_column_coercion(
        iamperf2020_survey[
          strategic_filter
          ,domain_column
          ], 
        ordering_option = "level");
      
      facet_data$facet = domain_title;
      facet_data$facet_2 = strategic_strength;
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
    levels = c("Weak", "Focused"),
    ordered = TRUE
  );
  
  return(plot_data);
}

plot_q23_association_q24_facetedbarchart = function(){
  
  plot_data = prepare_data_q23_association_q24();
  
  plot_barchart_gradients_dodged_series(
    title = "Strategic Strength versus Capability Maturity",
    subtitle = "This faceted bar chart shows the capability maturity level distributions of survey answers organized by domains and IAM strategic strength",
    axis_x_title = "Domain",
    axis_y_title = "Capability Maturity",
    plot_data = plot_data, # Pre-summarized data with multiple series
    # Data structure: series, category, count, label
    legend_title = "Legend",
    x_lim_min = NULL,
    x_lim_max = .5,
    faceted = FALSE,
    grid_faceted = TRUE,
    geom_text_angle = 0,
    geom_text_hjust = .5,
    geom_text_vjust = -.3,
    axis_text_x_blank = TRUE
    #ncol = 3
  );
  
}

plot_q23_association_q24_facetedbarchart();


test_q23_association_q24_kendall = function(){
  
  priorities_data = prepare_data_q23_priorities(remove_no_goals = FALSE);
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    cat("\n", as.character(iamperf2020_q24_domains[domain_index, c("Title")]), ":\n");
    # Retrieve survey data
    domain_data = data.frame(
      iamperf2020_survey[paste0("Q24R", domain_index)], 
      priorities_data$Strategy);
    colnames(domain_data) = c("capability_maturity", "strategic_strength");
    # Remove NAs
    domain_data = domain_data[!is.na(domain_data$capability_maturity),];
    domain_data = domain_data[!is.na(domain_data$strategic_strength),];
    print(Kendall::Kendall(domain_data$capability_maturity, domain_data$strategic_strength));
  }
}

test_q23_association_q24_kendall()


# **********************************************
# * Hypothesis Positive Association between CMM and Best Practice Compliance *
# **********************************************

# Survey Questions 27, 28, 29

prepare_data_q27q28q29_association_q24r1 = function(
  na.rm = TRUE
){
  
  # Group best practices-related columns
  best_practices_columns = rbind(
    iamperf2020_q27_best_practices,
    iamperf2020_q28_best_practices,
    iamperf2020_q29_best_practices
  );
  
  # Format it in paired values
  # That is: general CMM level versus best practice compliance level
  paired_values = data.frame();
  cmm_column = "Q24R1";
  for(column_index in 1:nrow(best_practices_columns)){
    best_practice_column = as.character(best_practices_columns[column_index,c("X")]);
    paired_values_subset = iamperf2020_survey[
      ,c(cmm_column, best_practice_column)];
    colnames(paired_values_subset) = c("CMM", "BestPracticeCompliance");
    paired_values = rbind(
      paired_values,
      paired_values_subset
    );
  }
  
  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$CMM),];
    paired_values = paired_values[!is.na(paired_values$BestPracticeCompliance),];
  }
  
  return(paired_values);
}

plot_q27q28q29_association_q24r1_bubblechart = function(){

  paired_values = prepare_data_q27q28q29_association_q24r1(na.rm = TRUE);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = paired_values %>% count("CMM", "BestPracticeCompliance");
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_bubblechart(
    plot_data = group_counts, 
    title = "IAM General Capability Maturity Level vs Performance Measurement Best Practice Compliance (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between IAM general maturity level and degree of compliance with performance measurement best practices",
    x_axis_title = "IAM General Capability Maturity Level",
    y_axis_title = "Degree of compliance"
    );

  return(plot_object);
}

plot_q27q28q29_association_q24r1_bubblechart();
#save_plot(
#  plot_object = plot_q27q28q29_association_q24r1_bubblechart(),
#  file_name = "IAMPerf2020-Q27-Q28-Q29-Association-Q24R1-BubbleChart.png",
#  width = 11);

test_q27q28q29_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q27q28q29_association_q24r1(na.rm = FALSE);
  
  test_kendall_tau(paired_values$CMM, paired_values$BestPracticeCompliance);
}

test_q27q28q29_association_q24r1_kendalltau();


# **********************************************
# * Hypothesis Positive Association between Indicator Role and Best Practice Compliance *
# **********************************************

# Survey Questions 27, 28, 29
# Survey Question 26

prepare_data_q27q28q29_association_q26 = function(
  na.rm = TRUE
){
  
  # Group best practices-related columns
  best_practices_columns = rbind(
    iamperf2020_q27_best_practices,
    iamperf2020_q28_best_practices,
    iamperf2020_q29_best_practices
  );
  
  # Format it in paired values
  # That is: indicator role versus best practice compliance level
  # The trick here is that participants may play multiple roles!
  paired_values = data.frame();
  
  for(role_column_index in 1:nrow(iamperf2020_q26_indicator_roles)){
    role_column = as.character(
      iamperf2020_q26_indicator_roles[role_column_index,c("X")]);
    
    for(best_practice_column_index in 1:nrow(best_practices_columns)){
      best_practice_column = as.character(
        best_practices_columns[best_practice_column_index,c("X")]);
      paired_values_subset = iamperf2020_survey[
        ,c(role_column, best_practice_column)];
      colnames(paired_values_subset) = c("Role", "BestPracticeCompliance");
      paired_values = rbind(
        paired_values,
        paired_values_subset
      );
    }
    
  }
  
  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$Role),];
    paired_values = paired_values[!is.na(paired_values$BestPracticeCompliance),];
  }
  
  return(paired_values);
}

library(dplyr);
plot_q27q28q29_association_q26_bubblechart = function(){
  
  paired_values = prepare_data_q27q28q29_association_q26(na.rm = TRUE);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = count(paired_values, Role, BestPracticeCompliance);
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "Participant Indicator Role vs Performance Measurement Best Practice Compliance (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between the role played by survey participants in relation to IAM performance indicators and the degree of compliance with performance measurement best practices\nThe roles dimension is a non-exclusive categorical variable.",
    x_axis_title = "Indicator Role",
    y_axis_title = "Degree of compliance"
  );
  
  return(plot_object);
}

plot_q27q28q29_association_q24r1_bubblechart();
#save_plot(
#  plot_object = plot_q27q28q29_association_q24r1_bubblechart(),
#  file_name = "IAMPerf2020-Q27-Q28-Q29-Association-Q24R1-BubbleChart.png",
#  width = 11);

test_q27q28q29_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q27q28q29_association_q24r1(na.rm = FALSE);
  
  test_kendall_tau(paired_values$CMM, paired_values$BestPracticeCompliance);
}

test_q27q28q29_association_q24r1_kendalltau();

#######
#     #
# Q30 #
#     #
#######

prepare_data_q30_association_q24r1 = function(
  na.rm = TRUE
){
  
  # Format it in paired values
  paired_values = iamperf2020_survey[,c("Q24R1", "Q30")];
  
  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$Q24R1),];
    paired_values = paired_values[!is.na(paired_values$Q30),];
  }
  
  return(paired_values);
}

plot_q30_association_q24r1_bubblechart = function(){
  
  paired_values = prepare_data_q30_association_q24r1(na.rm = TRUE);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = plyr::count(paired_values, vars = c("Q24R1", "Q30"));
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "IAM General Capability Maturity Level vs Indicator Automation (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between general IAM capability maturity level and indicator automation",
    x_axis_title = "IAM General Capability Maturity Level",
    y_axis_title = "Indicator Automation"
  );
  
  return(plot_object);
}

plot_q30_association_q24r1_bubblechart()

test_q30_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q30_association_q24r1(na.rm = FALSE);
  test_kendall_tau(paired_values[,1], paired_values[,2]);
}

test_q30_association_q24r1_kendalltau();


#######
#     #
# Q31 #
#     #
#######

prepare_data_q31_association_q24r1 = function(
  na.rm = TRUE
){
  
  paired_values = data.frame();
  for(column_index in 1:nrow(iamperf2020_q31_dimensions)){
    columns_pair = c("Q24R1", as.character(iamperf2020_q31_dimensions[column_index, c("X")]));
    cat(columns_pair, "\n");
    dimension_values = iamperf2020_survey[,columns_pair];
    colnames(dimension_values) = c("Q24R1", "Q31");
    paired_values = rbind(
      paired_values,
      dimension_values
      );
  }

  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$Q24R1),];
    paired_values = paired_values[!is.na(paired_values$Q31),];
  }
  
  return(paired_values);
}

plot_q31_association_q24r1_bubblechart = function(){
  
  paired_values = prepare_data_q31_association_q24r1(na.rm = TRUE);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = plyr::count(paired_values, vars = c("Q24R1", "Q31"));
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "IAM General Capability Maturity Level vs Indicator Coverage (Bubble Chart)",
    subtitle = "This bubble chart shows the relationship between general IAM capability maturity level and indicator coverage.\nThe numbers in bubbles and the bubble size is the number of indicator dimensions. The survey question lists 10 indicator dimensions, in consequence each survey answer may provide between 0 and 10 indicator dimensions.",
    x_axis_title = "IAM General Capability Maturity Level",
    y_axis_title = "Coverage"
  );
  
  return(plot_object);
}

plot_q31_association_q24r1_bubblechart()

test_q31_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q31_association_q24r1(na.rm = FALSE);
  test_kendall_tau(paired_values[,1], paired_values[,2]);
}

test_q31_association_q24r1_kendalltau();

#######
#     #
# Q32 #
#     #
#######

prepare_data_q32_association_q24r1 = function(
  na.rm = TRUE
){
  
  # Format it in paired values
  paired_values = iamperf2020_survey[,c("Q24R1", "Q32")];
  
  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$Q24R1),];
    paired_values = paired_values[!is.na(paired_values$Q32),];
  }
  
  return(paired_values);
}

plot_q32_association_q24r1_bubblechart = function(){
  
  paired_values = prepare_data_q32_association_q24r1(na.rm = TRUE);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = plyr::count(paired_values, vars = c("Q24R1", "Q32"));
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "IAM General Capability Maturity Level vs Indicators Placement (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between general IAM capability maturity level and indicator placement (lagging / leading)",
    x_axis_title = "IAM General Capability Maturity Level",
    y_axis_title = "Indicator Placement"
  );
  
  return(plot_object);
}

test_q32_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q32_association_q24r1(na.rm = FALSE);
  colnames(paired_values) = c("CapabilityMaturity", "IndicatorPlacement");
  test_kendall_tau(paired_values$CapabilityMaturity, paired_values$IndicatorPlacement);
}

test_q32_association_q24r1_kendalltau();
