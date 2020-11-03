
# **********************************************
# * Hypothesis IAM CMM is normally distributed *
# **********************************************

# Survey Question 24

prepare_data_hypothesis_iam_cmm_normal = function(){
  
  plot_data = data.frame();
  pline_data = NULL;
  
  for(column_index in 1:nrow(iamperf2020_q24_domains)){
    
#    for(level_index in 1:nrow(iamperf2020_q24_maturity_levels)){
#      
#    }
    
    facet_data = prepare_data_barchart_with_single_column_coercion(
      iamperf2020_survey[,as.character(iamperf2020_q24_domains$X[column_index])], 
      ordering_option = "level");
    
    facet_data$facet = iamperf2020_q24_domains$Title[column_index];
    facet_data$series = facet_data$category; # "Capability Maturity Level";
    facet_data$value = facet_data$count;
    
    #if(column_index == 1){ 
    #  pline_data = facet_data$value; 
    #  facet_data$pline_value = rep(NA, nrow(facet_data));
    #} else {
    #  facet_data$pline_value = pline_data;
    #  }

    plot_data = rbind(
      plot_data,
      facet_data
      );
    
  }

  plot_data = plyr::arrange(plot_data, facet, category, series);
    
  # Put capability levels on two lines for better readability of the chart.
  plot_data$category = plyr::mapvalues(
    plot_data$category, 
    from = c("Level 1: Initial", "Level 2: Repeatable", "Level 3: Defined", "Level 4: Managed", "Level 5: Optimized"), 
    to = c("Level 1\nInitial", "Level 2\nRepeatable", "Level 3\nDefined", "Level 4\nManaged", "Level 5\nOptimized"))
  
  return(plot_data);
}

plot_hypothesis_cmm_normal = function(){
  
  plot_object = plot_barchart_gradients_dodged_series(
    title = "Distributions of Capability Maturity Levels for IAM and its Sub-Domains (Faceted Bar Chart)",
    subtitle = "This faceted bar chart shows the distributions of capability maturity levels for IAM and its sub-domains",
    axis_x_title = "Distribution of Sample Answers (Percentage)",
    axis_y_title = "Capability Maturity Levels",
    plot_data = prepare_data_hypothesis_iam_cmm_normal(), 
    legend_title = "Legend",
    faceted = TRUE,
    display_pline = FALSE
  );
  
  return(plot_object);
}

# File: IAMPerf2020_Q24_CapabilityMaturity_FacetedBarChart.png
plot_hypothesis_cmm_normal();

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
    x_lim_max = .6,
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

# Question Q21


#      "IAM (general answer)", "Workforce IAM", "PAM / TAM",
#      "Customer IAM", "3rd Party IAM", "IoT IAM")

 
prepare_data_q21_association_q24 = function(){
  
  plot_data = data.frame();
  
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    domain_column = as.character(iamperf2020_q24_domains$X[domain_index]);
    domain_title = as.character(iamperf2020_q24_domains$Title[domain_index]);
    # Hopefuly, domain indexes are identical.
    centralization_column = as.character(iamperf2020_q21_domains$X[domain_index]);
    for(centralization_index in 1:nrow(iamperf2020_q21_centralization)){
      centralization_level = as.character(iamperf2020_q21_centralization$Title[centralization_index]);
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

plot_q21_association_q24_facetedbarchart = function(){
  
  plot_data = prepare_data_q21_association_q24();
  
  plot_barchart_gradients_dodged_series(
    title = "Decentralization versus Centralization",
    subtitle = "This faceted bar chart shows the capability maturity level distributions of survey answers organized by domains and level of centralization",
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

plot_q21_association_q24_facetedbarchart();
#save_plot(
#  plot_object = plot_hypothesis_csp_greater_pammaturity(),
#  file_name = "IAMPerf2020-Q21-Association-Q24-FacetedBarChart.png",
#  width = 11);

test_q21_association_q24_kendall = function(){
  for(domain_index in 1:nrow(iamperf2020_q24_domains)){
    cat("\n", as.character(iamperf2020_q24_domains[domain_index, c("Title")]), ":\n");
    # Retrieve survey data
    domain_data = data.frame(iamperf2020_survey[paste0("Q24R", domain_index)], 
                             iamperf2020_survey[paste0("Q21R", domain_index)]);
    colnames(domain_data) = c("capability_maturity", "centralization");
    # Remove NAs
    domain_data = domain_data[!is.na(domain_data$capability_maturity),];
    domain_data = domain_data[!is.na(domain_data$centralization),];
    print(Kendall::Kendall(domain_data$capability_maturity, domain_data$centralization));
  }
}

test_q21_association_q24_kendall()


# ****************************
# * Association Q22 with Q23 *
# ****************************

# Q22: Reporting Line
# Q23: Goals & Priorities

prepare_data_q22_association_q23 = function(){
  
  plot_data = data.frame();
  
  for(goal_index in 1:nrow(iamperf2020_q23_goals)){
    goal_column = as.character(iamperf2020_q23_goals$X[goal_index]);
    goal_title = as.character(iamperf2020_q23_goals$Title[goal_index]);
    
    # Compute goal statistics, making it possible to add the goal mean "pline"
    goal_data = prepare_data_barchart_with_single_column_coercion(
      iamperf2020_survey[,goal_column], 
      ordering_option = "level",
      label_percent_value_sep = "\n");
    
    for(reporting_line_index in 1:nrow(iamperf2020_q22_reporting_lines)){
      reporting_line_column = as.character(iamperf2020_q22_reporting_lines$X[reporting_line_index]);
      reporting_line_title = as.character(iamperf2020_q22_reporting_lines$Title[reporting_line_index]);
      reporting_line_filter = !is.na(iamperf2020_survey[,reporting_line_column]);
      
      # Remove the CFO from the graph because no IAM manager 
      # has this reporting line in the sample, resulting in empty charts. 
      # Remove also the "Other" option to avoid readability confusion with NAs etc.
      if(reporting_line_title != "CFO" & reporting_line_title != "Other"){
      
        facet_data = prepare_data_barchart_with_single_column_coercion(
          iamperf2020_survey[reporting_line_filter,goal_column], 
          ordering_option = "level",
          label_percent_value_sep = "\n");

        facet_data$facet = reporting_line_title;
        facet_data$facet_2 = goal_title;
        facet_data$series = facet_data$category;
        facet_data$value = facet_data$frequency;
        facet_data$pline_value = goal_data$frequency; # Comparison with goal mean
        
        plot_data = rbind(plot_data, facet_data);
      
      }
    }
  }

  plot_data = plyr::arrange(plot_data, facet, facet_2, category, series);
  
  # Finalize factors
  plot_data$facet = factor(
    plot_data$facet,
    levels = as.character(iamperf2020_q22_reporting_lines$Title),
    ordered = FALSE
  ); 
  plot_data$facet_2 = factor(
    plot_data$facet_2, 
    levels = as.character(iamperf2020_q23_goals$Title),
    ordered = FALSE
  );
  
  return(plot_data);
}

plot_q22_association_q23_facetedbarchart = function(){
  
  plot_data = prepare_data_q22_association_q23();
  
  plot_barchart_gradients_dodged_series(
    title = "Reporting Line versus Goal Priorities",
    subtitle = "This faceted bar chart shows the distribution of priorities by goals and reporting lines.\nReporting lines are non-exclusive categories, for instance a subset of IAM managers report both to the CISO and CIO.\nThe red lines show the mean per goal (including N/A or \"other\" reporting line).",
    axis_x_title = "Reporting Line",
    axis_y_title = "Goals",
    plot_data = plot_data, # Pre-summarized data with multiple series
    # Data structure: series, category, count, label
    legend_title = "Legend",
    x_lim_min = NULL,
    x_lim_max = 1,
    faceted = FALSE,
    grid_faceted = TRUE,
    geom_text_angle = 0,
    geom_text_hjust = .5,
    geom_text_vjust = -.3,
    axis_text_x_blank = TRUE,
    display_pline = TRUE
    #ncol = 3
  );
  
}

plot_q22_association_q23_facetedbarchart();
#save_plot(
#  plot_object = plot_hypothesis_csp_greater_pammaturity(),
#  file_name = "IAMPerf2020-Q22-Association-Q23-FacetedBarChart.png",
#  width = 11);

test_q22_association_q23 = function(){
  
  # Doing a Kendall test
  
  test_data = data.frame();
  
  for(goal_index in 1:nrow(iamperf2020_q23_goals)){
    goal_column = as.character(iamperf2020_q23_goals$X[goal_index]);
    goal_title = as.character(iamperf2020_q23_goals$Title[goal_index]);
    #cat("Goal: ", goal_title, " (", goal_column, ")\n");
    
    # Retrieve goal transversal data
    goal_data = as.numeric(iamperf2020_survey[,goal_column] == "Primary Goal");
    goal_mean = mean(goal_data, na.rm = TRUE);
    
    for(reporting_line_index in 1:nrow(iamperf2020_q22_reporting_lines)){
      reporting_line_column = as.character(iamperf2020_q22_reporting_lines$X[reporting_line_index]);
      reporting_line_title = as.character(iamperf2020_q22_reporting_lines$Title[reporting_line_index]);
      
      # Remove the CFO from the graph because no IAM manager 
      # has this reporting line in the sample, resulting in empty charts. 
      # Remove also the "Other" option to avoid readability confusion with NAs etc.
      if(reporting_line_title != "CFO" & reporting_line_title != "Other"){

        #cat("  Reporting Line: ", reporting_line_title, " (", reporting_line_column, ")\n");
        reporting_line_filter = !is.na(iamperf2020_survey[,reporting_line_column]);
        reporting_line_data = ifelse(reporting_line_filter, goal_data, NA);
        reporting_line_mean = mean(reporting_line_data, na.rm = TRUE);
        
        # Test direction
        alternative_hypothesis = "greater";
        if(reporting_line_mean < goal_mean){
          alternative_hypothesis = "less";  
        }
        
        # Too many warnings in a the loop will stop the script.
        # Since we only take results where p-value is below threshold,
        # we may safely suppress errors and warnings to only collect valid results.
        #try({suppressWarnings({
        
        interesting = FALSE;
        stat_test_dichotomous = NULL;
        #dicho_result =  data.frame(
        dicho_statistic = NA;
        dicho_p_value = NA;
        dicho_estimate = NA;
        dicho_conf_int = NA;
        #);
        #chi_test = NULL;
        #chi_result =  data.frame(
        #  chi_statistic = NA,
        #  chi_doff = NA,
        #  chi_p_value = NA
        #);
        
        #try({
          stat_test_dichotomous = Partiallyoverlapping::Prop.test(
            x1 = reporting_line_data,
            x2 = goal_data,
            alternative = alternative_hypothesis,
            stacked = TRUE,
            conf.level = .95
            );
          #if(stat_test_dichotomous$p.value <= .1) { 
          #  interesting = TRUE;
          #  dicho_result =  data.frame(
              dicho_statistic = stat_test_dichotomous$statistic;
              dicho_p_value = stat_test_dichotomous$p.value;
              dicho_estimate = stat_test_dichotomous$estimate;
              dicho_conf_int = paste(stat_test_dichotomous$conf.int, collapse = " - ");
          #  );
          #  };
        #});
        significant = FALSE;
        if(!is.na(dicho_p_value)){
          if(dicho_p_value <= .05){
            significant = TRUE;
          }
        }
        
        #try({
        #  chi_test = stats::chisq.test(
        #    x = goal_data, #factor(goal_data, levels = c(0,1), ordered = TRUE, exclude = NA),
        #    y = reporting_line_data, #factor(reporting_line_data, levels = c(0,1), ordered = TRUE, exclude = NA),
        #    );
        #  if(chi_test$p.value <= .05) { 
        #    interesting = TRUE;
        #    chi_result =  data.frame(
        #      chi_statistic = chi_test$statistic,
        #      chi_doff = chi_test$parameter,
        #      chi_p_value = chi_test$p.value
        #    );
        #  };
        #});
        
        #if(interesting){

          test_data = bind_rows(
            test_data,
            #bind_cols(
              data.frame(
                goal = goal_title, 
                goal_mean = goal_mean,
                reporting_line = reporting_line_title,
                reporting_line_mean = reporting_line_mean,
                mean_diff = abs(goal_mean - reporting_line_mean),
                alternative_hypothesis = alternative_hypothesis,
                significant = significant,
                dicho_p_value = dicho_p_value,
                dicho_statistic = dicho_statistic,
                dicho_estimate = dicho_estimate,
                dicho_conf_int = paste("'", dicho_conf_int, sep = "")
                ),
              #dicho_result #,
              #chi_result
              #)
            );
        
        #}
          #})}, silent = FALSE);

      }
    }
  }
  
  return(test_data);
  
}

test_data = test_q22_association_q23();
View(test_data);
#writeClipboard(test_data);
write.table(test_data, "clipboard", sep="\t", row.names=FALSE);


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



# ************************************************
# * Association Q23 versus Q24 - STRATEGIC FOCUS *
# ************************************************

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
# * Association Q23 versus Q24R1 - GOAL COVERAGE *
# **********************************************

# IAM Goal Coverage: infered from Survey Question Q23
# CMM: Survey Question Q24R1

prepare_data_q23_association_q24r1_goalcoverage = function(
  na.rm = TRUE
  ,priority = "PrimaryGoal" #SecondaryGoal, #NiceToHave, #NoGoal
){
  
  q23_priorities = prepare_data_q23_priorities(remove_no_goals = FALSE);
  
  # Format it in paired values
  paired_values = data.frame(
    Q24R1 = iamperf2020_survey$Q24R1,
    PriorityCount = q23_priorities[,priority]
    );
  
  # Remove NAs
  if(na.rm){
    paired_values = paired_values[!is.na(paired_values$Q24R1),];
    paired_values = paired_values[!is.na(paired_values$PriorityCount),];
  }
  
  return(paired_values);
}

plot_q23_association_q24r1_goalcoverage_bubblechart = function(
  priority = "PrimaryGoal", #SecondaryGoal, #NiceToHave, #NoGoal
  priority_title = priority_title
){
  
  paired_values = prepare_data_q23_association_q24r1_goalcoverage(
    na.rm = TRUE,
    priority = priority);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = plyr::count(paired_values, vars = c("Q24R1", "PriorityCount"));
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = paste0("IAM General Capability Maturity Level vs ", priority_title, " (Bubble Chart)"),
    subtitle = paste0("Relation between general IAM capability maturity level and the number of ", priority_title, "."),
    x_axis_title = "IAM General Capability Maturity Level",
    y_axis_title = paste0(priority_title, "Number"),
    scale_size_min = 5,
    scale_size_max = 12
  );
  
  return(plot_object);
}

plot_q23_association_q24r1_goalcoverage_bubblechart(priority = "PrimaryGoal", priority_title = "Primary Goals");
plot_q23_association_q24r1_goalcoverage_bubblechart(priority = "SecondaryGoal", priority_title = "Secondary Goals");
plot_q23_association_q24r1_goalcoverage_bubblechart(priority = "NiceToHave", priority_title = "Nice to Haves");
plot_q23_association_q24r1_goalcoverage_bubblechart(priority = "NotAGoal", priority_title = "Non-Goals");

test_q23_association_q24_goalcoverage_kendall = function(){
  
  q23_priorities = prepare_data_q23_priorities(remove_no_goals = FALSE);
  cat("PRIMARY GOALS \n");
  test_kendall_tau(iamperf2020_survey$Q24R1, q23_priorities$PrimaryGoal);
  cat("\nSECONDARY GOALS \n");
  test_kendall_tau(iamperf2020_survey$Q24R1, q23_priorities$SecondaryGoal);
  cat("\nSNICE TO HAVES \n");
  test_kendall_tau(iamperf2020_survey$Q24R1, q23_priorities$NiceToHave);
  cat("\nNON-GOALS \n");
  test_kendall_tau(iamperf2020_survey$Q24R1, q23_priorities$NotAGoal);
}

test_q23_association_q24_goalcoverage_kendall()



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
    y_axis_title = "Indicator Placement",
    scale_size_min = 10,
    scale_size_max = 50
  );
  
  return(plot_object);
}

test_q32_association_q24r1_kendalltau = function(){
  
  paired_values = prepare_data_q32_association_q24r1(na.rm = FALSE);
  colnames(paired_values) = c("CapabilityMaturity", "IndicatorPlacement");
  test_kendall_tau(paired_values$CapabilityMaturity, paired_values$IndicatorPlacement);
}

test_q32_association_q24r1_kendalltau();


# ******************************
# * Association Q35 versus Q23 *
# ******************************

# Number of indicators Q35
# Goals: Survey Question Q23
test_q35_association_q23 = function(){

  cat("Association between number of primary goals and number of indicators\n");
  
  goals_data = prepare_data_q23_priorities(remove_no_goals = FALSE);
  
  # If no goal priorities were provided in the answer, this is equivalent to NA.
  primary_goals = ifelse(goals_data$AllPriorities > 0, goals_data$PrimaryGoal, NA);

  test_kendall_tau(
    primary_goals,
    iamperf2020_survey$Q35);
  
  cat("\nAssociation between number of primary and secondary goals and number of indicators\n");

  # If no goal priorities were provided in the answer, this is equivalent to NA.
  all_goals = ifelse(goals_data$AllPriorities > 0, goals_data$PrimaryGoal + goals_data$SecondaryGoal, NA);
    
  test_kendall_tau(
    all_goals,
    iamperf2020_survey$Q35);
  
  }

plot_q35_association_q23_primary_goals = function(){
  
  goals_data = prepare_data_q23_priorities(remove_no_goals = FALSE);
  
  q23_na_filter = goals_data$AllPriorities >0;
  q35_na_filter = !is.na(iamperf2020_survey$Q35);
  full_filter = q23_na_filter & q35_na_filter;
  
  paired_values = data.frame(
    Goals = goals_data[full_filter, c("PrimaryGoal")],
    IndicatorsNumber = iamperf2020_survey[full_filter, c("Q35")]);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = dplyr::count(paired_values, Goals, IndicatorsNumber);
  
  colnames(group_counts) = c("y", "x", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "Primary Goals Number vs Indicators Number (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between the number of primary goals with the number of indicators",
    x_axis_title = "Indicators Number",
    y_axis_title = "Primary Goals Number",
    scale_size_min = 7,
    scale_size_max = 10
  );
  
  return(plot_object);
  
}

# File: IAMPerf2020_Q35_Association_Q23_PrimaryGoals_BubbleChart.png
plot_q35_association_q23_primary_goals();

plot_q35_association_q23_all_goals = function(){
  
  goals_data = prepare_data_q23_priorities(remove_no_goals = FALSE);
  
  q23_na_filter = goals_data$AllPriorities >0;
  q35_na_filter = !is.na(iamperf2020_survey$Q35);
  full_filter = q23_na_filter & q35_na_filter;
  
  paired_values = data.frame(
    Goals = goals_data[full_filter, c("PrimaryGoal")] + goals_data[full_filter, c("SecondaryGoal")],
    IndicatorsNumber = iamperf2020_survey[full_filter, c("Q35")]);
  
  # Summarizes the data to get counts by pair combinations
  group_counts = dplyr::count(paired_values, Goals, IndicatorsNumber);
  
  colnames(group_counts) = c("y", "x", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "Goals Number vs Indicators Number (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between the number of primary and secondary goals with the number of indicators",
    x_axis_title = "Indicators Number",
    y_axis_title = "Goals Number",
    scale_size_min = 7,
    scale_size_max = 10
  );
  
  return(plot_object);
  
}

# File: IAMPerf2020_Q35_Association_Q23_AllGoals_BubbleChart.png
plot_q35_association_q23_all_goals();

# ******************************
# * Association Q35 versus Q24 *
# ******************************

# Number of indicators Q35
# CMM: Survey Question Q24

prepare_data_q35_association_q24 = function(){
  
  plot_data = data.frame(
    capability_maturity = iamperf2020_survey$Q24R1,
    indicators_number = iamperf2020_survey$Q35
    );

  # Remove NAs
  plot_data = plot_data[
    !is.na(plot_data$capability_maturity)
    ,];
  plot_data = plot_data[
    !is.na(plot_data$indicators_number)
    ,];
  
  plot_data$indicator_best_practice = ifelse(
    plot_data$indicators_number < 3, "Less", ifelse(
      plot_data$indicators_number > 15, "More", "Best Practice"
      )
    );
  
  plot_data = plot_data[,c("capability_maturity", "indicator_best_practice")];

  plot_data = plyr::count(plot_data);

  # We want ordered factors
  # factor order will be re-used in GGPlot2
  plot_data$capability_maturity = factor(
    plot_data$capability_maturity,
    as.character(iamperf2020_q24_maturity_levels$Title),
    ordered = TRUE
  );
  plot_data$indicator_best_practice = factor(
    plot_data$indicator_best_practice,
    c("Less", "Best Practice", "More"),
    ordered = TRUE
  );
  
  colnames(plot_data) = c("category", "group", "count");
  
  # Reshaping
  plot_data$label = NA;
  #plot_data$count = plot_data$value;

  return(plot_data);
}

plot_q35_association_q24_facetedbarchart = function(){
  
  plot_data = prepare_data_q35_association_q24();
  
  plot_object = plot_stack_count(
    title = "Indicators Number versus Capability Maturity",
    subtitle = "This bar chart shows the distribution of indicators number per capability maturity level",
    axis_x_title = "Count",
    axis_y_title = "Indicators Number",
    plot_data = plot_data # Pre-summarized data with multiple series
    # Data structure: series, category, count, label
    #legend_title = "Legend",
    #x_lim_min = NULL,
    #x_lim_max = .5,
    #faceted = FALSE,
    #grid_faceted = FALSE,
    #geom_text_angle = 0,
    #geom_text_hjust = .5,
    #geom_text_vjust = -.3,
    #axis_text_x_blank = TRUE
    #ncol = 3
  );
  
  plot_object + ggplot2::coord_flip();
  
}

# File: IAMPerf2020_Q35_Association_Q24R1_BarChart.png
plot_q35_association_q24_facetedbarchart();


# ******************************
# * Association Q35 versus Q30 *
# ******************************

# Number of indicators Q35
# Indicators Automation Q30

plot_q30_association_q35_bubblechart = function(){
  
  paired_values = data.frame(
    Q35 = iamperf2020_survey$Q35, 
    Q30 = iamperf2020_survey$Q30);
  
  # Remove NAs
  paired_values = paired_values[
    !is.na(paired_values$Q30) & !is.na(paired_values$Q35)
    ,];
  
  # Summarizes the data to get counts by pair combinations
  group_counts = dplyr::count(paired_values, paired_values$Q35, paired_values$Q30);
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "Indicators Number vs Degree of Automation (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between the number of indicators and their degree of automation",
    x_axis_title = "Indicators number",
    y_axis_title = "Degree of automation",
    scale_size_min = 5,
    scale_size_max = 15
  );
  
  return(plot_object);
}

# File: IAMPerf2020_Q30_Association_Q35_BubbleChart.png

test_q30_association_q35_kendalltau = function(){
  
  test_kendall_tau(
    iamperf2020_survey$Q35, 
    iamperf2020_survey$Q30);
}

test_q30_association_q35_kendalltau();


# ******************************
# * Association Q35 versus Q31 *
# ******************************

# Number of indicators Q35
# Indicators Coverage Q31

plot_q31_association_q35_bubblechart = function(){
  
  paired_values = data.frame();
  
  for(column_index in 1:nrow(iamperf2020_q31_dimensions)){
    column_name = as.character(iamperf2020_q31_dimensions$X[column_index]);
    paired_values = rbind(
      paired_values,
      data.frame(
      Q35 = iamperf2020_survey[,"Q35"], 
      Q31 = iamperf2020_survey[,column_name])
      );
  }
  
  # Remove NAs
  paired_values = paired_values[
    !is.na(paired_values$Q31) & !is.na(paired_values$Q35)
    ,];
  
  # Summarizes the data to get counts by pair combinations
  group_counts = dplyr::count(paired_values, paired_values$Q35, paired_values$Q31);
  
  colnames(group_counts) = c("x", "y", "z");
  
  plot_object = plot_bubblechart(
    plot_data = group_counts, 
    title = "Indicators Coverage vs Indicators Number (Bubble Chart)",
    subtitle = "This bubble chart shows the relation between the coverage of indicators over a small subset of dimensions and the number of indicators actively used by organizations",
    x_axis_title = "Indicators Number",
    y_axis_title = "Indicators Coverage",
    scale_size_min = 5,
    scale_size_max = 15
  );
  
  return(plot_object);
}

plot_q31_association_q35_bubblechart();
# File: IAMPerf2020_Q31_Association_Q35_BubbleChart.png

test_q31_association_q35_kendalltau = function(){

  paired_values = data.frame();
  
  for(column_index in 1:nrow(iamperf2020_q31_dimensions)){
    column_name = as.character(iamperf2020_q31_dimensions$X[column_index]);
    paired_values = rbind(
      paired_values,
      data.frame(
        Q35 = iamperf2020_survey[,"Q35"], 
        Q31 = iamperf2020_survey[,column_name])
    );
  }
  
  test_kendall_tau(
    paired_values$Q35, 
    paired_values$Q31);
}

test_q31_association_q35_kendalltau();


