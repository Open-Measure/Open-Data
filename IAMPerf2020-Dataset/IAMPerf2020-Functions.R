##########################################################
#                                                        #
# Utility functions focused on facilitating the analysis #
# of the IAM 2020 Performance Measurement Survey         # 
#                                                        #
##########################################################

# Environment

# Set console to English
Sys.setenv(LANG = "en");

# Packages
if(!require("ggplot2")) install.packages("ggplot2");
if(!require("viridis")) install.packages("viridis");
if(!require("ggpubr")) install.packages("ggpubr");
if(!require("forcats")) install.packages("forcats");
if(!require("grid")) install.packages("grid");
if(!require("gridtext")) install.packages("gridtext");
if(!require("ggtext")) install.packages("ggtext");
if(!require("ggrepel")) install.packages("ggrepel");
if(!require("htmltools")) install.packages("htmltools");
if(!require("scales")) install.packages("scales");
if(!require("plyr")) install.packages("plyr");
if(!require("likert")) install.packages("likert");
#if(!require("HH")) install.packages("HH");
if(!require("RColorBrewer")) install.packages("RColorBrewer");

# Functions Declarations

# From the current version of I was getting the following error:
# Error in Partiallyoverlapping::Partover.test(x1 = goal_data, x2 = reporting_line_data,  : 
#                                                object 'p.value' not found
Partiallyoverlapping_Partover_test_bugfixed = function(x1=NULL,x2=NULL,x3=NULL,x4=NULL, alternative="two.sided", conf.level=NULL, stacked=FALSE){
    if (stacked==TRUE){#Separates observations into format required for stacked = FALSE.
      if (length(x1)!=length(x2)) stop ("samples must be specified of equal lengths within the matrix for stacked=FALSE. Check structure of data") #The length of a and b must be equal. 
      if ((!is.null(x3))|(!is.null(x4))) stop ("stacked = TRUE option requires only 2 specified vectors, one for each sample. Check structure of data")
      a2<-rep(x1,2)
      b2<-rep(x2,2)
      grp<-rep(NA,(length(x1)*2))
      stacked<-data.frame(a2,b2,grp) 
      for (i in 1:(length (stacked$a2)/2)){if (!is.na(stacked$a2[i])) if (is.na(stacked$b2[i])) stacked$grp[i]<-"aunp"}
      for (i in 1:(length (stacked$a2)/2)){if (!is.na(stacked$a2[i])) if (!is.na(stacked$b2[i])) stacked$grp[i]<-"apaired"}
      for (i in 1:(length (stacked$a2)/2)){if (is.na(stacked$a2[i])) stacked$grp[i]<-"exclude"}  #  The "exclude" coding allows for observations with na on both samples to pass through the system without issue.
      for (i in ((length(stacked$a2)/2)+1): length(stacked$a2)) if (is.na(stacked$a2[i])) if (!is.na(stacked$b2[i])) stacked$grp[i]<-"bunp"
      for (i in ((length(stacked$a2)/2)+1): length(stacked$a2)) if (!is.na(stacked$a[i])) if (!is.na(stacked$b[i])) stacked$grp[i]<-"bpaired"
      for (i in ((length(stacked$a2)/2)+1): length(stacked$a2)) if (is.na(stacked$b[i])) stacked$grp[i]<-"exclude"
      x1<-stacked[stacked$grp=="aunp",1]
      x2<-stacked[stacked$grp=="bunp",2]
      x3<-stacked[stacked$grp=="apaired",1]
      x4<-stacked[stacked$grp=="bpaired",2]}
    
    pairedobs<-paste(x3,x4)
    pairedobs_a<-pairedobs[pairedobs=="1 1"]
    a<-length(pairedobs_a)
    pairedobs_b<-pairedobs[pairedobs=="1 0"]
    b<-length(pairedobs_b)
    pairedobs_c<-pairedobs[pairedobs=="0 1"]
    c<-length(pairedobs_c)
    pairedobs_d<-pairedobs[pairedobs=="0 0"]
    d<-length(pairedobs_d)
    ind_e<-x1[x1==1]
    e<-length(ind_e)
    ind_f<-x1[x1==0]
    f<-length(ind_f)
    ind_g<-x2[x2==1]
    g<-length(ind_g)
    ind_h<-x2[x2==0]
    h<-length(ind_h)
    
    #Checks that vectors specified are of required length for partially overlapping samples t-test# 
    if (is.null(x1)&&is.null(x2)) {stop ("no independent observations specified")}
    if (length (x3)!=length(x4)) {stop ("paired observations not of same length")}
    if (length (x3)<2) {stop ("not enough paired observations")}
    
    #Elements of the test 
    
    u<-(length(x3)+length(x1))
    v<-(length(x4)+length(x2))    
    p1bar<-(a+b+e)/u
    p2bar<-(a+c+g)/v
    pbar<-((u*p1bar)+(v*p2bar))/((2*length(x3))+length(x1)+length(x2))
    if ((stats::sd(x3)==0)|(stats::sd(x4)==0)) {r<-0} else {r<-stats::cor (x3,x4)}  
    
    #test
    estimate<-p1bar- p2bar
    w<-pbar*(1-pbar)
    noise<- sqrt(((w/u)+(w/v))-(2*r*(w*length(x3)/(u*v))))
    statistic <- estimate / noise
    
    
    #calculates p-value, with p-value fixed in extremes of no variability so that output does not give NaN or NA.
    if (is.na(statistic)) {p.value<-1}
    if (!is.na(statistic)) {if (xor((statistic == -Inf), (statistic == Inf))) p.value<-0 else{
      if(alternative=="less"){
        p.value<-stats::pnorm(statistic)
      }
      if(alternative=="greater"){
        p.value<-stats::pnorm(-abs(statistic))
      }
      if(alternative=="two.sided"){
        p.value<-2*stats::pnorm(-abs(statistic))
      }
    }}
    
    #gives relevant outputs dependent on whether confidence intervals are required
    if(is.null(conf.level)){
      theoutputs<-list(statistic=statistic,p.value=p.value,estimate=estimate)
    }
    if(!is.null(conf.level)){
      if(alternative=="two.sided"){
        con<-(1-((1-conf.level)/2))
        critical_val<-stats::qnorm(con)
        lower_int<-estimate-(critical_val*noise)
        upper_int<-estimate+(critical_val*noise)
      }
      if(alternative=="less"){
        con<-conf.level +((1-conf.level)/2)
        critical_val<-stats::qnorm(conf.level)
        lower_int<-(-Inf)
        upper_int<-estimate+(critical_val*noise)
      }
      if(alternative=="greater"){
        con<-conf.level +((1-conf.level)/2)
        critical_val<-stats::qnorm(conf.level)
        lower_int<-estimate-(critical_val*noise)
        upper_int<-Inf
      }
      theoutputs<-list(statistic=statistic,p.value=p.value,estimate=estimate,conf.int=c(lower_int,upper_int))
    }
    return(theoutputs) 
  }




save_plot = function(
  plot_object, 
  file_name,
  width = 11,
  height = NA){
  
  setwd("C:\\Users\\david\\Google Drive (openmeasure@gmail.com)\\Private\\GitHub\\Open-Data\\IAMPerf2020-Dataset");
  
  if(is(plot_object, "upset")){
    print("Special handling for upset charts")
    stop("I couldn't find an easy solution to save upset graphs, please do it manually");
    png(
      #antialias = "cleartype",
      file = file_name,
      res = 300,
      width = width,
      height = height,
      units = "in"
      #type = "windows");
    );
    plot_object;
    dev.off();
  } else {
    ggplot2::ggsave(
      filename = file_name,
      plot = plot_object,
      dpi = 300,
      limitsize = FALSE,
      width = width,
      height = height
    );
  }
}

# Ideal to compare 3 or more distributions.
prepare_data_barchart_with_single_column_coercion = function(
  plot_data, # The data series
  ordering_option = "count", # category | count | level
  ordering_direction = 1, # 1 | -1
  label_percent_value_sep = " " # The separator between the percentage and absolute value
)
{
  data_series_levels = NULL;
  factor_is_ordered = NULL;
  if(is.factor(plot_data)){
    data_series_levels = levels(plot_data);
    factor_is_ordered = is.ordered(plot_data);
  } else if(is.data.frame(plot_data)) {
    for(column_index in 1: ncol(plot_data)){
      data_series_levels = unique(c(data_series_levels, levels(plot_data[,column_index])));
      # Minor "bug", this will yield the last ordering attribute from data frame columns
      factor_is_ordered = is.ordered(plot_data[,column_index]);
    }
  }
  decreasing = ifelse(ordering_direction == 1, FALSE, TRUE);
  # Coerce to a single vector.
  plot_data = as.vector(unlist(plot_data)); 
  # Remove NAs if applicable.
  plot_data = plot_data[!is.na(plot_data)];
  # Reapply original factors. Like this, if one factor has a count of 0, it will be maintained.
  plot_data = factor(plot_data, levels = data_series_levels, ordered = factor_is_ordered);
  data_table = base::table(plot_data);
  data_count_label = paste("(", data_table, ")", sep = "");
  data_category = names(data_table);
  data_frequency = prop.table(data_table);
  data_frequency_label = paste(rounded_ratios_with_largest_remainder(data_frequency, digits = 1), "%");
  data_label = paste(data_frequency_label, data_count_label, sep = label_percent_value_sep);
  data_count = as.vector(data_table);
  data_frame = data.frame(
    category = data_category,
    count = data_count,
    frequency = as.numeric(data_frequency),
    label = data_label
  );
  if(ordering_option == "category") { 
    data_frame = data_frame[order(data_frame$category, decreasing = decreasing),]; 
    rownames(data_frame) = 1:nrow(data_frame);
  }
  if(ordering_option == "count") { 
    data_frame = data_frame[order(data_frame$count, decreasing = decreasing),]; 
    rownames(data_frame) = 1:nrow(data_frame);
  }
  if(ordering_option == "level") { 
    data_frame$category = factor(
      data_frame$category,
      levels = data_series_levels,
      ordered = TRUE
    );
    data_frame = dplyr::arrange(data_frame, category);    
    rownames(data_frame) = 1:nrow(data_frame);
  }
  return(data_frame);
}

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

test_kendall_tau = function(
  sample_1_factor, 
  sample_2_factor){

  # It is expected that factors are properly ordered.
  sample_1_numeric = as.numeric(sample_1_factor);
  sample_2_numeric = as.numeric(sample_2_factor);
  
  cat(paste("Total observations: ", 
        length(sample_1_numeric), 
        "\n",
        sep=""));
  cat(paste("Valid observations (pairs of non-N/As): ", 
        sum(
          !is.na(sample_1_numeric) & 
            !is.na(sample_2_numeric)
        ),
        "\n",
        sep=""));
  
  print(cor(
    cbind(sample_1_numeric, sample_2_numeric), 
    method="kendall", 
    use="pairwise"));
  
  test_results = cor.test(
    sample_1_numeric, 
    sample_2_numeric,  
    method="kendall", 
    exact = FALSE);
  print(test_results);
  #Kendall::Kendall(
  #  plot_data$MaturityNumeric,
  #  plot_data$CoverageNumeric
  #  );
  
  #friendly_test_results = paste(
  #  "Test type: Kendall's 𝜏 coefficient (two-sided test)",
  #  "H₀: 𝜏 = 0",
  #  "Hₐ: 𝜏 ≠ 0",
  #  paste("z-score: ", format(test_results$statistic["z"], digits = 6), sep=""),
  #  paste("p-value: ", format(test_results$p.value, digits = 6, scientific = FALSE), sep=""),
  #  paste("𝜏 estimate: ", format(test_results$estimate["tau"], digits = 6), " - ", format(test_results$conf.int[2], digits = 4), sep=""),
  cat(
    paste(
      "Conclusion: ", 
      ifelse(test_results$p.value < .05, "Reject", "Fail to reject"),
      " H₀",
      "\n\n",
      sep = ""
      )
  );
  
  #return(friendly_test_results);

}

plot_likertchart = function(
  plot_data,
  levels_number,
  title,
  subtitle,
  legend,
  axis_x_title,
  axis_y_title
){
  
  likert_data = likert::likert(plot_data, nlevels = levels_number);
  
  plot_object = plot(
    likert_data,
    legend = legend,
    legend.position = "bottom",
    #plot.percents = TRUE,
    type = "bar",
    colors = viridis::viridis_pal(direction = -1)(levels_number)
  ) + 
    #ggplot2::ggtitle(
    #  title,
    #  subtitle = subtitle);
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = axis_x_title,
      y = axis_y_title
    );
  
  return(plot_object);
  
}

plot_statistical_test = function(test_results){
  plot_object = 
    ggplot2::ggplot(data.frame(x = c(0,1), y = c(0,1))) + 
    ggplot2::aes(x, y) +
    ggplot2::geom_line(linetype = "blank") +
    ggplot2::annotate(
      "text", 
      x = 0, #xmin = 0, xmax = 1,
      y = .5, #ymin = 0, ymax = 1,
      size = 5, 
      label = test_results,
      family = "mono",
      hjust = 0
      #vjust = -1
    ) +
    #ggplot2::geom_text(
    #  data = data.frame(text = test_results, x = 0, y = 0),
    #  size = 5, 
    #  label = test_results,
    #  family = "mono",
    #  hjust = 0
    #) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = margin(.1,.1,1,.1, "cm"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing = margin(.1,.1,1,.1, "cm"),
      plot.background=element_blank());
  return(plot_object);
}

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

plot_barchart_gradients = function(
  title = NULL,
  subtitle = NULL,
  axis_x_title = NULL,
  axis_y_title = NULL,
  plot_data, # Pre-summarized data
  legend_title = "Legend",
  x_lim_min = NULL,
  x_lim_max = NULL,
  ordering_option = "count",
  ordering_direction = 1,
  fill_aes = "count",
  scale_fill_discrete = FALSE)
  {
  # Returns a GGPlot2 barchart 
  # with gradient colors
  # and bars ordered by factor levels.

  x_lim_min = ifelse(
    is.null(x_lim_min), 
    0,
    x_lim_min);
    
  x_lim_max = ifelse(
    is.null(x_lim_max), 
    round(max(plot_data$count) * 1.1,0),
    x_lim_max);
  
  plot_aes = NULL;
  if(fill_aes == "category"){
    if(ordering_option == "level"){
      plot_aes = ggplot2::aes(
        y = category,
        x = count,
        fill = category);
    } else {
      plot_aes = ggplot2::aes(
        y = reorder(category, count),
        x = count,
        fill = category);
    }
  } else {
    if(ordering_option == "level"){
      plot_aes = ggplot2::aes(
        y = category,
        x = count,
        fill = count);
    } else {
      plot_aes = ggplot2::aes(
        y = reorder(category, count),
        x = count,
        fill = count);
    }
  }
  
  if(fill_aes == "category"){
    scale_fill_discrete = TRUE;
  }
  
  plot_object = 
    ggplot2::ggplot(
    data = plot_data, 
    plot_aes) + 
    ggplot2::geom_bar(
      stat = "identity",
      #position = ggplot2::position_dodge(width = .75),
      colour = "black"
    ) + 
    viridis::scale_fill_viridis(discrete = scale_fill_discrete, direction = -1) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), 
      hjust = -0.5, 
      size = 3,
      position = ggplot2::position_dodge(width = 1),
      inherit.aes = TRUE
    ) + 
    ggplot2::xlim(x_lim_min, x_lim_max) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(c(.1,.1,.1,.1), "cm"),
      plot.margin = ggplot2::unit(c(.1,.1,.1,.1), "cm")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = legend_title,
      x = axis_x_title,
      y = axis_y_title
    ) + 
    ggplot2::theme(legend.position = "none");
  
  return(plot_object);
}

if(!require("ungeviz")) devtools::install_github("wilkelab/ungeviz");
plot_barchart_gradients_dodged_series = function(
  title = NULL,
  subtitle = NULL,
  axis_x_title = NULL,
  axis_y_title = NULL,
  plot_data, # Pre-summarized data with multiple series
  # Data structure: series, category, count, label
  legend_title = "Legend",
  x_lim_min = NULL,
  x_lim_max = NULL,
  faceted = FALSE,
  grid_faceted = FALSE,
  label_wrap_width = 14,
  geom_text_angle = 90,
  geom_text_hjust = -.5,
  geom_text_vjust = 0,
  #,
  axis_text_x_blank = FALSE,
  nrow = NULL,
  display_pline = FALSE # Add a red pline from column pline_value for comparison purposes, typically with a mean
  #ncol = NULL
){
  # Returns a GGPlot2 barchart 
  # with gradient colors
  # and bars ordered by factor levels.
  
  x_lim_min = ifelse(
    is.null(x_lim_min), 
    0,
    x_lim_min);
  
  x_lim_max = ifelse(
    is.null(x_lim_max), 
    ceiling(max(plot_data$value) * 1.1),
    x_lim_max);
  
  plot_object = 
    ggplot2::ggplot(
      data = plot_data, 
      ggplot2::aes(
        y = category,
        x = value,
        fill = series)
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(width = 1),
      colour = "black"
    ) + 
    viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
    ggplot2::geom_text(
      angle = geom_text_angle,
      ggplot2::aes(label = label), 
      hjust = geom_text_hjust, 
      size = 3,
      vjust = geom_text_vjust,
      position = ggplot2::position_dodge(width = 1),
      inherit.aes = TRUE
    );
  
  if(display_pline){
    # Reference: https://wilkelab.org/ungeviz/reference/geom_hpline.html
    plot_object = plot_object +
      ungeviz::geom_vpline(
        mapping = ggplot2::aes(
          x = pline_value, 
          y = category,
          group = 1),
        size = 1,
        color = "red"
        );
    };
  
  plot_object = plot_object +
    ggplot2::xlim(x_lim_min, x_lim_max) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = legend_title,
      x = axis_x_title,
      y = axis_y_title
    );
  
  if(axis_text_x_blank){
    plot_object = plot_object + ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_blank())
  } else {
    plot_object = plot_object + ggplot2::theme(
      legend.position = "bottom")
  }
  
  if(faceted){
    # Reference for labeller: https://ggplot2.tidyverse.org/reference/labellers.html
    plot_object = plot_object + ggplot2::facet_wrap(
      ~facet,
      labeller = ggplot2::label_wrap_gen(width = label_wrap_width));
  }

  if(grid_faceted){
    # Reference for labeller: https://ggplot2.tidyverse.org/reference/labellers.html
    plot_object = plot_object + ggplot2::facet_grid(
      facet ~ facet_2, 
      labeller = ggplot2::label_wrap_gen(width = label_wrap_width));
  }
  
  return(plot_object);
  
}

plot_stack_count = function(
  title = NULL,
  subtitle = NULL,
  axis_x_title = NULL,
  axis_y_title = NULL,
  remove_na = TRUE,
  plot_data = NULL, # data.frame with columns "group", "category", "count", "label"
  legend_title = NULL
  )
  {
  
  plot_object = 
    ggplot2::ggplot(
      data = plot_data, 
      mapping = ggplot2::aes(
        x = count,
        y = group)) + 
    ggplot2::geom_bar(
      stat = "identity", 
      position = ggplot2::position_stack(),
      mapping = ggplot2::aes(fill = rev(category))) + 
    ggplot2::geom_label(
      position = ggplot2::position_stack(vjust = .5),
      show.legend = FALSE,
      fill = "white",
      ggplot2::aes(y = group, label = label)) +
    viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = legend_title,
      x = axis_x_title,
      y = axis_y_title) +
    ggplot2::theme(
      legend.position = "bottom");
  
  return(plot_object);
}

if(!require("processx")) install.packages("processx");
if(!require("devtools")) install.packages("devtools");
devtools::install_github("hms-dbmi/UpSetR", force = TRUE);
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
  #plot_object = 
  UpSetR::upset(
    data = data_frame, 
    sets = friendly_categories,
    #expression = "ColName > 1",
    #expression = "RowName > 1",
    sets.bar.color = 
      viridis::viridis(n = length(friendly_categories), direction = -1),
      #grDevices::colorRampPalette(
      #brewer.pal(8, "YlGnBu"))(length(friendly_categories)),
    matrix.color = "black",
    order.by = "freq",
    #nintersects = NULL, #30,
    mb.ratio = c(0.3, 0.7),
    keep.order = TRUE,
    set_size.show	= TRUE,
    );
  
  # Reference: https://github.com/hms-dbmi/UpSetR/issues/76
  # grid::grid.text(title,x = 0.1, y=0.95, gp=gpar(fontsize=16))
  
  # Was required in CRAN version of UpSetR:  
  #plot_object = ggplot2::last_plot();
  # Reference: https://github.com/hms-dbmi/UpSetR/pull/100
  
  #return(plot_object);
}

plot_boxandwhiskers = function(
  plot_data, # a data.frame with a plot_data column
  title,
  subtitle,
  x_axis
){
  
  plot_object = ggplot2::ggplot(
    plot_data, 
    ggplot2::aes(number, "")
    ) +
    ggplot2::geom_boxplot(
      outlier.shape = NA,
    ) + 
    ggplot2::geom_jitter(
      width = 0,
      color = "#440154",
      size = 2.5
      ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_axis,
      y = NULL);
  
  return(plot_object);
}

plot_bubblechart = function(
  plot_data, # Structure: data.frame(x, y, z)
  title,
  subtitle,
  x_axis_title,
  y_axis_title,
  scale_size_min = 1,
  scale_size_max = 50
){
  
  plot_object = 
    ggplot2::ggplot(
      data = plot_data,
      mapping = ggplot2::aes(x = x, y = y)
      ) +
    ggplot2::geom_point(ggplot2::aes(color = z, size = z)) + 
    ggplot2::geom_text(ggplot2::aes(label = z)) +
    viridis::scale_color_viridis(direction = -1) +
    ggplot2::scale_size_continuous(range = c(scale_size_min, scale_size_max), name="Frequency") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::xlab(x_axis_title) +
    ggplot2::ylab(y_axis_title);

  return(plot_object);
}

if(!require("stats")) install.packages("stats");
if(!require("Partiallyoverlapping")) install.packages("Partiallyoverlapping");
test_hypothesis_ordinal_greater = function(
  original_hypothesis = NULL,
  h0 = NULL,
  ha = NULL,
  sample_1,
  sample_2,
  alternative = "greater",
  print_output = TRUE){
  
  #cat("PARTIALLY OVERLAPPING SAMPLES T-TEST as per Derrick et al. (2017)\n")
  # Bibliography:
  # Derrick, B., White, P., 2018. Methods for comparing the responses from a Likert question, with paired observations and independent observations in each of two samples 10.
  #cat("Execution of the statistical test using the 'Partover.test' function of the 'Partiallyoverlapping' R package.\n");
  
  #if(!is.null(original_hypothesis)){
  #  cat("Original hypothesis:", original_hypothesis, "\n");
  #}
  # This transformation is acceptable because the Wilcoxon test accepts ordinal variables.
  #cat("H0: ", h0, "\n");
  #cat("Ha: ", ha, "\n"); 
  
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
  
  if(print_output){
    # Print a user-friendly version of the test outcome:
    cat("Alternative:", alternative, "\n");
    cat("T-statistic:", derrick_test_outcome$statistic, "\n");
    cat("Degrees of freedom:", derrick_test_outcome$parameter, "\n");
    cat("p-value:", derrick_test_outcome$p.value, "\n");
    cat("Estimated difference:", derrick_test_outcome$estimate, "\n");
    cat("CI:", derrick_test_outcome$conf.int, "\n");
    if(derrick_test_outcome$p.value < .05)
    { 
      cat("The test is statistically significant.\n"); 
    } else {
      cat("The test is not statistically significant.\n");
    };
  };
 
  return(derrick_test_outcome); 
  
}

