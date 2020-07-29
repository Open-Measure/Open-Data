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
if(!require("ggpubr")) install.packages("ggpubr");

# Functions Declarations

save_plot = function(
  plot_object, 
  file_name,
  width = NULL,
  height = NULL){
  
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
      width = width
    );
  }
}

if(!require("forcats")) install.packages("forcats");
prepare_data_barchart_with_single_column_coercion = function(
  plot_data, # The data series
  ordering_option = "count", # category | count | level
  ordering_direction = 1 # 1 | -1
)
{
  data_series_levels = levels(plot_data);
  decreasing = ifelse(ordering_direction == 1, FALSE, TRUE);
  plot_data = as.vector(unlist(plot_data)); # Coerce to a single vector.
  plot_data = plot_data[!is.na(plot_data)]; # Remove NAs if applicable.
  data_table = table(plot_data);
  data_count_label = paste("(", data_table, ")", sep = "");
  data_category = names(data_table);
  data_frequency = prop.table(data_table);
  data_frequency_label = paste(rounded_ratios_with_largest_remainder(data_frequency, digits = 1), "%");
  data_label = paste(data_frequency_label, data_count_label, sep = " ");
  data_count = as.vector(data_table);
  data_frame = data.frame(
    category = data_category,
    count = data_count,
    label = data_label
  );
  if(ordering_option == "category") { 
    data_frame = data_frame[order(data_frame$category, decreasing = decreasing),]; 
  }
  if(ordering_option == "count") { 
    data_frame = data_frame[order(data_frame$count, decreasing = decreasing),]; 
  }
  if(ordering_option == "level") { 
    data_frame$category = factor(
      data_frame$category,
      levels = data_series_levels,
      ordered = TRUE
    );
    data_frame = dplyr::arrange(data_frame, category);    
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

if(!require("grid")) install.packages("grid");
if(!require("gridtext")) install.packages("gridtext");
if(!require("ggtext")) install.packages("ggtext");
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
  x_lim_max = NULL
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
    round(max(plot_data$count) * 1.1,0),
    x_lim_max);
  
  plot_object = ggplot2::ggplot(
    data = plot_data, 
    ggplot2::aes(
      y = category,
      x = count,
      fill = category)
    ) +
    scale_fill_brewer(palette = "YlGnBu") +
    ggplot2::geom_bar(
      stat = "identity",
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
