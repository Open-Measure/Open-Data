# Developing an R package just for the IAM Performance Measurement 2020 Survey 
# would have been an overkill. To keep things simpler, I chose to store reusable
# functions in this R script and load it from GitHub with the devtools package.

# Configuration options
iamperf2020_data_url = "https://raw.githubusercontent.com/Open-Measure/Open-Data/master/IAMPerf2020-Dataset/";
iamperf2020_survey_url = paste0(iamperf2020_data_url, "IAMPerf2020.csv");
iamperf2020_q23_goals_url = paste0(iamperf2020_data_url, "IAMPerf2020Q23Goals.csv");
iamperf2020_q23_priorities_url = paste0(iamperf2020_data_url, "IAMPerf2020Q23Priorities.csv");

# Load the data
iamperf2020_data <- read.csv (text = RCurl::getURL(iamperf2020_survey_url));
iamperf2020_q23_goals <- read.csv (text = RCurl::getURL(iamperf2020_q23_goals_url));
iamperf2020_q23_priorities <- read.csv (text = RCurl::getURL(iamperf2020_q23_priorities_url))

# Q23: Apply nicely labeled and properly ordered factors.
iamperf2020_data$Q23R1 = factor(iamperf2020_data$Q23R1, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R2 = factor(iamperf2020_data$Q23R2, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R3 = factor(iamperf2020_data$Q23R3, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R4 = factor(iamperf2020_data$Q23R4, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R5 = factor(iamperf2020_data$Q23R5, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R6 = factor(iamperf2020_data$Q23R6, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R7 = factor(iamperf2020_data$Q23R7, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R8 = factor(iamperf2020_data$Q23R8, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R9 = factor(iamperf2020_data$Q23R9, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R10 = factor(iamperf2020_data$Q23R10, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);
iamperf2020_data$Q23R11 = factor(iamperf2020_data$Q23R11, levels = iamperf2020_q23_priorities$X, labels = iamperf2020_q23_priorities$Title, ordered = TRUE, exclude = NA);

