rm(list = ls(all.names = TRUE))

# Summarize basic demographic information about postdocs.

library(cowplot)
library(ggplot2)
library(canadianmaps)

source('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/CAPS_survey_analysis/0 - utility functions.R')

clean_data <- read.table('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified_prepped.txt',
                         header = TRUE, sep = '\t', stringsAsFactors = FALSE)


# Province
NA_provinces <- rep(NA, length(PROV$PRENAME))
names(NA_provinces) <- PROV$PRENAME

province_counts <- table(clean_data$province)

province_counts <- data.frame(province = names(province_counts),
                              counts = as.integer(province_counts))
rownames(province_counts) <- province_counts$province

province_counts_filled <- NA_provinces
province_counts_filled[rownames(province_counts)] <- province_counts$counts

count_per_province <- PROV
count_per_province$counts <- province_counts_filled

count_per_province$counts[which(is.na(count_per_province$counts))] <- 0

overall_province_plot <- ggplot() +
                            geom_prov(count_per_province, fill = "counts") +
                            crs_coord() +
                            label_prov(count_per_province, label="counts") +
                            scale_fill_continuous(low = "#e1e2e6", high = "#7d89b9") +
                            guides(fill = "none") +
                            theme_map() +
                            ggtitle('Number of respondents per province/territory') +
                            theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/province_barplot.pdf',
       plot = overall_province_plot,
       device = 'pdf',
       width = 6,
       height = 4,
       dpi = 400)


# Sex
overall_sex <- return_count_and_percent_breakdown(in_df = clean_data,
                                                  column = 'sex')

overall_sex_plot <- ggplot(data = overall_sex, aes(x = category, y = percent)) +
                            geom_col(position = 'stack', fill = '#111b42') +
                            geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.25) +
                            theme_bw() +
                            xlab('Sex') +
                            ylab(' Percentage of\nall respondents') +
                            theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                            ylim(0, 50)


# Disabled status
overall_disabled <- return_count_and_percent_breakdown(in_df = clean_data,
                                                       column = 'disabled_person')

overall_disabled_plot <- ggplot(data = overall_disabled, aes(x = category, y = percent)) +
  geom_col(position = 'stack', fill = '#111b42') +
  geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.52) +
  theme_bw() +
  xlab('Disabled person') +
  ylab(' Percentage of\nall respondents') +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5))


# Race/ethnicity

overall_race <- return_count_and_percent_breakdown(in_df = clean_data,
                                                   column = 'race_ethnicity')
overall_race$category[which(overall_race$category == 'European/Caucasian')] <- 'European/\ncaucasian'
overall_race$category[which(overall_race$category == 'Middle Eastern')] <- 'Middle\neastern'
overall_race$category <- factor(overall_race$category,
                                levels = c('European/\ncaucasian',
                                           'Asian',
                                           'Middle\neastern',
                                           'Hispanic',
                                           'African',
                                           'Other'))

overall_race_plot <- ggplot(data = overall_race, aes(x = category, y = percent)) +
                            geom_col(position = 'stack', fill = '#111b42') +
                            geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.25) +
                            theme_bw() +
                            xlab('Race/ethnicity') +
                            ylab(' Percentage of\nall respondents') +
                            theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                            ylim(0, 50)

# Key demographic plot with sex and race breakdown.
# sex_and_race_barplot <- cowplot::plot_grid(overall_sex_plot,
#                                            overall_race_plot,
#                                            ncol = 2,
#                                            rel_widths = c(1.2, 2),
#                                            labels = c('a', 'b'))

# Decided to just go with race/ethnicity plot, as sex can be reported in the text.
ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/overall_race_plot.pdf',
       plot = overall_race_plot,
       device = 'pdf',
       width = 7,
       height = 4,
       dpi = 400)


# Number of postdoc positions
overall_num.postdoc.positions <- return_count_and_percent_breakdown(in_df = clean_data,
                                                                    column = 'num_postdocs')

overall_num.postdoc.positions$category <- factor(overall_num.postdoc.positions$category,
                                                 levels = c('<1', '1', '2', '3', '4'))

overall_num.postdoc.positions_plot <- ggplot(data = overall_num.postdoc.positions, aes(x = category, y = percent)) +
  geom_col(position = 'stack', fill = '#111b42') +
  geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.1) +
  theme_bw() +
  xlab('Number of postdoc positions') +
  ylab(' Percentage of\nall respondents') +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5))

num_postdocs_numeric <- clean_data$num_postdocs
num_postdocs_numeric[which(num_postdocs_numeric == '<1')] <- '0'
num_postdocs_numeric <- as.integer(num_postdocs_numeric)
format(mean(num_postdocs_numeric), digits = 3)
format(sd(num_postdocs_numeric), , digits = 3)

## Number of years as postdoc
overall_num.years.postdoc <- return_count_and_percent_breakdown(in_df = clean_data,
                                                                column = 'years_postdoc')

overall_num.years.postdoc$category <- factor(overall_num.years.postdoc$category,
                                             levels = c('<1', '1', '2', '3', '4', '5+'))

overall_num.years.postdoc_plot <- ggplot(data = overall_num.years.postdoc, aes(x = category, y = percent)) +
  geom_col(position = 'stack', fill = '#111b42') +
  geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.1) +
  theme_bw() +
  xlab('Years as postdoc') +
  ylab(' Percentage of\nall respondents') +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5))

# Summary metrics for text:
num_years_numeric <- clean_data$years_postdoc
num_years_numeric[which(num_years_numeric == '<1')] <- '0'
num_years_numeric[which(num_years_numeric == '5+')] <- '5'
num_years_numeric <- as.integer(num_years_numeric)
format(mean(num_years_numeric), digits = 3)
format(sd(num_years_numeric), digits = 3)
format(length(which(num_years_numeric > 1)) / length(num_years_numeric) * 100, digits = 3)

postdoc_time_summary <- cowplot::plot_grid(overall_num.postdoc.positions_plot,
                                           overall_num.years.postdoc_plot,
                                           nrow = 1,
                                           labels = c('a', 'b'),
                                           rel_widths = c(1, 1.15))

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/postdoc_overall_time_barplot.pdf',
       plot = postdoc_time_summary,
       device = 'pdf',
       width = 9,
       height = 4,
       dpi = 400)
