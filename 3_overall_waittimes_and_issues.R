rm(list = ls(all.names = TRUE))

# Summarize basic statistics across *all* postdocs:
  # Wait times for permits
  # Satisfaction with wait times
  # Impact of COVID pandemic
  # Impact on acquiring position

# The only plot saved is actually just the wait times and satisfactions.
# The rest is clearer to just describe as percentages in the text.

library(cowplot)
library(ggplot2)
library(ggbeeswarm)

source('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/CAPS_survey_analysis/caps_2021_survey/0_utility_functions.R')

clean_data <- read.table('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified_prepped.txt',
                         header = TRUE, sep = '\t', stringsAsFactors = FALSE)

# Wait-time for work permit(s)
overall_first.wait.time <- return_count_and_percent_breakdown(in_df = clean_data,
                                                              column = 'first_permit_wait')

# Filter out ambiguous/questionable responses regarding second permit.
clean_data_filt_for_second <- clean_data
clean_data_filt_for_second <- clean_data_filt_for_second[which(clean_data_filt_for_second$extended_or_applied_for_second == 'Yes'), ]
clean_data_filt_for_second <- clean_data_filt_for_second[which(clean_data_filt_for_second$years_postdoc != '<1'), ]
clean_data_filt_for_second$second_permit_wait[which(clean_data_filt_for_second$second_permit_wait == 'Other')] <- NA
clean_data_filt_for_second <- clean_data_filt_for_second[which(! is.na(clean_data_filt_for_second$second_permit_wait)), ]
clean_data_filt_for_second <- clean_data_filt_for_second[which(! is.na(clean_data_filt_for_second$second_permit_timing_satisfaction)), ]

overall_second.wait.time <- return_count_and_percent_breakdown(in_df = clean_data_filt_for_second,
                                                               column = 'second_permit_wait')

overall_first.wait.time$Permit <- 'First work permit'
overall_second.wait.time$Permit <- 'Second work permit'

overall_wait.time <- rbind(overall_first.wait.time, overall_second.wait.time)

overall_wait.time$category <- factor(overall_wait.time$category,
                                     levels = c('< 1', '1-3', '4-6', '> 6'))

overall_wait.time_plot <- ggplot(data = overall_wait.time, aes(x = category, y = percent)) +
                                  geom_col(position = 'stack', fill = '#111b42') +
                                  geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.25) +
                                  theme_bw() +
                                  xlab('Months to receive work permit') +
                                  ylab(' Percentage of\n  respondents') +
                                  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                                  facet_wrap(Permit ~ .)


# Satisfaction regarding wait-time for work permit(s)

overall_first.satisfaction <- return_count_and_percent_breakdown(in_df = clean_data,
                                                                 column = 'first_permit_timing_satisfaction')

overall_second.satisfaction <- return_count_and_percent_breakdown(in_df = clean_data_filt_for_second,
                                                                  column = 'second_permit_timing_satisfaction')

overall_first.satisfaction$Permit <- 'First work permit'
overall_second.satisfaction$Permit <- 'Second work permit'

overall_satisfaction <- rbind(overall_first.satisfaction, overall_second.satisfaction)

overall_satisfaction$category <- factor(overall_satisfaction$category)

overall_satisfaction_plot <- ggplot(data = overall_satisfaction, aes(x = category, y = percent)) +
                                geom_col(position = 'stack', fill = '#111b42') +
                                geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.25) +
                                theme_bw() +
                                xlab('Satisfaction with permit wait time') +
                                ylab(' Percentage of\n  respondents') +
                                theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                                facet_wrap(Permit ~ ., scales = 'free_x')


# Combined wait-time plot.
combined_waittime_figure <- plot_grid(overall_wait.time_plot, overall_satisfaction_plot, nrow = 2, labels = c('a', 'b'))

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/waittime_summary.pdf',
       plot = combined_waittime_figure,
       device = 'pdf',
       width = 6,
       height = 6,
       dpi = 400)


# Wait times by satisfaction (which is largely a sanity check!)
clean_data_tmp <- clean_data
clean_data_tmp$Permit <- 'First work permit'
clean_data_tmp$first_permit_wait <- factor(clean_data_tmp$first_permit_wait, levels = c('< 1', '1-3', '4-6', '> 6'))
waittime_by_satisfaction_permit1 <- ggplot(data = clean_data_tmp, aes(x = first_permit_wait, y = first_permit_timing_satisfaction)) +
                                              geom_boxplot() +
                                              theme_bw() +
                                              xlab('Permit wait-time') +
                                              ylab(' Satisfacton\nwith wait time') +
                                              theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                                              facet_wrap(Permit ~ ., scales = 'free_x')

clean_data_filt_for_second_tmp <- clean_data_filt_for_second
clean_data_filt_for_second_tmp$Permit <- 'Second work permit'
clean_data_filt_for_second_tmp$second_permit_wait <- factor(clean_data_filt_for_second_tmp$second_permit_wait, levels = c('< 1', '1-3', '4-6', '> 6'))
waittime_by_satisfaction_permit2 <- ggplot(data = clean_data_filt_for_second_tmp, aes(x = second_permit_wait, y = second_permit_timing_satisfaction)) +
                                          geom_boxplot() +
                                          theme_bw() +
                                          xlab('Permit wait-time') +
                                          ylab(' Satisfacton\nwith wait time') +
                                          theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
                                          facet_wrap(Permit ~ ., scales = 'free_x')

waittime_by_satisfaction_combined <- plot_grid(waittime_by_satisfaction_permit1, waittime_by_satisfaction_permit2, nrow = 1)

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/waittime_by_satisfaction.pdf',
       plot = waittime_by_satisfaction_combined,
       device = 'pdf',
       width = 8,
       height = 3,
       dpi = 400)

# And correlation coefficients to report:
waittime_by_satisfaction_permit1_cor <- cor.test(as.integer(clean_data_tmp$first_permit_wait),
                                                 clean_data_tmp$first_permit_timing_satisfaction,
                                                 method = 'spearman', exact = FALSE)

waittime_by_satisfaction_permit2_cor <- cor.test(as.integer(clean_data_filt_for_second_tmp$second_permit_wait),
                                                 clean_data_filt_for_second_tmp$second_permit_timing_satisfaction,
                                                 method = 'spearman', exact = FALSE)

round(waittime_by_satisfaction_permit1_cor$estimate, 3)
round(waittime_by_satisfaction_permit1_cor$p.value, 3)
round(waittime_by_satisfaction_permit2_cor$estimate, 3)
round(waittime_by_satisfaction_permit2_cor$p.value, 3)



# Extended or applied for second permit?
# Compute counts and %'s:
# clean_data_above_1_year <- clean_data[which(! clean_data$years_postdoc %in% c('<1', '1')), ]
# table(clean_data_above_1_year$extended_or_applied_for_second)
# round((table(clean_data_above_1_year$extended_or_applied_for_second) / sum(table(clean_data_above_1_year$extended_or_applied_for_second))) * 100, 2)
# 
# overall_extended.or.not <- return_count_and_percent_breakdown(in_df = clean_data,
#                                                               column = 'extended_or_applied_for_second')
# 
# overall_extended.or.not$category <- factor(overall_extended.or.not$category)
# 
# overall_extended.or.not_plot <- ggplot(data = overall_extended.or.not, aes(x = category, y = percent)) +
#   geom_col(position = 'stack', fill = '#111b42') +
#   geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1) +
#   theme_bw() +
#   xlab('Extended or applied for second permit') +
#   ylab(' Percentage of\nall respondents') +
#   theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5))

# Issues related to COVID-19 pandemic?

overall_first.covid19 <- return_count_and_percent_breakdown(in_df = clean_data,
                                                            column = 'first_covid_related')

overall_second.covid19 <- return_count_and_percent_breakdown(in_df = clean_data_filt_for_second,
                                                             column = 'second_covid_related')

overall_first.covid19$Permit <- 'First work permit'
overall_second.covid19$Permit <- 'Second work permit'

overall_covid19 <- rbind(overall_first.covid19, overall_second.covid19)

overall_covid19$category <- factor(overall_covid19$category)

# Key statistics here:
overall_covid19

overall_covid19_plot <- ggplot(data = overall_covid19, aes(x = category, y = percent)) +
  geom_col(position = 'stack', fill = '#111b42') +
  geom_text(aes(x = category, y = percent, label = count), colour = 'white', vjust = 1.25) +
  theme_bw() +
  xlab('Issues related to COVID-19 pandemic?') +
  ylab(' Percentage of\n  respondents') +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = -0.5)) +
  facet_wrap(Permit ~ ., scales = 'free_x')


# Did your closed work permit prevent you from accepting a new position?
# Key comparison is between relative abundance of No's and Yes'
clean_data_above_1_year_and_clear_answer <- clean_data_above_1_year
clean_data_above_1_year_and_clear_answer$new_position_prevent[grep('Not applicable', clean_data_above_1_year_and_clear_answer$new_position_prevent)] <- NA

overall_new_position_prevent <- return_count_and_percent_breakdown(in_df = clean_data_above_1_year_and_clear_answer,
                                                                   column = 'new_position_prevent')

# Statistical test on first vs. second wait-times.
# (Focused on comparing paired samples of postdocs who responded to both questions).
wilcox_first_vs_second_paired_satis <- wilcox.test(clean_data_filt_for_second$first_permit_timing_satisfaction,
                                                   clean_data_filt_for_second$second_permit_timing_satisfaction,
                                                   paired = TRUE)

round(wilcox_first_vs_second_paired_satis$p.value, 3)
