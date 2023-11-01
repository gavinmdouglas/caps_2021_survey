rm(list = ls(all.names = TRUE))

library(ggplot2)
library(cowplot)
library(ComplexHeatmap)
library(reshape2)
library(circlize)

source('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/CAPS_survey_analysis/0 - utility functions.R')

# Province by (for first and second work permits):
  # (1) Timing satisfaction.
  # (2) Related to COVID 

clean_data <- read.table('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified_prepped.txt',
                         header = TRUE, sep = '\t', stringsAsFactors = FALSE)

clean_data <- clean_data[which(! is.na(clean_data$province_min20)), ]
clean_data$province_min20 <- factor(clean_data$province_min20, levels = c('Alberta', 'British Columbia', 'Ontario', 'Quebec', 'Saskatchewan'))

clean_data_filt_for_second <- clean_data
clean_data_filt_for_second <- clean_data_filt_for_second[which(clean_data_filt_for_second$extended_or_applied_for_second == 'Yes'), ]
clean_data_filt_for_second <- clean_data_filt_for_second[which(clean_data_filt_for_second$years_postdoc != '<1'), ]
clean_data_filt_for_second$second_permit_wait[which(clean_data_filt_for_second$second_permit_wait == 'Other')] <- NA
clean_data_filt_for_second <- clean_data_filt_for_second[which(! is.na(clean_data_filt_for_second$second_permit_wait)), ]
clean_data_filt_for_second <- clean_data_filt_for_second[which(! is.na(clean_data_filt_for_second$second_permit_timing_satisfaction)), ]

# Timing satisfaction.
kw_first_satis_by_province <- kruskal.test(first_permit_timing_satisfaction ~ province_min20, data = clean_data)

boxplot_first_satis_by_province <- ggplot(data = clean_data, aes(x = province_min20, y = first_permit_timing_satisfaction)) +
  geom_boxplot(fill = 'deepskyblue3',) +
  theme_bw() +
  xlab('Province') +
  ylab('Satisfaction with permit wait-time') +
  ggtitle('First work permit\n(Kruskal-Wallis Chi-squared: 0.99, df = 4, P = 0.91)') +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

kw_second_satis_by_province <- kruskal.test(second_permit_timing_satisfaction ~ province_min20, data = clean_data_filt_for_second)

boxplot_second_satis_by_province <- ggplot(data = clean_data_filt_for_second, aes(x = province_min20, y = second_permit_timing_satisfaction)) +
  geom_boxplot(fill = 'deepskyblue3',) +
  theme_bw() +
  xlab('Province') +
  ylab('Satisfaction with permit wait-time') +
  ggtitle('Second work permit\n(Kruskal-Wallis Chi-squared: 13.88, df = 4, P = 0.0077)') +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Quebec stats:
mean(clean_data_filt_for_second$second_permit_timing_satisfaction[which(clean_data_filt_for_second$province_min20 == 'Quebec')])
sd(clean_data_filt_for_second$second_permit_timing_satisfaction[which(clean_data_filt_for_second$province_min20 == 'Quebec')])

mean(clean_data_filt_for_second$second_permit_timing_satisfaction[which(clean_data_filt_for_second$province_min20 != 'Quebec')])
sd(clean_data_filt_for_second$second_permit_timing_satisfaction[which(clean_data_filt_for_second$province_min20 != 'Quebec')])

combined_satisfaction_by_prov <- plot_grid(boxplot_first_satis_by_province, boxplot_second_satis_by_province, labels = c('a', 'b'), nrow = 2)


ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/waittime_satisfaction_by_province.pdf',
       plot = combined_satisfaction_by_prov,
       device = 'pdf',
       width = 6,
       height = 8,
       dpi = 400)


# Wait-times by province.
first_permit_waittime_prov_summary <- return_count_and_percent_breakdown_by_provinceMin20(in_df = clean_data, column = 'first_permit_wait')
first_permit_waittime_prov_count <- reshape2::acast(data = first_permit_waittime_prov_summary, formula = province ~ category, value.var = 'count')
first_permit_waittime_prov_percent <- reshape2::acast(data = first_permit_waittime_prov_summary, formula = province ~ category, value.var = 'percent')
first_permit_waittime_prov_count <- first_permit_waittime_prov_count[, c('< 1', '1-3', '4-6', '> 6')]
first_permit_waittime_prov_percent <- first_permit_waittime_prov_percent[, c('< 1', '1-3', '4-6', '> 6')]
colnames(first_permit_waittime_prov_percent) <- paste('first', colnames(first_permit_waittime_prov_percent))

second_permit_waittime_prov_summary <- return_count_and_percent_breakdown_by_provinceMin20(in_df = clean_data_filt_for_second, column = 'second_permit_wait')
second_permit_waittime_prov_count <- reshape2::acast(data = second_permit_waittime_prov_summary, formula = province ~ category, value.var = 'count')
second_permit_waittime_prov_percent <- reshape2::acast(data = second_permit_waittime_prov_summary, formula = province ~ category, value.var = 'percent')
second_permit_waittime_prov_count <- second_permit_waittime_prov_count[, c('< 1', '1-3', '4-6', '> 6')]
second_permit_waittime_prov_percent <- second_permit_waittime_prov_percent[, c('< 1', '1-3', '4-6', '> 6')]
colnames(second_permit_waittime_prov_percent) <- paste('second', colnames(second_permit_waittime_prov_percent))

combined_permit_waittime_prov_percent <- cbind(first_permit_waittime_prov_percent, second_permit_waittime_prov_percent)

combined_permit_waittime_prov_info <- apply(combined_permit_waittime_prov_percent, 2, function(x){ paste(as.character(round(x, 1)), '%', sep = '')})
combined_permit_waittime_prov_info[, 1] <- paste(combined_permit_waittime_prov_info[, 1], ' (', as.character(first_permit_waittime_prov_count[, 1]), ')', sep = '')
combined_permit_waittime_prov_info[, 2] <- paste(combined_permit_waittime_prov_info[, 2], ' (', as.character(first_permit_waittime_prov_count[, 2]), ')', sep = '')
combined_permit_waittime_prov_info[, 3] <- paste(combined_permit_waittime_prov_info[, 3], ' (', as.character(first_permit_waittime_prov_count[, 3]), ')', sep = '')
combined_permit_waittime_prov_info[, 4] <- paste(combined_permit_waittime_prov_info[, 4], ' (', as.character(first_permit_waittime_prov_count[, 4]), ')', sep = '')

combined_permit_waittime_prov_info[, 5] <- paste(combined_permit_waittime_prov_info[, 5], ' (', as.character(second_permit_waittime_prov_count[, 1]), ')', sep = '')
combined_permit_waittime_prov_info[, 6] <- paste(combined_permit_waittime_prov_info[, 6], ' (', as.character(second_permit_waittime_prov_count[, 2]), ')', sep = '')
combined_permit_waittime_prov_info[, 7] <- paste(combined_permit_waittime_prov_info[, 7], ' (', as.character(second_permit_waittime_prov_count[, 3]), ')', sep = '')
combined_permit_waittime_prov_info[, 8] <- paste(combined_permit_waittime_prov_info[, 8], ' (', as.character(second_permit_waittime_prov_count[, 4]), ')', sep = '')

# Set NA value to be 0%.
combined_permit_waittime_prov_info[5, 8] <- "0% (0)"
combined_permit_waittime_prov_percent[is.na(combined_permit_waittime_prov_percent)] <- 0
permit_waittime_prov_heatmap <- ComplexHeatmap::Heatmap(matrix = as.matrix(combined_permit_waittime_prov_percent),
                                                     name = "Percent",
                                                     row_names_side = "left",
                                                     cluster_rows = TRUE,
                                                     show_row_dend = FALSE,
                                                     column_split = c('First permit', 'First permit', 'First permit', 'First permit',
                                                                      'Second permit', 'Second permit', 'Second permit', 'Second permit'),
                                                     column_labels = c('< 1', '1-3', '4-6', '> 6',
                                                                       '< 1', '1-3', '4-6', '> 6'),
                                                     column_gap = unit(5, "mm"),
                                                     column_title = 'Permit wait-time',
                                                     column_title_side = 'bottom',
                                                     cluster_columns = FALSE,
                                                     column_names_rot = 45,
                                                     col = colorRamp2(c(0, 100), c("white", "red")),
                                                     cell_fun = function(j, i, x, y, width, height, fill) {
                                                       grid.text(combined_permit_waittime_prov_info[i, j], x, y, gp = gpar(fontsize = 10, fontface="bold"))
                                                     })

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/permit_waittime_by_prov.pdf',
       plot = grid.grabExpr(draw(permit_waittime_prov_heatmap,
                                 column_title_gp=grid::gpar(fontsize=16))),
       device = 'pdf',
       width = 10,
       height = 6,
       dpi = 400)

second_permit_waittime_prov_count[is.na(second_permit_waittime_prov_count)] <- 0
fisher.test(as.matrix(first_permit_waittime_prov_count), simulate.p.value=TRUE, B=1e7)
fisher.test(as.matrix(second_permit_waittime_prov_count), simulate.p.value=TRUE, B=1e7)


# COVID impacts.

first_permit_covid_prov_summary <- return_count_and_percent_breakdown_by_provinceMin20(in_df = clean_data, column = 'first_covid_related')
first_permit_covid_prov_count <- reshape2::acast(data = first_permit_covid_prov_summary, formula = province ~ category, value.var = 'count')
first_permit_covid_prov_percent <- reshape2::acast(data = first_permit_covid_prov_summary, formula = province ~ category, value.var = 'percent')
colnames(first_permit_covid_prov_percent) <- paste('first', colnames(first_permit_covid_prov_percent))

second_permit_covid_prov_summary <- return_count_and_percent_breakdown_by_provinceMin20(in_df = clean_data_filt_for_second, column = 'second_covid_related')
second_permit_covid_prov_count <- reshape2::acast(data = second_permit_covid_prov_summary, formula = province ~ category, value.var = 'count')
second_permit_covid_prov_percent <- reshape2::acast(data = second_permit_covid_prov_summary, formula = province ~ category, value.var = 'percent')
colnames(second_permit_covid_prov_percent) <- paste('second', colnames(second_permit_covid_prov_percent))

combined_permit_covid_prov_percent <- cbind(first_permit_covid_prov_percent, second_permit_covid_prov_percent)

combined_permit_covid_prov_info <- apply(combined_permit_covid_prov_percent, 2, function(x){ paste(as.character(round(x, 1)), '%', sep = '')})
combined_permit_covid_prov_info[, 1] <- paste(combined_permit_covid_prov_info[, 1], ' (', as.character(first_permit_covid_prov_count[, 1]), ')', sep = '')
combined_permit_covid_prov_info[, 2] <- paste(combined_permit_covid_prov_info[, 2], ' (', as.character(first_permit_covid_prov_count[, 2]), ')', sep = '')

combined_permit_covid_prov_info[, 3] <- paste(combined_permit_covid_prov_info[, 3], ' (', as.character(second_permit_covid_prov_count[, 1]), ')', sep = '')
combined_permit_covid_prov_info[, 4] <- paste(combined_permit_covid_prov_info[, 4], ' (', as.character(second_permit_covid_prov_count[, 2]), ')', sep = '')


permit_covid_prov_heatmap <- ComplexHeatmap::Heatmap(matrix = as.matrix(combined_permit_covid_prov_percent),
                                                name = "Percent",
                                                row_names_side = "left",
                                                cluster_rows = TRUE,
                                                show_row_dend = FALSE,
                                                column_split = c('First permit', 'First permit', 'Second permit', 'Second permit'),
                                                column_labels = c('No', 'Yes', 'No', 'Yes'),
                                                column_gap = unit(5, "mm"),
                                                cluster_columns = FALSE,
                                                column_names_rot = 45,
                                                col = colorRamp2(c(0, 100), c("white", "red")),
                                                cell_fun = function(j, i, x, y, width, height, fill) {
                                                  grid.text(combined_permit_covid_prov_info[i, j], x, y, gp = gpar(fontsize = 10, fontface="bold"))
                                                })

ggsave(filename = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/plots/covid_by_province.pdf',
       plot = grid.grabExpr(draw(permit_covid_prov_heatmap)),
       device = 'pdf',
       width = 6,
       height = 5,
       dpi = 400)

fisher.test(as.matrix(first_permit_covid_prov_count))
fisher.test(as.matrix(second_permit_covid_prov_count))
