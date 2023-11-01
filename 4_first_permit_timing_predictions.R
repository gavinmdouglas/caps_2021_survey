rm(list = ls(all.names = TRUE))

# Investigation to what degree satisfaciton with the first permit timing 

clean_data <- read.table('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified_prepped.txt',
                         header = TRUE, sep = '\t', stringsAsFactors = FALSE)

# Ignore postdocs that have been postdocs for < 2 years.
clean_data <- clean_data[which(! clean_data$years_postdoc %in% c('<1', '1')), ]

clean_data$years_postdoc[which(clean_data$years_postdoc == '5+')] <- '5'
clean_data$years_postdoc <- as.integer(clean_data$years_postdoc)

clean_data[which(is.na(clean_data$province_min20)), 'province_min20'] <- 'Other'

clean_data$extended_or_applied_for_second <- factor(clean_data$extended_or_applied_for_second,
                                                    levels = c('Yes', 'No'))

clean_data$extended_or_applied_for_second_int <- 0
clean_data$extended_or_applied_for_second_int[which(clean_data$extended_or_applied_for_second == 'Yes')] <- 1

regress_second_permit_applied_no_province <- glm(clean_data$extended_or_applied_for_second_int ~ first_permit_timing_satisfaction,
                                                data = clean_data,
                                                family = 'binomial')

regress_second_permit_applied_w_province <- glm(clean_data$extended_or_applied_for_second_int ~ first_permit_timing_satisfaction + province_min20,
                                                 data = clean_data,
                                                 family = 'binomial')

regress_second_permit_applied_no_province_summary <- summary(regress_second_permit_applied_no_province)
regress_second_permit_applied_no_province_summary$coefficients

regress_second_permit_applied_w_province_summary <- summary(regress_second_permit_applied_w_province)
regress_second_permit_applied_w_province_summary$coefficients