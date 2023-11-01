# Function to get counts and breakdown for categorical column.
# Used to simplify input for plotting.
return_count_and_percent_breakdown <- function(in_df, column) {
  # First, remove any NA rows for this column.
  in_df <- in_df[which(! is.na(in_df[, column])), ]
  
  category_tallies <- table(in_df[, column])
  category_percent <- (as.numeric(category_tallies) / sum(category_tallies) * 100)
  
  return(data.frame(category = names(category_tallies),
                    count = as.integer(category_tallies),
                    percent = category_percent))
  
}

# Get category broken down separately by province.
# Hard-coded for provinces already filtered to have at least 20 samples.
return_count_and_percent_breakdown_by_provinceMin20 <- function(in_df, column) {
  
  # First, remove any NA rows for this column.
  in_df <- in_df[which(! is.na(in_df[, column])), ]
  
  province_levels <- unique(in_df$province_min20)
  province_levels <- province_levels[which(! is.na(province_levels))]
  
  province_breakdowns <- list()
  
  for (province in province_levels) {
    
    in_df_subset <- in_df[which(in_df$province_min20 == province), ]
    
    category_tallies <- table(in_df_subset[, column])
    
    category_percent <- (as.numeric(category_tallies) / sum(category_tallies) * 100)
    
    province_breakdowns[[province]] <- data.frame(category = names(category_tallies),
                                                  count = as.integer(category_tallies),
                                                  percent = category_percent,
                                                  province = province)
  }
  
  return(do.call(rbind, province_breakdowns))
  
}

# Similar to the province function above, but used to the get
# the breakdown of counts and percents of a category by race/ethnicity.
return_count_and_percent_breakdown_by_race <- function(in_df, column) {
  
  # First, remove any NA rows for this column.
  in_df <- in_df[which(! is.na(in_df[, column])), ]
  
  race_levels <- unique(in_df$race_ethnicity)
  race_levels <- race_levels[which(! is.na(race_levels))]
  
  race_breakdowns <- list()
  
  for (race in race_levels) {
    
    in_df_subset <- in_df[which(in_df$race_ethnicity == race), ]
    
    category_tallies <- table(in_df_subset[, column])
    
    category_percent <- (as.numeric(category_tallies) / sum(category_tallies) * 100)
    
    race_breakdowns[[race]] <- data.frame(category = names(category_tallies),
                                          count = as.integer(category_tallies),
                                          percent = category_percent,
                                          race = race)
  }
  
  race_breakdown_combined <- do.call(rbind, race_breakdowns)
  
  race_breakdown_combined$race <- factor(race_breakdown_combined$race,
                                         levels = c('European/Caucasian',
                                                    'Asian',
                                                    'Middle Eastern',
                                                    'Hispanic',
                                                    'African',
                                                    'Other'))
  return(race_breakdown_combined)
  
}
