# CAPS 2021 Postdoc Survey Analysis

Code used for processing and analyzing data associated with the soon-to-be published [Canadian Association of Postdoctoral Scholars](https://www.caps-acsp.ca/en/) (CAPS) 2021 survey. This was in collaboration with [Science & Policy Exchange](https://www.sp-exchange.ca/).

Scripts are ordered numerically:

* `0_utility_functions.R` - Functions to prepare the data in a consistent format of category counts/percents for plotting.
* `1_preprocess_data.R` - Commands used to process the raw survey table, which was then used for all downstream quantitative analyses.
* `2_demographic_breakdown.R` - Breakdown of geographical distribution, demographic factors and postdoc experience of respondents.
* `3_overall_waittimes_and_issues.R` - Breakdown of work permit wait times and related factors, such as whether the COVID-19 pandemic negatively impacted wait times.
* `4_first_permit_timing_predictions.R` - Exploratory analysis to test whether timing to receive first work permit was predictive of whether respondents applied for a second work permit. This analysis was dropped for the report.
* `5_province_by_impacts.R` - Similar analyses on wait times and COVID-19 impacts as above, but broken down by province.
