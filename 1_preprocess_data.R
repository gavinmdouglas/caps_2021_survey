rm(list = ls(all.names = TRUE))

# Pre-process survey table and prepare for downstream analyses.

survey <- read.table('/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified.txt',
                     header = TRUE, sep = '\t', stringsAsFactors = FALSE)

# Remove one sample with all missing data.
num_missing_columns_per_row <- rowSums(survey == '', na.rm = TRUE) + rowSums(is.na(survey))
survey <- survey[-which(num_missing_columns_per_row == 19), ]

# Note that only one person identified as Indigenous, so that
# variable was not considered here.
table(survey$Do.you.identify.as.an.Indigenous.Person.)

# Num postdocs
num_postdocs <- survey$How.many.postdoctoral.appointments.have.you.held.in.Canada.

# Years postdoc
years_postdoc <- survey$In.sum..for.how.many.years.have.you.been.a.postdoc.in.Canada.
years_postdoc[which(years_postdoc == 'more')] <- '5+'

# First permit wait time
first_permit_wait <- survey$How.long.did.it.take.to.receive.your.first.work.permit.
first_permit_wait[which(first_permit_wait == '<1 month')] <- '< 1'
first_permit_wait[which(first_permit_wait == '1-3 months')] <- '1-3'
first_permit_wait[which(first_permit_wait == '4-6 months')] <- '4-6'
first_permit_wait[which(first_permit_wait == 'More than 6 months')] <- '> 6'

# First permit timing satisfaction
first_permit_timing_satisfaction <- survey$How.satisfied.are.you.with.the.timing.for.the.issuance.of.your.first.work.permit.

# Extended or applied for a second work permit (in Canada)?
extended_or_applied_for_second <- survey$Have.you.extended.your.work.permit.applied.for.a.second.work.permit.while.in.Canada.

# How.long.did.it.take.to.receive.your.second.work.permit
second_permit_wait <- survey$How.long.did.it.take.to.receive.your.second.work.permit.
# Note that values set as '' (presumably missing) were set as NA. 
second_permit_wait[which(second_permit_wait == '')] <- NA 
second_permit_wait[which(second_permit_wait == '<1 months')] <- '< 1'
second_permit_wait[which(second_permit_wait == '1-3 months')] <- '1-3'
second_permit_wait[which(second_permit_wait == '4-6 months')] <- '4-6'
second_permit_wait[which(second_permit_wait == 'More than 6 months')] <- '> 6'

# Second permit timing satisfaction
second_permit_timing_satisfaction <- survey$How.satisfied.are.you.with.the.timing.for.the.issuance.of.your.second.work.permit.

# Note that in several cases that 'Other' as response to the time it took to receive the second work permit was 'NA' in second
# permit timing satifaction so they were ignored (as we assumed 'Other' was meant to mean NA).
# length(which(second_permit_wait == 'Other' & is.na(second_permit_timing_satisfaction)))
second_permit_wait[which(second_permit_wait == 'Other' & is.na(second_permit_timing_satisfaction))] <- NA


# Work permit issues related to COVID-19 pandemic?

# For first work permit:
first_covid_related <- survey$Were.the.issues.you.have.encountered..regarding.your.work.permit.application.related.to.the.COVID19.pandemic.
first_covid_related[which(first_covid_related == '')] <- NA
first_covid_related[which(first_covid_related == 'I did not experience any issues or difficulties')] <- NA
first_covid_related[which(first_covid_related == 'no, my issues predated the pandemic or occured independent from any issues the pandemic has caused')] <- 'No'
first_covid_related[which(first_covid_related == 'yes, my issues were related to the COVID19 pandemic')] <- 'Yes'

# For second work permit:
second_covid_related <- survey$Were.the.issues.you.have.encountered..regarding.your.work.permit.extension.related.to.the.COVID19.pandemic.
second_covid_related[which(second_covid_related == '')] <- NA
second_covid_related[which(second_covid_related == 'I did not experience any issues or difficulties')] <- NA
second_covid_related[which(second_covid_related == 'no, my issues predated the pandemic or occured independent from any issues the pandemic has caused')] <- 'No'
second_covid_related[which(second_covid_related == 'yes, my issues were related to the COVID19 pandemic')] <- 'Yes'


# Has.your.closed.work.permit.prevented.you.from.accepting.a.new.position.
new_position_prevent <- survey$Has.your.closed.work.permit.prevented.you.from.accepting.a.new.position.
new_position_prevent[which(new_position_prevent == 'No and this doesnt concern me')] <- 'No (and not concerned)'
new_position_prevent[which(new_position_prevent == 'No, but I am concerned that this might happen')] <- 'No (but concerned)'
new_position_prevent[which(new_position_prevent == 'Not applicable / I was not offered a new position yet')] <- 'Not applicable and/or not offered new position'
new_position_prevent[which(new_position_prevent == 'Yes, I was not offered a specific position due to my closed work permit')] <- 'Yes'
new_position_prevent[which(new_position_prevent == 'Yes, I was not offered the position due to my closed work permit')] <- 'Yes'
new_position_prevent[which(new_position_prevent == 'Yes, I was offered a position but could not accept it')] <- 'Yes'


# Province
province <- survey$Where.in.Canada.is.your.employer.employing.organization.located.


# Province, but with Nova Scotia and Nunavut samples (3) set to NA due to low N.
province_min20 <- province
province_min20[which(province_min20 %in% c('Nova Scotia', 'Nunavut'))] <- NA


# Sex
sex <- survey$How.do.you.identify.your.gender.
sex[which(sex == 'Prefer to not answer')] <- 'No answer'



# Disability
disabled_person <- survey$For.the.purpose.of.this.survey..a.Person.with.a.Disability.means.a.person.who.has.a.long.term.or.recurring.physical..mental..sensory..psychiatric.or.learning.disability..Based.on.the.description..Are.you.a.Person.with.a.Disability.
disabled_person[which(disabled_person == '')] <- NA


# Race/ethnicity
race_ethnicity <- survey$For.the.purpose.of.this.survey..how.would.you.describe.your.race.or.ethnicity.


# Then add in all these variables into a clean data.frame:
clean_data <- data.frame(
  num_postdocs = num_postdocs,
  years_postdoc = years_postdoc,
  first_permit_wait = first_permit_wait,
  first_permit_timing_satisfaction = first_permit_timing_satisfaction,
  extended_or_applied_for_second = extended_or_applied_for_second,
  second_permit_wait = second_permit_wait,
  second_permit_timing_satisfaction = second_permit_timing_satisfaction,
  first_covid_related = first_covid_related,
  second_covid_related = second_covid_related,
  new_position_prevent = new_position_prevent,
  province = province,
  province_min20 = province_min20,
  sex = sex,
  disabled_person = disabled_person,
  race_ethnicity = race_ethnicity)

write.table(x = clean_data,
            file = '/Users/gavin/Drive/science_policy/SPE/data/2023.05.03_CAPS_survey/2021_CAPS_Workpermit_immigration_survey_responses_deidentified_prepped.txt',
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = '\t')
