library(readr)
library(dplyr)
library(qdapTools)
library(reshape2)
library(stringr)

source_csvs = dir('data')[grep('csv$', dir('data'))]

original_data = bind_rows(lapply(file.path('data', source_csvs), read_csv, col_types=cols(.default=col_character())))

# 1 - timestamp
# 2 - acknowledgement
# 3 - english first language
# 4 - english fluency
# 5 - other languages
# 6 - enjoy puns
# 7:36 - Puns
# 37 - gender
# 38 - race
# 39 - hispanic ethnicity
# 40 - age group
# 41 - location
# 42 - email (contest only)
names(original_data)[42] = 'email'

filtered_data = original_data %>% filter(
  # acknowledgement
  `Do you understand and accept the terms, including the privacy notice, above?`=='Yes',
  # email duplication
  (is.na(email) | !(duplicated(email) | duplicated(email, fromLast=TRUE))),
  # English fluency inconsistency
  !(`Are you fluent in English?` == 'No' & `Is English your first language?`=='Yes')
) 

# only matches were people who really hated all of these lol
#filtered_data = filtered_data[(duplicated(filtered_data[,7:36]) | 
#                                  duplicated(filtered_data[,7:36], fromLast=TRUE)), ]

# rename columns
names(filtered_data)[3:6] = c(
  'EnglishFirstLanguage',
  'EnglishFluency',
  'OtherLanguages',
  'EnjoysPuns'
)

names(filtered_data)[c(7:36)] = paste0('Q',1:30)

names(filtered_data)[37:41] = c(
  'gender',
  'race',
  'hispanic',
  'age',
  'location'
)


# winners code
if (FALSE){
  emails = filtered_data$email[!is.na(filtered_data$email)]# & filtered_data$age != 'Under 18']
  
  winners = sample(emails, 13)
  runner_ups = sample(emails[!emails %in% winners], 60)
  
  write_csv(
    data.frame(
      email=c(winners, runner_ups),
      status=c(rep('winner', 13), rep('runner-up', 60)),
      prize=c('20','7.89','7.89', rep('1', 10), rep('', 60))
    ),
    path='winners.csv'
  )
}

filtered_data$Timestamp=NULL
filtered_data$email=NULL

# randomize order for extra anonymity
write_csv(filtered_data[order(runif(nrow(filtered_data))),], 'puns.csv')
