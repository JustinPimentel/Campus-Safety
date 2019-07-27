library(tidyverse)
library(stringr)
setwd('/Users/justinpimentel/Downloads/Personal Project')

fileNames = list.files('OPE CSS Custom Data 2019-06-23 184005', pattern = '*.csv', full.names = T)
fileNames2 = list.files('OPE CSS Custom Data 2019-06-23 184005', pattern = '*.csv', full.names = F)
data <- lapply(fileNames, function(x) {read.csv(x, stringsAsFactors = F, quote = "", row.names = NULL)})
locationsData <- read.csv('Most-Recent-Cohorts-All-Data-Elements.csv') 


###################################################################################################
types = (c('local_state_police.csv','noncampus.csv','public_property.csv',
          '^(?!total_fires).*_student_housing_facilities.csv','unfounded_crimes.csv', 
          '^total_fires', 'detailed_fire_data.csv', 'on_campus.csv'))

for (x in 1:length(data)){
  switch(types[sapply(types, function(type) grepl(type,tolower(fileNames2[x]), perl = T))], 
            "local_state_police.csv" = {data[[x]]$Type <- "Local"},
            'noncampus.csv' = {data[[x]]$Type <- "Non-Campus"},
            'public_property.csv' = {data[[x]]$Type <- "Public Property"},
            '^(?!total_fires).*_student_housing_facilities.csv' = {data[[x]]$Type <- "Student Housing"},
            'on_campus.csv'= {data[[x]]$Type <- "On-Campus"}
         )
}

arrests <- bind_rows(data[grep('Arrests',fileNames)]) %>%
  rename(`Weapon Arrests` = Illegal.weapons.possession, 
         `Drug Arrests` = Drug.law.violations,
         `Liquor Arrests` = Liquor.law.violations)


criminalOffenses <- bind_rows(data[grep('Criminal',fileNames)])  %>%
  rename_at(vars(7:19), list(~str_replace(.,'^','(Criminal) ')))


discActions <- bind_rows(data[grep('Actions',fileNames)])  %>% 
  rename(`Weapon Disciplinary Actions` = Illegal.weapons.possession, 
         `Drug Disciplinary Actions` = Drug.law.violations,
         `Liquor Disciplinary Actions` = Liquor.law.violations)


hateCrimes <- bind_rows(data[grep('Hate',fileNames)])  %>% 
         select(-matches('Race|Religion|orientation|Gender|Disability|Ethnicity|National')) %>%
         rename_at(vars(7:23), list(~str_replace(.,'^','(Hate) ')))


vawaOffenses <- bind_rows(data[grep('VAWA',fileNames)])  %>%
         rename_at(vars(7:9), list(~str_replace(.,'^','(VAWA) ')))


invalidReports <- data[[grep('Unfounded',fileNames)]] %>% 
         dplyr::filter(Campus.ID == 1) %>% 
         select(-Unitid, -Campus.ID, -Campus.Name)

###################################################################################################
validReports <- arrests %>% full_join(discActions) %>%  full_join(criminalOffenses) %>% 
                            full_join(vawaOffenses) %>% full_join(hateCrimes) %>% 
         dplyr::filter(Campus.ID == 1) %>%
         select(-Campus.ID) %>%
         mutate(`Total Criminal Offenses` = rowSums(.[grep('Criminal',colnames(.))],na.rm = T),
                 `Total VAWA Offenses` = rowSums(.[grep('VAWA',colnames(.))],na.rm = T),
                 `Total Hate Offenses` = rowSums(.[grep('Hate',colnames(.))],na.rm = T)) %>%
         mutate(`Total Crimes` = rowSums(.[grep('Total',colnames(.))],na.rm = T)) 

withLocations <- locationsData %>% 
         select(Unitid = UNITID, City = CITY, State = STABBR, ZIP) %>%
         right_join(validReports, by = 'Unitid') %>% 
         mutate_at(vars('City','State','ZIP'), as.character) %>%
         mutate(ZIP = substr(ZIP,1,5)) %>%
         dplyr::filter(!(State %in% c('GU','PR'))) %>%
         select(Survey.year,Unitid, Institution.name, City, State, Type, everything()) %>%
         select(-Campus.Name)

write.csv(withLocations, 'University Crime.csv', row.names = F)

