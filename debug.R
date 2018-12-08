timeWarp <- function(age) {
  age_elements <- str_split_fixed(age,"\\s",2)
  norm_age = as.numeric(age_elements[1])
  norm_age
  time_unit = age_elements[2]
  #Based on unit of time, appropriately scale up age variable
  if (grepl(time_unit, "week") | grepl(time_unit, "weeks")) {
    norm_age = norm_age*7
  } else if (grepl(time_unit, "month") | grepl(time_unit, "months")) {
    norm_age = norm_age*30
  } else if (grepl(time_unit, "year") | grepl(time_unit, "years")) {
    norm_age = norm_age * 365
  }
  return(norm_age)
}

animal_out <- read_csv(gzfile("data/train.csv.gz")) %>% 
  select(-DateTime, -OutcomeSubtype) %>% 
  transform(SexuponOutcome = colsplit(SexuponOutcome, split = "\\s+", names = c('S_Organ_Status_Out', 'Sex_drop')))


animal_out$AgeuponOutcome <- vapply(animal_out$AgeuponOutcome, FUN = timeWarp, FUN.VALUE = double(1))