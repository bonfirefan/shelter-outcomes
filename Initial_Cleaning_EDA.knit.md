
<!-- rnb-text-begin -->

---
title: "Final Project R Notebook"
output: html_notebook
author: "Phil Prosapio, Shayan Khan, Jiyuan Zhou"
---

#Introduction to Data Set
For our final project, our group decided to tackle the Shelter Animal Outcomes Kaggle competition (found here: https://www.kaggle.com/c/shelter-animal-outcomes/data). This data comes from the Austin Animal Center from October 1st, 2013 to March, 2016. To supplement this Kaggle dataset, we went directly to the Austin Animal Center website and downloaded a related dataset that contains intake information about the pets and merged this additional info into our overall dataset (https://data.austintexas.gov/Health-and-Community-Services/Austin-Animal-Center-Intakes/wter-evkm).Fun fact is that one of our group members actually worked at this animal center back in Austin!

This dataset contains information about cats and dogs that were sheltered at the Austin Animal Center. It contains general info about the animals like: type of animal, breed, color, age on outcome, etc.; where the outcome of interest in this competition represent the status of animals as they leave the Animal Center. We needed to merge our intake and outcome datasets and some of the data columns required cleanup or transformation to be of more use to us (e.g. splitting the Sex column into the animals Sex as well as the status of their sexual organs - neutered/intact/etc.), while others were of no value when it comes to trying to apply our models. All animals receive a unique Animal ID during intake.

Our goal is to predict the outcome of animals as they leave the animal center. The outcomes can be one of the following: Adoption, Died, Euthanasia, Return to owner, and Transfer.

The train and test data were randomly split for us already.  

It is our belief that producing an accurate and detailed model to help predict outcomes for these shelter animals could help the Austin Animal Center and the animals themselves in a couple ways:
- Help the animal center better predict the outcome of a new animal that comes into the shelter. And depending on that predicted outcome, take proactive measures to steer "assist" the animal in receiving a better outcome based on what our model tells us makes for a pet that is more likely to be adopted.
- Help the animal center better predict allocation of resources based on what actions might need to be taken for each pet in order to maximize the number of positive outcomes for the shelter animals.

## Project setup

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI0xvYWQgTGlicmFyaWVzXG5saWJyYXJ5KHRpZHl2ZXJzZSlcbmxpYnJhcnkoZ2dwbG90MilcbmxpYnJhcnkoRGF0YUV4cGxvcmVyKVxubGlicmFyeShjYXJldClcbmxpYnJhcnkoZ3JpZEV4dHJhKVxubGlicmFyeShyZXNoYXBlKVxubGlicmFyeShwdXJycilcbmxpYnJhcnkobHVicmlkYXRlKVxuYGBgIn0= -->

```r
#Load Libraries
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(caret)
library(gridExtra)
library(reshape)
library(purrr)
library(lubridate)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# #Helper Functions for Data Cleanup and EDA

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI0hlbHBlciBmdW5jdGlvbiB0byBjb252ZXJ0IHRoZSB2YWx1ZXMgaW4gdGhlIEFnZXVwb25PdXRjb21lIGNvbHVtbiBmcm9tIHRpbWUgc3RyaW5ncyAoZS5nLiAxIHllYXIsIDIgbW9udGhzLCBldGMuKSBpbnRvIHRpbWUgaW4gZGF5c1xudGltZVdhcnAgPC0gZnVuY3Rpb24oYWdlKSB7XG4gIGFnZV9lbGVtZW50cyA8LSBzdHJfc3BsaXRfZml4ZWQoYWdlLFwiXFxcXHNcIiwyKVxuICBub3JtX2FnZSA9IGFzLm51bWVyaWMoYWdlX2VsZW1lbnRzWzFdKVxuICBub3JtX2FnZVxuICB0aW1lX3VuaXQgPSBhZ2VfZWxlbWVudHNbMl1cbiAgI0Jhc2VkIG9uIHVuaXQgb2YgdGltZSwgYXBwcm9wcmlhdGVseSBzY2FsZSB1cCBhZ2UgdmFyaWFibGVcbiAgaWYgKGdyZXBsKHRpbWVfdW5pdCwgXCIod2VlaylzP1wiKSkge1xuICAgIG5vcm1fYWdlID0gbm9ybV9hZ2UqN1xuICB9IGVsc2UgaWYgKGdyZXBsKHRpbWVfdW5pdCwgXCIobW9udGhzKXM/XCIpKSB7XG4gICAgbm9ybV9hZ2UgPSBub3JtX2FnZSozMFxuICB9IGVsc2UgaWYgKGdyZXBsKHRpbWVfdW5pdCwgXCIoeWVhcilzP1wiKSkge1xuICAgIG5vcm1fYWdlID0gbm9ybV9hZ2UgKiAzNjVcbiAgfVxuICByZXR1cm4obm9ybV9hZ2UpXG59XG5gYGAifQ== -->

```r
#Helper function to convert the values in the AgeuponOutcome column from time strings (e.g. 1 year, 2 months, etc.) into time in days
timeWarp <- function(age) {
  age_elements <- str_split_fixed(age,"\\s",2)
  norm_age = as.numeric(age_elements[1])
  norm_age
  time_unit = age_elements[2]
  #Based on unit of time, appropriately scale up age variable
  if (grepl(time_unit, "(week)s?")) {
    norm_age = norm_age*7
  } else if (grepl(time_unit, "(months)s?")) {
    norm_age = norm_age*30
  } else if (grepl(time_unit, "(year)s?")) {
    norm_age = norm_age * 365
  }
  return(norm_age)
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Exploratory Data Analysis of Data
Read in the data.

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI0xvYWQgaW4gZGF0YSBhbmQgc3RhcnQgdHJhbnNmb3JtaW5nIGl0XG5hbmltYWxfb3V0IDwtIHJlYWRfY3N2KGd6ZmlsZShcImRhdGEvdHJhaW4uY3N2Lmd6XCIpKSAlPiUgXG4gICAgICAgICAgICAgIHNlbGVjdCgtRGF0ZVRpbWUsIC1PdXRjb21lU3VidHlwZSkgJT4lIFxuICAgICAgICAgICAgICB0cmFuc2Zvcm0oU2V4dXBvbk91dGNvbWUgPSBjb2xzcGxpdChTZXh1cG9uT3V0Y29tZSwgc3BsaXQgPSBcIlxcXFxzK1wiLCBuYW1lcyA9IGMoJ1NfT3JnYW5fU3RhdHVzX091dCcsICdTZXhfZHJvcCcpKSlcblxuYW5pbWFsX2luIDwtIHJlYWRfY3N2KFwiZGF0YS9BdXN0aW5fQW5pbWFsX0NlbnRlcl9JbnRha2VzLmNzdlwiKSAlPiUgXG4gICAgICAgICAgICAgc2VsZWN0KC1OYW1lLCAtQnJlZWQsIC1Db2xvciwgLWBBbmltYWwgVHlwZWApICU+JSBcbiAgICAgICAgICAgICB0cmFuc2Zvcm0oYFNleCB1cG9uIEludGFrZWAgPSBjb2xzcGxpdChgU2V4IHVwb24gSW50YWtlYCwgc3BsaXQgPSBcIlxcXFxzK1wiLCBuYW1lcyA9IGMoJ1NfT3JnYW5fU3RhdHVzX0luJywgJ1NleCcpKSlcblxuYW5pbWFsX291dCRBZ2V1cG9uT3V0Y29tZSA8LSB2YXBwbHkoYW5pbWFsX291dCRBZ2V1cG9uT3V0Y29tZSwgRlVOID0gdGltZVdhcnAsIEZVTi5WQUxVRSA9IGRvdWJsZSgxKSkgXG5hbmltYWxfaW4kQWdldXBvbkludGFrZSA8LSB2YXBwbHkoYW5pbWFsX2luJEFnZS51cG9uLkludGFrZSwgRlVOID0gdGltZVdhcnAsIEZVTi5WQUxVRSA9IGRvdWJsZSgxKSkgXG5gYGAifQ== -->

```r
#Load in data and start transforming it
animal_out <- read_csv(gzfile("data/train.csv.gz")) %>% 
              select(-DateTime, -OutcomeSubtype) %>% 
              transform(SexuponOutcome = colsplit(SexuponOutcome, split = "\\s+", names = c('S_Organ_Status_Out', 'Sex_drop')))

animal_in <- read_csv("data/Austin_Animal_Center_Intakes.csv") %>% 
             select(-Name, -Breed, -Color, -`Animal Type`) %>% 
             transform(`Sex upon Intake` = colsplit(`Sex upon Intake`, split = "\\s+", names = c('S_Organ_Status_In', 'Sex')))

animal_out$AgeuponOutcome <- vapply(animal_out$AgeuponOutcome, FUN = timeWarp, FUN.VALUE = double(1)) 
animal_in$AgeuponIntake <- vapply(animal_in$Age.upon.Intake, FUN = timeWarp, FUN.VALUE = double(1)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

##Clean the data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI0NvbnZlcnQgTmFtZSBjb2x1bW4gaW50byBmYWN0b3Igc28gd2UgY2FuIHNlZSBpZiB0aGUgZmFjdCB0aGF0IHRoZSBwZXQgaGFkIGEgbmFtZSBkb2N1bWVudGVkIGFmZmVjdGVkIGl0J3Mgb3V0Y29tZS4gMT1IYWQgTmFtZSwgMD1OYW1lIFVua25vd25cbmFuaW1hbF9vdXQkU19Pcmdhbl9TdGF0dXNfT3V0IDwtIGFuaW1hbF9vdXQkU2V4dXBvbk91dGNvbWUkU19Pcmdhbl9TdGF0dXNfT3V0XG5hbmltYWxfb3V0JFNleF9kcm9wIDwtIGFuaW1hbF9vdXQkU2V4dXBvbk91dGNvbWUkU2V4X2Ryb3BcblxuYW5pbWFsX2luJFNfT3JnYW5fU3RhdHVzX0luIDwtIGFuaW1hbF9pbiRTZXgudXBvbi5JbnRha2UkU19Pcmdhbl9TdGF0dXNfSW5cbmFuaW1hbF9pbiRTZXggPC0gYW5pbWFsX2luJFNleC51cG9uLkludGFrZSRTZXhcblxuYW5pbWFsX291dCROYW1lWyFpcy5uYShhbmltYWxfb3V0JE5hbWUpXSA8LSAxXG5hbmltYWxfb3V0JE5hbWVbaXMubmEoYW5pbWFsX291dCROYW1lKV0gPC0gMFxuYW5pbWFsX291dCROYW1lIDwtIGFzLmZhY3RvcihhbmltYWxfb3V0JE5hbWUpXG5cbiNGaW5hbCBzdGVwcyBvZiBjbGVhbmluZyB1cCBteSBkYXRhXG5jYWQgPC0gYW5pbWFsX291dCAlPiUgI2NhZCBzdGFuZGluZyBmb3IgQ2xlYW5lZF9BbmltYWxfRGF0YVxuICAgICAgIHNlbGVjdCgtU2V4dXBvbk91dGNvbWUsIC1TZXhfZHJvcCkgJT4lIFxuICAgICAgIG5hLm9taXQoKVxuXG5hbmltYWxfaW4gPC0gYW5pbWFsX2luICU+JSBcbiAgICAgICAgICAgICBzZWxlY3QoLVNleC51cG9uLkludGFrZSwgLUFnZS51cG9uLkludGFrZSlcblxuYW5pbWFsX2NvbWJpbmVkIDwtIGlubmVyX2pvaW4oY2FkLCBhbmltYWxfaW4sIGJ5ID0gYyhcIkFuaW1hbElEXCIgPSBcIkFuaW1hbC5JRFwiKSwgYWxsLng9VFJVRSkgJT4lIFxuICAgICAgICAgICAgICAgICAgIGZpbHRlcihBZ2V1cG9uSW50YWtlIDw9IEFnZXVwb25PdXRjb21lKVxuXG5jYWQgPC0gYW5pbWFsX2NvbWJpbmVkICU+JVxuICAgICAgIHNlbGVjdCgtQW5pbWFsSUQpICU+JSBcbiAgICAgICBkdW1taWZ5KG1heGNhdCA9IDVMKVxuYGBgIn0= -->

```r
#Convert Name column into factor so we can see if the fact that the pet had a name documented affected it's outcome. 1=Had Name, 0=Name Unknown
animal_out$S_Organ_Status_Out <- animal_out$SexuponOutcome$S_Organ_Status_Out
animal_out$Sex_drop <- animal_out$SexuponOutcome$Sex_drop

animal_in$S_Organ_Status_In <- animal_in$Sex.upon.Intake$S_Organ_Status_In
animal_in$Sex <- animal_in$Sex.upon.Intake$Sex

animal_out$Name[!is.na(animal_out$Name)] <- 1
animal_out$Name[is.na(animal_out$Name)] <- 0
animal_out$Name <- as.factor(animal_out$Name)

#Final steps of cleaning up my data
cad <- animal_out %>% #cad standing for Cleaned_Animal_Data
       select(-SexuponOutcome, -Sex_drop) %>% 
       na.omit()

animal_in <- animal_in %>% 
             select(-Sex.upon.Intake, -Age.upon.Intake)

animal_combined <- inner_join(cad, animal_in, by = c("AnimalID" = "Animal.ID"), all.x=TRUE) %>% 
                   filter(AgeuponIntake <= AgeuponOutcome)

cad <- animal_combined %>%
       select(-AnimalID) %>% 
       dummify(maxcat = 5L)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


##Handle Dummified Columns

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FkIDwtIGNhZCAlPiUgXG4gICAgICAgc2VsZWN0KC1OYW1lXzAsIC1BbmltYWxUeXBlX0NhdCwgLUZvdW5kLkxvY2F0aW9uKSAlPiUgXG4gICAgICAgZHBseXI6OnJlbmFtZShJc19Eb2cgPSBBbmltYWxUeXBlX0RvZykgJT4lIFxuICAgICAgIGRwbHlyOjpyZW5hbWUoSGFzX05hbWUgPSBOYW1lXzEpXG5gYGAifQ== -->

```r
cad <- cad %>% 
       select(-Name_0, -AnimalType_Cat, -Found.Location) %>% 
       dplyr::rename(Is_Dog = AnimalType_Dog) %>% 
       dplyr::rename(Has_Name = Name_1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuVmlldyhjYWQpXG5pbnRyb2R1Y2UoY2FkKVxuYGBgIn0= -->

```r
View(cad)
introduce(cad)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

There are 1380 unique breeds in the dataset. This is far too many

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGVuZ3RoKHVuaXF1ZShjYWQkQnJlZWQpKVxuYGBgIn0= -->

```r
length(unique(cad$Breed))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

## Feature Engineering
First we need to clean the dataset to move "mix" into its own feature and keep only the first breed mentioned for the breed mixes

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FkIDwtIGNhZCAlPiVcbiAgbXV0YXRlKG1peCA9IGlmZWxzZShncmVwbChcIm1peFwiLCBCcmVlZCwgaWdub3JlLmNhc2UgPSBUKSwgMSwgMCksXG4gICAgICAgICBicmVlZF9uZXcgPSBnc3ViKFwiIE1peFwiLCBcIlwiLCBCcmVlZCksXG4gICAgICAgICBicmVlZF9uZXcgPSBzdHJzcGxpdCh4ID0gYnJlZWRfbmV3LCBzcGxpdCA9IFwiL1wiKSxcbiAgICAgICAgIGJyZWVkX25ldyA9IG1hcF9jaHIoYnJlZWRfbmV3LCBmdW5jdGlvbih4KXt1bmxpc3QoeFsxXSl9KSlcbmBgYCJ9 -->

```r
cad <- cad %>%
  mutate(mix = ifelse(grepl("mix", Breed, ignore.case = T), 1, 0),
         breed_new = gsub(" Mix", "", Breed),
         breed_new = strsplit(x = breed_new, split = "/"),
         breed_new = map_chr(breed_new, function(x){unlist(x[1])}))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


It does look like there are a number of popular breeds that take up more than 75% of the dataset:

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FkICU+JVxuICBjb3VudChicmVlZF9uZXcsIHNvcnQgPSBUUlVFKSAlPiVcbiAgbXV0YXRlKGZyZXEgPSByb3VuZCgxMDAgKiBuIC8gc3VtKG4pLDIpKSAlPiVcbiAgaGVhZCgxNilcbmBgYCJ9 -->

```r
cad %>%
  count(breed_new, sort = TRUE) %>%
  mutate(freq = round(100 * n / sum(n),2)) %>%
  head(16)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY291bnQoY2FkLCBicmVlZF9uZXcpICU+JVxuICAgIGFycmFuZ2UoZGVzYyhuKSkgJT4lXG4gICAgZmlsdGVyKG4gPiAyMjIpIC0+IGNvbW1vblxuXG5jYWQgPC0gY2FkICU+JVxuICBtdXRhdGUoYnJlZWRfY29tbW9uID0gaWZlbHNlKGJyZWVkX25ldyAlaW4lIGNvbW1vbiRicmVlZF9uZXcsIDEsIDApKVxuYGBgIn0= -->

```r
count(cad, breed_new) %>%
    arrange(desc(n)) %>%
    filter(n > 222) -> common

cad <- cad %>%
  mutate(breed_common = ifelse(breed_new %in% common$breed_new, 1, 0))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

We've now grouped the 1365 sparse class breeds into one "Uncommon" group.



<!-- rnb-text-end -->

