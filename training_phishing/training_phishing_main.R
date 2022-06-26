packages <- c("tidyverse","readxl","openxlsx","janitor","svDialogs")
package_check <- lapply(packages, require, character.only = TRUE)


season <- dlgInput("Enter season name for phishing data summary page.")$res



########################################
## Phishing
########################################

filepath1 <- file.choose()
phishing_df <-read_excel(filepath1)


df1 <- phishing_df
df1 <- df1 %>%
  clean_names()

## Create summary
df1s <- df1 %>%
  group_by(bureau_name) %>%
  summarise(click_false = sum(primary_clicked == FALSE)
            , click_true = sum(primary_clicked == TRUE)
            , row_total = n()
            ) %>%
  mutate(click_rate = click_true/row_total) %>%
  mutate(impact = click_true/sum(row_total)) %>%
  arrange(bureau_name)

f <- c(unique(df1s$bureau_name))


df1ss <- df1s %>%
  mutate(bureau_name = 'Grand Total') %>%
  group_by(bureau_name) %>%
  summarise(click_false = sum(click_false)
            , click_true = sum(click_true)
            , row_total = sum(row_total)) %>%
  mutate(click_rate = click_true/click_false) %>%
  mutate(impact = click_true/row_total)

f <- c(levels(df1s$bureau_name), "Grand Total")

df1sss <- bind_rows(df1s,df1ss)
df1sss$bureau_name <- factor(df1sss$bureau_name, levels = f)
df1s$bureau_name <- factor(df1s$bureau_name, levels = f)

levels(df1sss$bureau_name)

rm(df1s)

## Create detail looping through each 
df1dlist <- list()
for (b in unique(df1s$bureau_name)) {
  df1dlist[[b]] <- df1 %>%
    filter(bureau_name == b) %>%
    filter(primary_clicked == TRUE)
}

rm(df1d)
rm(blist)

########################################
## Training
########################################
filepath2 <- file.choose()
training_df <-read_excel(filepath2)

df2 <- training_df %>%
  clean_names()

df2s <- df2 %>%
  group_by(bureau) %>%
  summarise(completed = sum(user_progress_in_assignment == "Completed")
            , in_progress = sum(user_progress_in_assignment == "In Progress")
            , not_started = sum(user_progress_in_assignment == "Not Started")
            , row_total = n()
            ) %>%
  mutate(completion_rate = completed/row_total) %>%
  arrange(bureau)

tempate_path <- 'training_phishing/training_phishing_template.xlsx'
output_path <- 'training_phishing/training_phishing_logistics.xlsx'
.wb <- openxlsx::loadWorkbook(tempate_path)

title1 <-  "Summary of Q2 2022 Phishing Exercise Results logistics"
title2 <- "ITA Training Completion as of 06/26/2022 logistics"

openxlsx::writeData(
  wb = .wb,
  sheet = 1,
  x = title1,
  xy = c(1,1)
)

openxlsx::writeData(
  wb = .wb,
  sheet = 3,
  x = title2,
  xy = c(1,1)
)

openxlsx::saveWorkbook(
  .wb,
  output_path,
  overwrite = TRUE
)

rm(.wb)

