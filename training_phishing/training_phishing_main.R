## Load Packages
packages <- c("tidyverse","readxl","openxlsx","janitor","svDialogs")
package_check <- lapply(packages, require, character.only = TRUE)

help("svDialogs-package")

########################################
## Phishing
########################################

## Load in phishing by bureau data
dlg_message("Please select raw 'Phishing by Bureau.xlsx' file in next window.", type=("ok"))
phishing_filepath <- file.choose()
phishing_df <-read_excel(phishing_filepath)

df <- phishing_df
df <- df %>% clean_names()


## Create summary
dfs <- df %>%
  group_by(bureau_name) %>%
  summarise(click_false = sum(primary_clicked == FALSE)
            , click_true = sum(primary_clicked == TRUE)
            , row_total = n()
  ) %>%
  mutate(click_rate = click_true/row_total) %>%
  mutate(impact = click_true/sum(row_total)) %>%
  arrange(bureau_name)

bureaus <- c(unique(dfs$bureau_name))
dfs$bureau_name <- factor(dfs$bureau_name, levels = bureaus)

## Get grand total row
dfss <- dfs %>%
  mutate(bureau_name = 'Grand Total') %>%
  group_by(bureau_name) %>%
  summarise(click_false = sum(click_false)
            , click_true = sum(click_true)
            , row_total = sum(row_total)) %>%
  mutate(click_rate = click_true/click_false) %>%
  mutate(impact = click_true/row_total)


## Bind summary with grand total row and set order
## Output for Sheet[1] - "Phishing - Summary"
dfsss <- bind_rows(dfs,dfss)
dfsss$bureau_name <- factor(dfsss$bureau_name, levels = c(bureaus, "Grand Total"))
phishing_summary_df <- dfsss

## Create list of filtered detail dataframes for each bureau.
## Output for Sheet[2] - "Phishing - Users Who Clicked"
phishing_detail_df_list_by_bureau <- list()
for (b in bureaus) {
  phishing_detail_df_list_by_bureau[[b]] <- df %>%
    filter(bureau_name == b) %>%
    filter(primary_clicked == TRUE)
}

## Clean up environment
tryCatch({
  rm("df")
  rm("dfs")
  rm("dfss")
  rm("dfsss")
})


########################################
## Training
########################################

## Load in training by bureau data
dlg_message("Please select raw 'Training by Bureau.xlsx' file in next window.", type=("ok"))
training_filepath <- file.choose()
training_df <-read_excel(training_filepath)

df <- training_df %>% clean_names()

## Create summary
dfs <- df %>%
  group_by(bureau) %>%
  summarise(completed = sum(user_progress_in_assignment == "Completed")
            , in_progress = sum(user_progress_in_assignment == "In Progress")
            , not_started = sum(user_progress_in_assignment == "Not Started")
            , row_total = n()
  ) %>%
  mutate(completion_rate = completed/row_total) %>%
  arrange(bureau)

bureaus <- c(unique(dfs$bureau))
dfs$bureau <- factor(dfs$bureau, levels = bureaus)


## Get grand total row
dfss <- dfs %>%
  mutate(bureau = 'Grand Total') %>%
  group_by(bureau) %>%
  summarise(completed = sum(completed)
            , in_progress = sum(in_progress)
            , not_started = sum(not_started)
            , row_total = sum(row_total)) %>%
  mutate(completion_rate= completed/row_total)


## Bind summary with grand total row and set order
## Output for Sheet[3] - "Training - Summary"
dfsss <- bind_rows(dfs,dfss)
dfsss$bureau <- factor(dfsss$bureau, levels = c(bureaus, "Grand Total"))
training_summary_df <- dfsss


## Create list of filtered detail dataframes for each bureau.
## Output for Sheet[4] - "Training - Not Completed"
training_detail_df_list_by_bureau <- list()
for (b in bureaus) {
  training_detail_df_list_by_bureau[[b]] <- df %>%
    filter(bureau == b) %>%
    filter(user_progress_in_assignment != "Completed")
}


## Clean up environment
tryCatch({
  rm("df")
  rm("dfs")
  rm("dfss")
  rm("dfsss")
})


########################################
## To Excel
########################################

tempate_filepath <- paste0(getwd(), "/training_phishing/training_phishing_template.xlsx")

## Get season name not in data and needs to be provided by user
season <- dlgInput("Please enter [season] name for phishing data summary page.")$res
phishing_summary_title <-  paste0("Summary of ", season, " Phishing Exercise Results")

## Get as of date in case data was pulled on different day than date of report generation
as_of_date <- dlgInput("Please enter as of [date] for training data summary page.")$res
training_summary_title <- paste0("ITA Training Completion as of ", as_of_date)


for (b in bureaus) {
  .wb <- openxlsx::loadWorkbook(tempate_filepath)
  ## Sheet[1] Phishing - Summary
  sheet_number = 1
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = phishing_summary_title,
    xy = c(1,1)
  )
  
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = phishing_summary_df,
    xy = c(1,5),
    colNames = FALSE,
    rowNames = FALSE 
  )
  
  ## Sheet[2] Phishing - Users Who Clicked
  sheet_number = 2
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = phishing_detail_df_list_by_bureau[[b]],
    xy = c(1,2),
    colNames = FALSE,
    rowNames = FALSE 
  )
  
  ## Sheet[3] Training - Summary
  sheet_number = 3
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = training_summary_title,
    xy = c(1,1)
  )
  
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = phishing_summary_df,
    xy = c(1,5),
    colNames = FALSE,
    rowNames = FALSE 
  )
  
  ## Sheet[4] training - Not Completed
  sheet_number = 4
  openxlsx::writeData(
    wb = .wb,
    sheet = sheet_number,
    x = training_detail_df_list_by_bureau[[b]],
    xy = c(1,2),
    colNames = FALSE,
    rowNames = FALSE 
  )
  
  
  ## Dynamically generate output filepath for each bureau. 
  ## Sanitize date if contains "/"
  output_filepath <- paste0(getwd()
                            , "/training_phishing/training_phishing_reports/"
                            , "ITA Cyber Training and Phishing "
                            , gsub("/", "-", as_of_date)
                            , "_"
                            , b
                            , ".xlsx")
  
  openxlsx::saveWorkbook(
    .wb,
    output_filepath,
    overwrite = TRUE
  )
  
}

tryCatch({
  rm(".wb")
})

dlg_message("Done.", type=("ok"))