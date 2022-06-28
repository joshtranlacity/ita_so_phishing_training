
## Load packages needed for .r and rmd file
source("scorecard/utils.R")

packages <- utils.packages_vector()
package_check <- lapply(packages, require, character.only = TRUE)


## Get raw data file path
filepath <- file.choose()

## Read raw data file to get list of departments
raw_data_df <- read_excel(filepath, sheet = "Raw Data")


df <- utils.process_scorecard_raw_data(raw_data_df) %>% arrange(department)
season_current <- tail(levels(df$season), n=1)
departments <- unique(df$department) %>% sort()

pvt <- pivot_wider(df
  , names_from = "season"
  , values_from = c("dept_click_rate", "dept_true_count"))
smry <- df %>%
  group_by(season) %>%
  summarise(mean_dept_click_rate = mean(dept_click_rate),
            mean_dept_true_count = mean(dept_true_count))

datasheet_names <- list("long" = df, "wide" = pvt, "summary" = smry)

write_xlsx(datasheet_names
  , paste0(getwd(), "/scorecard/scorecard_reports/"
    , season_current
    , " Phising Exercise Scorecard Processed Data.xlsx"
  )
)

for (department in departments) {
  output_file <- paste(getwd(), "/scorecard/scorecard_reports/"
    , department
    , " - "
    , season_current
    , " Phising Scorecard.pdf", sep = ""
  )

  output_file_subbed <- gsub(" ", "_", output_file)

  df1 <- utils.process_scorecard_assign_your_department(df, department)

  filtered_raw_data_df <- raw_data_df %>%
    filter(Department==department)

  write_xlsx(filtered_raw_data_df
    , paste0(getwd(), "/scorecard/scorecard_reports/"
      , department
      , " - "
      , season_current
      , " Phishing Scorecard Detail.xlsx"
    )
  )

  df2 <- utils.process_scorecard_season_current(df1)
  df3 <- utils.process_scorecard_season_trend(df1)

  rmarkdown::render(paste0(getwd(), "/scorecard/scorecard_template.Rmd"),
                    output_file = output_file_subbed,
                    params = list(
                      processed_raw_data_df = df1
                      , current_df = df2
                      , trend_df = df3
                      , your_department = department
                      , raw_data_filepath = filepath
                      )
                    )
  file.rename(output_file_subbed, output_file)
}