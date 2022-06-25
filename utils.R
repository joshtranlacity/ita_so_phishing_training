packages <- c("tidyverse", "readxl", "janitor","ggplot2","tinytex","writexl","tidyr")
package.check <- lapply(packages, require, character.only = TRUE)


utils.filter_scorecard_raw_data_by_department <- function(df, your_deptartment) {
  df <- df %>%
    filter(Department == your_deptartment)
}


utils.process_scorecard_raw_data <- function(df) {
  df <- df %>%
    clean_names() %>%
    arrange(date_sent)
  
  f <- unique(df$season)
  df$season <- factor(df$season, levels = f)
  
  df <- df %>%
    group_by(department,season) %>%
    mutate(dept_total_count = n()) %>%
    mutate(dept_true_count = sum(primary_clicked == TRUE)) %>%
    mutate(dept_click_rate = sum(primary_clicked == TRUE)/n()) %>%
    mutate(row_number = rank(dept_click_rate, ties.method="first")) %>%
    filter(row_number==1) %>%
    ungroup() %>%
    select(season, department, dept_true_count, dept_click_rate)
}

utils.process_scorecard_assign_your_department <- function(df, your_deptartment) {
  df <- df %>%
    mutate(series = ifelse(department == your_deptartment, 'Your Department','Other Departments'))
  df$series <- factor(df$series, levels = c('Other Departments','Your Department'))
  return(df)
}


utils.process_scorecard_season_current <- function(df) {
  season_curret <- tail(levels(df$season), n=1)
  df <- df %>%
    filter(season == season_curret)
  return(df)
}

utils.process_scorecard_season_trend <- function(df) {
  df1 <- df %>%
    filter(series == 'Your Department') %>%
    group_by(series, season) %>%
    summarise(mean_rate = mean(dept_click_rate))
    
  df2 <- df %>%
    group_by(season) %>%
    summarise(mean_rate = mean(dept_click_rate)) %>%
    mutate(series = 'Department Average')
  
  
  return(bind_rows(df1,df2))
}


