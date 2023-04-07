
# setup -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, tidyverse, janitor)


# coerce to parquet -------------------------------------------------------

# set the directory containing the csv files
csv_dir <- "data/"

# get a list of all csv files in the directory
csv_files <- list.files(csv_dir, pattern = ".csv", full.names = TRUE)

# loop through each csv file and convert it to a parquet file
for (file in csv_files) {

  # load the csv data
  csv_data <- read_csv(file) |> janitor::clean_names()

  # convert the csv data to an arrow table
  arrow_table <- as_arrow_table(csv_data)

  # define the parquet filename by replacing csv with parquet format
  parquet_file <- str_replace(file, ".csv", ".parquet")

  # write the arrow table to a parquet file
  arrow::write_parquet(arrow_table, parquet_file)
  
  # remove the csv data and arrow table from memory
  rm(arrow_table, csv_data)

  # remove the csv file from data directory
  file.remove(file)
  
}
