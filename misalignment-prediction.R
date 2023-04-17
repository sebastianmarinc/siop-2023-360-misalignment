
# setup -------------------------------------------------------------------

# restart R session
.rs.restartR() 

# load libraries 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, janitor, tidyverse, tidymodels, duckdb)
con <- dbConnect(duckdb::duckdb())


# explore data ------------------------------------------------------------

# list all Parquet files in the data directory
data_dir <- "data/"
parquet_files <- list.files(data_dir, pattern = "\\.parquet$", full.names = TRUE)

# function to create a table name from a file path
get_table_name <- function(file_path) {
  return(tools::file_path_sans_ext(basename(file_path)) %>% gsub(" ", "_", .))
}

# register each parquet file as a separate virtual table
for (file in parquet_files) {
  table_name <- get_table_name(file)
  query <- glue::glue("CREATE OR REPLACE TABLE {table_name} AS SELECT * FROM parquet_scan('{file}')")
  dbExecute(con, query)
}

# query column names 
dbGetQuery(con, "PRAGMA table_info('self_report_360_data')")
dbGetQuery(con, "PRAGMA table_info('contributor_assessment_responses')")
dbGetQuery(con, "PRAGMA table_info('roles_and_IDs')")

dbGetQuery(con, 
"select x1_assessments_creator_id, x1_assessments_user_id, x1_assessments_assessment_id
from self_report_360_data
--where x1_assessments_user_id = 130090
where x1_assessments_assessment_id == 1563786
limit 10")

dbGetQuery(con, 
"select x1_assessments_creator_id, x1_assessments_user_id, x1_assessments_assessment_id
from contributor_assessment_responses
--where x1_assessments_assessment_id = 1532652
limit 10")

dbGetQuery(con,
"SELECT other.*
FROM contributor_assessment_responses AS other
INNER JOIN self_report_360_data AS self
ON other.x1_assessments_assessment_id = self.x1_assessments_assessment_id")


# ^ here i found that these don't have any values between them
# they are not the same -- i'm looking towards 'parent_assessment_id' and 
#   'response_assessment_id' next 


# wrangle data ------------------------------------------------------------

# data loading
df_ob <- read_parquet('data/onboarding data.parquet')
df_self <- read_parquet('data/self report 360 data.parquet')
df_other <- read_parquet('data/contributor assessment responses.parquet')
df_roles <- read_parquet('data/roles and IDs.parquet')

# drop from other ratings when creator ID == user ID -- they shouldn't be the same  
df_other <- df_other |> filter(x1_assessments_creator_id != x1_assessments_user_id) 

# connect contributor and assessment IDs 
df_other |> filter(x1_assessments_creator_id == 58201)
df_roles |> filter(contributor_id == 58201)

df_other |> filter(x1_assessments_assessment_id == 1735104)
df_roles |> filter(parent_assessment_id == 1735104)

df_self |> filter(x1_assessments_user_id == 130090)
df_other |> filter(x1)
df_roles |> count(role)

# manager misalignment ----------------------------------------------------



