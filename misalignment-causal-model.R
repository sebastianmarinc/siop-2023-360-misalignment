
# setup  ------------------------------------------------------------------

# restart R session
.rs.restartR() 

# load libraries 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, janitor, tidyverse, duckdb, collapse, 
               DoubleML, marginaleffects, did, ggridges, psych, 
               sandwich, lmtest, lmerTest, panelvar, easystats, modelsummary)

# # connect to data directory
# con <- dbConnect(duckdb::duckdb())


# wrangling ---------------------------------------------------------------
## wpm --------------------------------------------------------------------

# get ob and rp data; clean names
ob <- read_parquet('data/onboarding data.parquet')
ob <- ob |> rename_with(~str_remove(.x, "x2_construct_scores_base_or_post_"))

rp <- read_parquet('data/rp data.parquet')
rp <- rp |> rename_with(~str_remove(.x, "x2_construct_scores_base_or_post_"))

# get time vars
ob <- ob|> mutate(time = 1)
rp <- rp |> mutate(time = 2)

# get common member IDs across data sets 
common_member_ids <- intersect(ob$member_id, rp$member_id)

# filter ob data
ob_filtered <- 
  ob |> 
  # subset of IDs across ob and rp
  filter(member_id %in% common_member_ids) |> 
  # subset cols 
  select(member_id, construct_key, raw_score, time, submitted_at_date)

# filter rp data
rp_filtered <- 
  rp |> 
  # subset of IDs across ob and rp
  filter(member_id %in% common_member_ids) |> 
  # subset cols 
  select(member_id, construct_key, raw_score, time, submitted_at_date)

# only keep rp after ob per member id
dates <- ob_filtered |> 
  distinct(member_id, submitted_at_date) |>
  arrange(member_id, submitted_at_date) |> 
  group_by(member_id) |> 
  slice(1) |> 
  ungroup() |>
  rename(submitted_at_date_ob = submitted_at_date) |> 
  inner_join(
    rp_filtered |> 
      distinct(member_id, submitted_at_date) |> 
      rename(submitted_at_date_rp = submitted_at_date),
    by = "member_id"
  ) |> 
  mutate(ob_before_rp = submitted_at_date_ob < submitted_at_date_rp) |> 
  filter(ob_before_rp == T) |> 
  arrange(member_id, submitted_at_date_ob, submitted_at_date_rp) |> 
  group_by(member_id, submitted_at_date_ob) |> 
  slice(1) |> 
  ungroup() |> 
  select(-ob_before_rp)

# bind ob and rp; arrange
ob_rp <- bind_rows(ob_filtered, rp_filtered)
ob_rp <- ob_rp |> arrange(member_id, construct_key, time)

# drop unwanted dates
ob_rp <- ob_rp |> inner_join(dates, by = "member_id")

# filter the rows where submitted_at_date matches submitted_at_date_ob or submitted_at_date_rp
ob_rp_filtered <- ob_rp |> 
  filter(submitted_at_date == submitted_at_date_ob | 
           submitted_at_date == submitted_at_date_rp)

# drop wpm1 constructs 
ob_rp_filtered <- ob_rp_filtered |> 
  filter(!str_detect(construct_key, "wpm1")) #drop wpm1

# get 360 data 
ts <- read_parquet('data/self report 360 data.parquet')
unique_360_member_ids <- unique(ts$x1_assessments_user_id)

# create a binary variable in the combined data set based on the 360 member IDs
ob_rp_filtered <- ob_rp_filtered |> 
  mutate(took_360 = if_else(member_id %in% unique_360_member_ids, 1, 0))


## 360 --------------------------------------------------------------------

# self report 
glimpse(ts)
ts_filtered <- ts |> 
  select(x1_assessments_user_id, x1_assessments_assessment_id, x1_assessments_creator_id,
         x2_assessment_item_responses_item_key, x2_assessment_item_responses_item_response)
ts_filtered$role <- "self"

ts_filtered <- ts_filtered |> 
  rename_with(~str_remove(.x, "x2_assessment_item_responses_")) |> 
  rename_with(~str_remove(.x, "x1_assessments_"))

ts_filtered <- ts_filtered |> 
  filter(str_detect(item_key, "subdimension_score"))

ts_filtered <- ts_filtered |> 
  filter(creator_id == user_id)

# other report 
ts_other <- read_parquet('data/contributor assessment responses.parquet')

ts_other <- ts_other |> 
  rename_with(~str_remove(.x, "x2_assessment_item_responses_")) |> 
  rename_with(~str_remove(.x, "x1_assessments_")) 

ts_other_filtered <- ts_other |> 
  filter(str_detect(item_key, "subdimension_score"))

ts_other_filtered <- ts_other_filtered |> 
  filter(creator_id != user_id)

# get roles for other report 
roles <- read_parquet('data/roles and IDs.parquet')

roles <- roles |> 
  drop_na(response_assessment_id)

roles <- roles |> 
  unite(assessment_id, contributor_id, response_assessment_id, sep = '_')

roles_filtered <- roles |> 
  distinct(assessment_id, role, .keep_all = TRUE) |> 
  group_by(assessment_id) |> 
  arrange(desc(response_submitted_at)) |> 
  slice(1) |> 
  ungroup() |> 
  select(assessment_id, role)

# join with ts_other_filtered
ts_other_filtered <- ts_other_filtered |> 
  unite(assessment_id, creator_id, assessment_id, sep = "_")

ts_other_filtered <- roles_filtered |> 
  right_join(ts_other_filtered) |>  
  select(user_id, item_key, item_response, role, 
         organizations_through_tracks_organization_id, 
         sfdc_accounts_zoominfo_primary_industry,
         product_access_at_time_of_assessment_product_group_of_psa)


# analysis ----------------------------------------------------------------

## inspire ----------------------------------------------------------------
# get inspire data
inspire_data <- ob_rp_filtered |> 
  filter(construct_key == "inspire") |> 
  select(-contains('date')) |> 
  pivot_wider(names_from = "time", 
              values_from = "raw_score",
              names_prefix = "t",
              values_fn = mean) |> 
  drop_na()

# get inspire outcome
inspire_data$outcome <- inspire_data$t2 - inspire_data$t1

# get counfounders
inspire_data <- inspire_data |> 
  left_join(
    ob |> distinct(
      member_id, members_level, members_language,
      members_job_function, organization_id
    ),
    by = "member_id"
  ) |>
  mutate(members_job_function = replace_na(members_job_function, "general"),
         members_level = replace_na(members_level, "individual contributor")) |> 
  fastDummies::dummy_cols(select_columns = c(
    "members_level", "members_language",
    "members_job_function", "organization_id"
  ), 
  remove_selected_columns = T) |> 
  janitor::clean_names() 

confounders <- inspire_data |> 
  select(t1, members_level_frontline_manager:organization_id_975) |> 
  names()

confounders <- inspire_data |>
  select(members_level_frontline_manager:members_job_function_sales_business_development) |> 
  names()

# get doubleml data class
inspire_dt <- data.table::data.table(inspire_data)
dml_data <- DoubleMLData$new(inspire_dt, 
                             y_col = "t2", 
                             d_cols = "took_360", 
                             x_cols = confounders
)

# # dml task w tuning and cross-fitting
# library(paradox); library(mlr3tuning)
# lgr::get_logger("mlr3")$set_threshold("warn")
# lgr::get_logger("bbotk")$set_threshold("warn")
# 
# # set seed for reproducbility
# set.seed(1994)
# 
# # set learner algorithms
# ml_l = lrn("regr.glmnet")
# ml_m = lrn("classif.glmnet")
# #doubleml_plr = DoubleMLPLR$new(dml_dat_inspiring, ml_l, ml_m)
# 
# doubleml_irm <- DoubleMLIRM$new(
#   dml_data,
#   ml_g = ml_l,
#   ml_m = ml_m,
#   score = "ATTE",
#   dml_procedure = "dml2"
# )
# 
# par_grids = list(
#   "ml_g" = ParamSet$new(list(
#     ParamDbl$new("lambda", lower = 0.00001, upper = 1))),
#   "ml_m" = ParamSet$new(list(
#     ParamDbl$new("lambda", lower = 0.00001, upper = 1)))
# )
# 
# # params to try
# tune_settings = list(terminator = trm("evals", n_evals = 500),
#                      algorithm  = tnr("grid_search", resolution = 100),
#                      rsmp_tune  = rsmp("cv", folds = 5),
#                      measure    = list("ml_g" = msr("regr.rmse")
#                                        ,"ml_m" = msr("classif.acc")
#                      ))
# # tuning
# #doubleml_plr$tune(param_set = par_grids, tune_settings = tune_settings)
# doubleml_irm$tune(param_set = par_grids, tune_settings = tune_settings)
# 
# # tuned parameters
# #str(doubleml_plr$params)
# str(doubleml_irm$params)
# 
# # estimate model and summary
# # doubleml_plr$fit()
# # doubleml_plr$summary()
# doubleml_irm$fit()
# doubleml_irm$summary()


# dml task with no tuning and with cross-fitting 
n_vars <- length(inspire_dt)
n_obs <- nrow(inspire_dt)

# reg_ranger <- lrn("regr.ranger",
#                   num.trees=500,
#                   mtry=floor(sqrt(n_vars)),
#                   max.depth=5, min.node.size=2)
# 
# classif_ranger <- lrn("classif.ranger",
#                       num.trees=500,
#                       mtry=floor(sqrt(n_vars)),
#                       max.depth=5, min.node.size=2)

reg_xgb <- lrn('regr.xgboost' )#, max_depth = 10, max_leaves = 5,
# grow_policy = 'lossguide', tree_method = 'hist',
# booster = 'gbtree')
classif_xgb <- lrn('classif.xgboost') #, max_depth = 10, max_leaves = 5,
# grow_policy = 'lossguide', tree_method = 'hist',
# booster = 'gbtree')

reg_glmnet <- lrn("regr.cv_glmnet")
classif_glmnet <- lrn("classif.cv_glmnet")

# partially regression
dml_plr <- DoubleMLPLR$new(
  dml_data,
  ml_l = reg_glmnet,
  ml_m = classif_xgb,
  n_folds = 10,
  n_rep = 5
)
dml_plr$fit()
dml_plr

# # iv regression
# dml_irm <- DoubleMLIRM$new(
#   dml_data,
#   ml_g = reg_glmnet,
#   ml_m = classif_xgb,
#   score = "ATTE",
#   dml_procedure = "dml2"
# )
# dml_irm$fit()
# dml_irm


# misalignment ------------------------------------------------------------

# join with self scores
ts_other_trim <- ts_other_filtered |> 
  select(user_id, role, item_key, item_response) |> 
  rename_with(~paste0("other_", .x)) |> 
  rename(user_id = other_user_id) 

ts_other_trim <- ts_other_trim |> 
  mtt(other_item_response = as.numeric(other_item_response)) |> 
  slt(user_id, other_role, other_item_key, other_item_response) |> 
  gby(user_id, other_role, other_item_key) |> 
  smr(other_avg = fmean(other_item_response))

ts_trim <- ts_filtered |> 
  mtt(item_response = as.numeric(item_response)) |> 
  slt(user_id, item_key, item_response) |> 
  gby(user_id, item_key) |> 
  smr(avg = fmean(item_response))

ts_trim <- ts_trim |> 
  mutate(role = case_when(
    str_detect(item_key, "contributor") ~ "contributor", 
    str_detect(item_key, "direct_report") ~ "direct_report", 
    str_detect(item_key, "manager") ~ "manager", 
    str_detect(item_key, "other_coworker") ~ "peer", 
    T ~ "self"
  ))

ts_trim <- ts_trim |> 
  mutate(construct = sub(".*subdimension_score_", "", item_key)) |> 
  select(user_id, role, construct, avg)

ts_trim_wide <- ts_trim |>  
  pivot_wider(names_from='role', 
              values_from = 'avg', 
              values_fn = mean) 

ts_trim_mis <- ts_trim_wide |> 
  mutate(diff_manager = manager - self,
         diff_peer = peer - self, 
         diff_direct_report = direct_report - self) |> 
  mutate(mis_manager = if_else(diff_manager > -12 , 0, 1),
         mis_peer = if_else(diff_peer > -12 , 0, 1),
         mis_direct_report = if_else(diff_direct_report > -12 , 0, 1))


# create a binary variable in the combined data set based on the 360 member IDs
unique_360_member_ids <- unique(ts_trim_mis$user_id)

ob_rp_mis <- ob_rp_filtered |> 
  mutate(mis = if_else(member_id %in% unique_360_member_ids, 1, 0)) #|> sbt(mis==1)

# focus on constructs in the 360 
unique_constructs <- unique(ts_trim_mis$construct)

ob_rp_mis <- ob_rp_mis |> 
  mutate(subdims = if_else(construct_key %in% unique_constructs, 1, 0)) |> 
  sbt(subdims == 1)



## classical DiD  ---------------------------------------------------------

# finalize data set for diff-in-diff
ob_rp_mis <- ob_rp_mis |> 
  select(user_id = member_id, construct = construct_key, time, score = raw_score)

did_data <- 
  ts_trim_mis |> 
  select(user_id, construct, contains("mis")) |> 
  left_join(ob_rp_mis) |> 
  drop_na(time, score) |> 
  mutate(time = time-1) |> 
  distinct()

did_data |> glimpse()



# fit model
pacman::p_load(plm)


# figure out this warning 
#Warning message:
#  In pdata.frame(data, index) :
#  duplicate couples (id-time) in resulting pdata.frame
#to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")


did <- plm(
  score ~ time*mis_manager,
  data = did_data[did_data$construct == "alignment",],
  effect = 'time',
  model = "within"
)

lmtest::coeftest(did, vcov = function(x) {vcovHC(x, cluster = "group", type = "HC1")})




## mixed effects ----------------------------------------------------------

unique_constructs <- unique(did_data$construct)

did_data <- did_data |> 
  left_join(
    ob |> distinct(
      member_id, members_level, members_language,
      members_job_function, organization_id
    ) |> 
      group_by(member_id) |> 
      slice(1) |> 
      ungroup(),
    by = c("user_id" = "member_id")
  ) |> 
  left_join(
    ts |> distinct(
      x1_assessments_user_id, 
      sfdc_accounts_zoominfo_primary_industry,
      product_access_at_time_of_assessment_product_group_of_psa
    ) |> 
      group_by(x1_assessments_user_id) |> 
      slice(1) |> 
      ungroup(),
    by = c("user_id" = "x1_assessments_user_id")
  ) |> 
  rename(industry = sfdc_accounts_zoominfo_primary_industry,
         product = product_access_at_time_of_assessment_product_group_of_psa) |> 
  mutate(members_job_function = replace_na(members_job_function, "general"),
         industry = replace_na(industry, "general"), 
         product = replace_na(product, "general"),
         members_level = replace_na(members_level, "individual contributor"),
  ) |> 
  fastDummies::dummy_cols(select_columns = c(
    "members_level", "members_language",
    "members_job_function", "industry", "product"
  ), 
  remove_selected_columns = T) |> 
  janitor::clean_names() |> glimpse()



### manager ----
effect_ls <- list()

for(unique_construct in unique_constructs) {
  
  num_rows <- did_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(mis_manager) |> 
    drop_na() |> 
    nrow()
  
  if(num_rows == 0) {next}
  
  else{
    
    dat <- did_data |>
      filter(str_detect(construct, unique_construct)) |> 
      select(-construct, -mis_direct_report, -mis_peer)
    
    mod <- lmerTest::lmer(
      score ~ time*mis_manager + . + (1|user_id),
      data = dat #wpm_360_long[wpm_360_long$domain == 'inspiring',]
    )
    
    effect <- coef(summary(mod)) |> 
      as_tibble(rownames = "coef") |> 
      filter(str_detect(coef, "time:mis_manager")) |> 
      janitor::clean_names() |> 
      mutate(construct = unique_construct) |> 
      select(construct, estimate, std_err = std_error, p_val = pr_t) |> 
      mutate(sig = p_val < .05)
    
    effect_ls[[unique_construct]] <- effect
    
  }
}

bind_rows(effect_ls, .id = "construct") |> 
  write_csv('output/lmer_manager.csv')


### peer ----
effect_ls <- list()

for(unique_construct in unique_constructs) {
  
  num_rows <- did_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(mis_peer) |> 
    drop_na() |> 
    nrow()
  
  if(num_rows == 0) {next}
  
  else{
    
    dat <- did_data |>
      filter(str_detect(construct, unique_construct)) |> 
      select(-construct, -mis_direct_report, -mis_manager)
    
    mod <- lmerTest::lmer(
      score ~ time*mis_peer + . + (1|user_id),
      data = dat #wpm_360_long[wpm_360_long$domain == 'inspiring',]
    )
    
    effect <- coef(summary(mod)) |> 
      as_tibble(rownames = "coef") |> 
      filter(str_detect(coef, "time:mis_peer")) |> 
      janitor::clean_names() |> 
      mutate(construct = unique_construct) |> 
      select(construct, estimate, std_err = std_error, p_val = pr_t) |> 
      mutate(sig = p_val < .05)
    
    effect_ls[[unique_construct]] <- effect
    
  }
}

bind_rows(effect_ls, .id = "construct") |> 
  write_csv('output/lmer_peer.csv')


### dr ----
effect_ls <- list()

for(unique_construct in unique_constructs) {
  
  num_rows <- did_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(mis_direct_report) |> 
    drop_na() |> 
    nrow()
  
  if(num_rows == 0) {next}
  
  else{
    
    dat <- did_data |>
      filter(str_detect(construct, unique_construct)) |> 
      select(-construct, -mis_manager, -mis_peer)
    
    mod <- lmerTest::lmer(
      score ~ time*mis_direct_report + . + (1|user_id),
      data = dat #wpm_360_long[wpm_360_long$domain == 'inspiring',]
    )
    
    effect <- coef(summary(mod)) |> 
      as_tibble(rownames = "coef") |> 
      filter(str_detect(coef, "time:mis_direct_report")) |> 
      janitor::clean_names() |> 
      mutate(construct = unique_construct) |> 
      select(construct, estimate, std_err = std_error, p_val = pr_t) |> 
      mutate(sig = p_val < .05)
    
    effect_ls[[unique_construct]] <- effect
    
  }
}

bind_rows(effect_ls, .id = "construct") |> 
  write_csv('output/lmer_direct_report.csv')



## causal forest ----------------------------------------------------------

pacman::p_load(grf)

grf_data <- did_data |> 
  pivot_wider(names_from = "time", 
              values_from = "score", 
              names_prefix = "t",
              values_fn = mean)

grf_data <- grf_data |>   
  left_join(
    ob |> distinct(
      member_id, members_level, members_language,
      members_job_function, organization_id
    ) |> 
      group_by(member_id) |> 
      slice(1) |> 
      ungroup(),
    by = c("user_id" = "member_id")
  ) |> 
  left_join(
    ts |> distinct(
      x1_assessments_user_id, 
      sfdc_accounts_zoominfo_primary_industry,
      product_access_at_time_of_assessment_product_group_of_psa
    ) |> 
      group_by(x1_assessments_user_id) |> 
      slice(1) |> 
      ungroup(),
    by = c("user_id" = "x1_assessments_user_id")
  ) |> 
  rename(industry = sfdc_accounts_zoominfo_primary_industry,
         product = product_access_at_time_of_assessment_product_group_of_psa) |> 
  mutate(members_job_function = replace_na(members_job_function, "general"),
         industry = replace_na(industry, "general"), 
         product = replace_na(product, "general"),
         members_level = replace_na(members_level, "individual contributor"),
  ) |> 
  fastDummies::dummy_cols(select_columns = c(
    "members_level", "members_language",
    "members_job_function", "industry", "product"
  ), 
  remove_selected_columns = T) |> 
  janitor::clean_names() 

unique_constructs <- unique(grf_data$construct)


### run ATE and ATT for manager misalignment ------------------------------

ate_ls <- list()
att_ls <- list()
lm_cf_cov_ls <- list()

n_ls <- list()
n_cov_ls <- list()

for(unique_construct in unique_constructs) {
  
  # get data for manager misalignment
  dat <- grf_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(-mis_peer, -mis_direct_report, 
           #-contains("organization_"), 
           #-contains("members_language_")
    ) |> 
    drop_na()
  
  # continue the loop if no misaligned ratings
  if(nrow(dat) == 0) {next}
  
  else{
    
    # get vectors
    Y <- dat |> pull(t1) 
    W <- dat |> pull(mis_manager)
    organizations <- dat |> pull(organization_id)
    X <- dat |> 
      select(-t1, -mis_manager, -construct, 
             -user_id, -organization_id) |> 
      as.matrix()
    
    # get num of clusters
    n_ls[[unique_construct]] <- length(unique(organizations))
    
    # initial model 
    cf1 <- causal_forest(X, Y, W, clusters = organizations)
    
    # # calculate variable importance
    # var_imp <- variable_importance(cf1)
    # 
    # # get relevant covariates
    # covariates <- 
    #   tibble(var = colnames(X), 
    #          var_imp = var_imp[,1]) |> 
    #   filter(var_imp > 0) |> 
    #   pull(var)
    
    # get num of covariates
    n_cov_ls[[unique_construct]] <- length(covariates)
    
    # # final causal forest model
    # cf2 <- causal_forest(X[,colnames(X) %in% covariates], Y, W, clusters = organizations)
    
    # average treatment effects
    ate <- average_treatment_effect(cf1, target.sample = "all")
    ate <- as_tibble_row(ate)
    
    # average treatment effects of the treated
    att <- average_treatment_effect(cf1, target.sample = "treated")
    att <- as_tibble_row(att)
    
    # save output
    ate_ls[[unique_construct]] <- ate
    att_ls[[unique_construct]] <- att
    
    
    # fit linear model w/ selected covariates from causal forest
    dat_lm <- dat |> 
      select(mis_manager, t1, colnames(X))#[,colnames(X) %in% covariates]))
    
    mod_lm <- lm(t1 ~ ., data = dat_lm) #|> tidy() |> filter(term == "mis_manager")
    vcov_clustered <- vcovCL(mod_lm, cluster = organizations)
    mod_lm_clustered <- coeftest(mod_lm, vcov_clustered) 
    
    lm_est_cf_cov <- mod_lm_clustered |> 
      tidy() |> 
      filter(term == "mis_manager") |> 
      mutate(construct = unique_construct, 
             sig = p.value < .05) |> 
      select(construct, estimate, std_err = std.error, p_val = p.value, sig)
    
    lm_cf_cov_ls[[unique_construct]] <- lm_est_cf_cov
    
  }
  
};beepr::beep(2)

# bind n's
n <- 
  bind_rows(n_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n")

n_cov <- 
  bind_rows(n_cov_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n_cov")

# bind ate's
ate <- 
  bind_rows(ate_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

ate <- ate |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) |> 
  select(construct, estimate, std_err = std.err, p_val, sig)

ate |> filter(sig == T) |> arrange(-estimate)

# bind att's
att <- 
  bind_rows(att_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

att <- att |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) |> 
  select(construct, estimate, std_err = std.err, p_val, sig)

att |> filter(sig == T) |> arrange(-estimate)

ate |> write_csv('output/ate_manager_misalignment.csv')
att |> write_csv('output/att_manager_misalignment.csv')

# lm w/ covariates selected by causal forest 
lm_cf_cov <- bind_rows(lm_cf_cov_ls, .id = "construct")
lm_cf_cov |> write_csv('output/lm_cf_cov_manager_misalignment.csv')


### run linear estimates for misalignment ---------------------------------
lm_ls <- list()

for(unique_construct in unique_constructs) {
  
  # get data for manager misalignment
  dat <- grf_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(-mis_peer, -mis_direct_report, 
           #-contains("organization_"), 
           #-contains("members_language_")
    ) |> 
    drop_na()
  
  # continue the loop if no misaligned ratings
  if(nrow(dat) == 0) {next}
  
  else{
    
    # fit initial models to find covariates 
    organizations <- dat |> pull(organization_id)
    
    mod_lm_outcome <- lm(t1 ~ ., data = dat[-c(1:3,6)]) 
    vcov_outcome_clustered <- vcovCL(mod_lm_outcome, cluster = organizations)
    mod_lm_outcome_clustered <- 
      coeftest(mod_lm_outcome, vcov_outcome_clustered) |> 
      tidy() |> filter(p.value < .05)
    
    mod_lm_tx <- lm(mis_manager ~ ., data = dat[-c(1:2,5:6)]) 
    vcov_tx_clustered <- vcovCL(mod_lm_tx, cluster = organizations)
    mod_lm_tx_clustered <- 
      coeftest(mod_lm_tx, vcov_tx_clustered) |> 
      tidy() |> filter(p.value < .05)
    
    covariates <- mod_lm_tx_clustered |> 
      full_join(mod_lm_outcome_clustered, by="term") |> 
      drop_na() |> 
      slice(-1) |> 
      pull(term)
    
    
    # fit final lm model with linearly selected covariates 
    X <- dat |> 
      select(-t1, -mis_manager, -construct, 
             -user_id, -organization_id) |> 
      as.matrix()
    
    dat_lm <- dat |> 
      select(mis_manager, t1, colnames(X[,colnames(X) %in% covariates]))
    
    mod_lm <- lm(t1 ~ ., data = dat_lm) #|> tidy() |> filter(term == "mis_manager")
    vcov_clustered <- vcovCL(mod_lm, cluster = organizations)
    mod_lm_clustered <- coeftest(mod_lm, vcov_clustered) 
    
    lm_est <- mod_lm_clustered |> 
      tidy() |> 
      filter(term == "mis_manager") |> 
      mutate(construct = unique_construct, 
             sig = p.value < .05) |> 
      select(construct, estimate, std_err = std.error, p_val = p.value, sig)
    
    lm_ls[[unique_construct]] <- lm_est
    
  }
}; beepr::beep(2)

lm <- bind_rows(lm_ls, .id = "construct")
lm |> write_csv('output/lm_manager_misalignment.csv')


### run ATE and ATT for peer misalignment ----------------------------------

ate_ls <- list()
att_ls <- list()
n_ls <- list()
n_cov_ls <- list()

for(unique_construct in unique_constructs) {
  
  # get data for manager misalignment
  dat <- grf_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(#-mis_peer, 
      -mis_direct_report, 
      -mis_manager
    ) |> 
    drop_na()
  
  # continue the loop if no misaligned ratings
  if(nrow(dat) == 0) {next}
  
  else{
    
    # get vectors
    Y <- dat |> pull(t1) 
    W <- dat |> pull(mis_peer)
    organizations <- dat |> pull(organization_id)
    X <- dat |> 
      select(-t1, -mis_peer, -construct, 
             -user_id, -organization_id) |> 
      as.matrix()
    
    # get num of clusters
    n_ls[[unique_construct]] <- length(unique(organizations))
    
    # initial model 
    cf1 <- causal_forest(X, Y, W, clusters = organizations)
    
    # calculate variable importance
    var_imp <- variable_importance(cf1)
    
    # get relevant covariates
    covariates <- 
      tibble(var = colnames(X), 
             var_imp = var_imp[,1]) |> 
      filter(var_imp > 0) |> 
      pull(var)
    
    # get num of covariates
    n_cov_ls[[unique_construct]] <- length(covariates)
    
    # final model
    cf2 <- causal_forest(X[,colnames(X) %in% covariates], Y, W, clusters = organizations)
    
    # average treatment effects
    ate <- average_treatment_effect(cf2, target.sample = "all")
    ate <- as_tibble_row(ate)
    
    # average treatment effects of the treated
    att <- average_treatment_effect(cf2, target.sample = "treated")
    att <- as_tibble_row(att)
    
    # save output
    ate_ls[[unique_construct]] <- ate
    att_ls[[unique_construct]] <- att
    
  }
  
};beepr::beep(2)

# bind ate's
n <- 
  bind_rows(n_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n")

n_cov <- 
  bind_rows(n_cov_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n_cov")

ate <- 
  bind_rows(ate_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

ate <- ate |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) 

ate |> filter(sig == T) |> arrange(-estimate)

# bind att's
att <- 
  bind_rows(att_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

att <- att |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) |> 
  select(construct, estimate, std_err = std.err, p_val, sig)

att |> filter(sig == T) |> arrange(-estimate)

ate |> write_csv('output/ate_peer_misalignment.csv')
att |> write_csv('output/att_peer_misalignment.csv')


### run ATE and ATT for direct report misalignment ------------------------

ate_ls <- list()
att_ls <- list()
n_ls <- list()
n_cov_ls <- list()

for(unique_construct in unique_constructs) {
  
  # get data for manager misalignment
  dat <- grf_data |> 
    filter(str_detect(construct, unique_construct)) |> 
    select(
      -mis_peer, 
      #-mis_direct_report, 
      -mis_manager
    ) |> 
    drop_na()
  
  # continue the loop if no misaligned ratings
  if(nrow(dat) == 0) {next}
  
  else{
    
    # get vectors
    Y <- dat |> pull(t1) 
    W <- dat |> pull(mis_direct_report)
    organizations <- dat |> pull(organization_id)
    X <- dat |> 
      select(-t1, -mis_direct_report, -construct, 
             -user_id, -organization_id) |> 
      as.matrix()
    
    # get num of clusters
    n_ls[[unique_construct]] <- length(unique(organizations))
    
    # initial model 
    cf1 <- causal_forest(X, Y, W, clusters = organizations)
    
    # calculate variable importance
    var_imp <- variable_importance(cf1)
    
    # get relevant covariates
    covariates <- 
      tibble(var = colnames(X), 
             var_imp = var_imp[,1]) |> 
      filter(var_imp > 0) |> 
      pull(var)
    
    # get num of covariates
    n_cov_ls[[unique_construct]] <- length(covariates)
    
    # final model
    cf2 <- causal_forest(X[,colnames(X) %in% covariates], Y, W, clusters = organizations)
    
    # average treatment effects
    ate <- average_treatment_effect(cf2, target.sample = "all")
    ate <- as_tibble_row(ate)
    
    # average treatment effects of the treated
    att <- average_treatment_effect(cf2, target.sample = "treated")
    att <- as_tibble_row(att)
    
    # save output
    ate_ls[[unique_construct]] <- ate
    att_ls[[unique_construct]] <- att
    
  }
  
};beepr::beep(2)

# bind ate's
n <- 
  bind_rows(n_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n")

n_cov <- 
  bind_rows(n_cov_ls, .id = "construct") |> 
  pivot_longer(everything(), 
               names_to = "construct", 
               values_to = "n_cov")

ate <- 
  bind_rows(ate_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

ate <- ate |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) 

ate |> filter(sig == T) |> arrange(-estimate)

# bind att's
att <- 
  bind_rows(att_ls, .id = "construct") |> 
  left_join(n) |> 
  left_join(n_cov)

att <- att |> 
  mtt(
    t = (estimate - 0) / std.err, #null = 0 ate
    df = n - (n_cov + 1), #subtracted by num covs + num estimates
    p_val = 2 * (1 - pt(abs(t), df)),
    sig = p_val < .05
  ) |> 
  select(construct, estimate, std_err = std.err, p_val, sig)

att |> filter(sig == T) |> arrange(-estimate)

ate |> write_csv('output/ate_direct_report_misalignment.csv')
att |> write_csv('output/att_direct_report_misalignment.csv')


# misalignment descriptives -----------------------------------------------

# misalignment across raters
mis_desc <- grf_data |> 
  select(1:7) |> 
  replace_na(list(mis_manager = 0, mis_peer = 0, mis_direct_report = 0)) |> 
  drop_na() |> 
  mtt(misaligned = mis_manager + mis_peer + mis_direct_report, 
      diff = t1 - t0) 

mis_desc |> 
  group_by(construct) |> 
  reframe(mis_mean = mean(misaligned), 
          diff = mean(diff)) |> 
  mtt(construct = snakecase::to_title_case(construct),
      construct = fct_reorder(as_factor(construct), mis_mean)) |> 
  pivot_longer(mis_mean:diff, names_to = "group", values_to = "score") |> 
  ggplot() + 
  aes(construct, score, fill = group) + 
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  facet_grid(~group) +
  theme(legend.position = "none")


mis_desc |> 
  group_by(construct) |> 
  reframe(mis_mean = mean(misaligned), 
          diff = -mean(diff)) |> 
  mtt(construct = snakecase::to_title_case(construct),
      construct = fct_reorder(as_factor(construct), mis_mean)) |> 
  pivot_longer(mis_mean:diff, names_to = "group", values_to = "score") |> 
  ggplot() + 
  aes(construct, score, fill = group, color = group) +
  geom_line(stat = "identity") +
  geom_point(stat = "identity") + 
  coord_flip()


mis_desc |> 
  select(contains('mis_')) |> 
  as.matrix() |> 
  psych::tetrachoric()

