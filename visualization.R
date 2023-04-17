
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)

ate_manager <- read_csv('output/ate_manager_misalignment.csv')
ate_peer <- read_csv('output/ate_peer_misalignment.csv')
ate_direct_report <- read_csv('output/ate_direct_report_misalignment.csv')

att_manager <-read_csv('output/att_manager_misalignment.csv')

lm_cf_manager <- read_csv('output/lm_cf_cov_manager_misalignment.csv')
lm_manager <- read_csv('output/lm_manager_misalignment.csv')

lmer_manager <- read_csv('output/lmer_manager.csv')
lmer_peer <- read_csv('output/lmer_peer.csv')
lmer_direct_report <- read_csv('output/lmer_direct_report.csv')



# manager viz -------------------------------------------------------------

# data for manager misalignment
dat_mgr <- bind_rows(
  att_manager, lm_cf_manager, lm_manager, lmer_test,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "cf", 
    source == 2 ~ "lm + cf", 
    source == 3 ~ "lm",
    source == 4 ~ "lmer"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

# select top 5 mean diff based on causal forest 
top_five_cf_constructs <- dat_mgr |> 
  filter(source == "cf") |> 
  slice_max(n = 5, order_by = estimate) |> 
  pull(construct)

# get top 5 from overall data
dat_mgr |> 
  filter(construct %in% top_five_cf_constructs)

dat_mgr |> 
  filter(str_detect(source, "^cf|lmer")) |> 
  ggplot() +
  aes(estimate, fill = source) |> 
  geom_density(alpha = .4) 


# peer viz -------------------------------------------------------------

# data for manager misalignment
dat_peer <- bind_rows(
  att_manager, lm_cf_manager, lm_manager, lmer_test,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "cf", 
    source == 2 ~ "lm + cf", 
    source == 3 ~ "lm",
    source == 4 ~ "lmer"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

# select top 5 mean diff based on causal forest 
top_five_cf_constructs <- dat_mgr |> 
  filter(source == "cf") |> 
  slice_max(n = 5, order_by = estimate) |> 
  pull(construct)

# get top 5 from overall data
dat_mgr |> 
  filter(construct %in% top_five_cf_constructs)

dat_mgr |> 
  filter(str_detect(source, "^cf|lmer")) |> 
  ggplot() +
  aes(estimate, fill = source) |> 
  geom_density(alpha = .4) 


# direct report viz -------------------------------------------------------------

# data for manager misalignment
dat_mgr <- bind_rows(
  att_manager, lm_cf_manager, lm_manager, lmer_test,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "cf", 
    source == 2 ~ "lm + cf", 
    source == 3 ~ "lm",
    source == 4 ~ "lmer"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

# select top 5 mean diff based on causal forest 
top_five_cf_constructs <- dat_mgr |> 
  filter(source == "cf") |> 
  slice_max(n = 5, order_by = estimate) |> 
  pull(construct)

# get top 5 from overall data
dat_mgr |> 
  filter(construct %in% top_five_cf_constructs)

dat_mgr |> 
  filter(str_detect(source, "^cf|lmer")) |> 
  ggplot() +
  aes(estimate, fill = source) |> 
  geom_density(alpha = .4) 

# visualization -----------------------------------------------------------

dat <- bind_rows(ate_manager, ate_peer, ate_direct_report, 
                 .id = "source") |> 
  mtt(source = case_when(
    source == 1 ~ "manager misalignment", 
    source == 2 ~ "peer misalignment", 
    source == 3 ~ "direct report misalignment"
  )) #|> 

dat |> 
  mtt(construct = snakecase::to_title_case(construct)) |> 
  ggplot() + 
  aes(construct, estimate, fill = sig) + 
  geom_bar(stat = "identity") +
  geom_linerange(aes(ymin = estimate - std.err, ymax = estimate + std.err)) +
  coord_flip() + 
  facet_wrap(~source)


dat_order <- dat %>%
  group_by(source) %>%
  mutate(order_var = rank(estimate, ties.method = "min")) %>%
  ungroup()

# Step 2: Reorder factors based on the new ordering variable
dat_order$construct <- with(dat_order, reorder(interaction(construct, source),  order_var))

# Step 3: Create the plot with facet_wrap()
ggplot(dat_order, aes(x = construct, y = estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_linerange(aes(ymin = estimate - std.err, ymax = estimate + std.err)) +
  coord_flip() +
  facet_wrap(~ source, scales = "free_y")
