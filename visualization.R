
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse, hrbrthemes)

ate_manager <- read_csv('output/ate_manager_misalignment.csv')
ate_peer <- read_csv('output/ate_peer_misalignment.csv')
ate_direct_report <- read_csv('output/ate_direct_report_misalignment.csv')

att_manager <-read_csv('output/att_manager_misalignment.csv')
att_peer <-read_csv('output/att_peer_misalignment.csv')
att_direct_report <-read_csv('output/att_direct_report_misalignment.csv')

lm_cf_manager <- read_csv('output/lm_cf_cov_manager_misalignment.csv')
lm_manager <- read_csv('output/lm_manager_misalignment.csv')

lmer_manager <- read_csv('output/lmer_manager.csv')
lmer_peer <- read_csv('output/lmer_peer.csv')
lmer_direct_report <- read_csv('output/lmer_direct_report.csv')


# manager viz -------------------------------------------------------------

# data for manager misalignment
dat_mgr <- bind_rows(
  att_manager, lmer_test,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "Causal Forest", 
    source == 2 ~ "Linear Regression"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

# select top 5 mean diff based on causal forest 
top_five_cf_constructs <- dat_mgr |> 
  filter(source == "Causal Forest") |> 
  slice_max(n = 5, order_by = estimate) |> 
  pull(construct)

# get top 5 from overall data
dat_mgr |> 
  filter(construct %in% top_five_cf_constructs)

p1 <- dat_mgr |> 
  pivot_wider(names_from = "source", values_from = "estimate") |> 
  select(construct, `Causal Forest`, `Linear Regression`) |> 
  group_by(construct) |> 
  reframe(across(everything(), ~mean(.x, na.rm=T))) |> 
  ggplot() +
  aes(x=x) +
  # geom_density(alpha = .4) 
  geom_density( aes(x = `Causal Forest`, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=.15, y=5, label="Causal Forest"), color="#69b3a2") +
  geom_density( aes(x = `Linear Regression`, y = -..density..), fill= "#404080") +
  geom_label( aes(x=.05, y=-5, label="Linear Regression"), color="#404080") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(
    title = "Manager Misalignment",
    x = "Estimated Treatment Effects"
  )



# peer viz -------------------------------------------------------------

# data for manager misalignment
dat_mgr <- bind_rows(
  att_manager, lmer_test,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "Causal Forest", 
    source == 2 ~ "Linear Regression"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

# # select top 5 mean diff based on causal forest 
# top_five_cf_constructs <- dat_mgr |> 
#   filter(source == "Causal Forest") |> 
#   slice_max(n = 5, order_by = estimate) |> 
#   pull(construct)
# 
# # get top 5 from overall data
# dat_mgr |> filter(construct %in% top_five_cf_constructs)

p1 <- dat_mgr |> 
  pivot_wider(names_from = "source", values_from = "estimate") |> 
  select(construct, `Causal Forest`, `Linear Regression`) |> 
  group_by(construct) |> 
  reframe(across(everything(), ~mean(.x, na.rm=T))) |> 
  ggplot() +
  aes(x=x) +
  # geom_density(alpha = .4) 
  geom_density( aes(x = `Causal Forest`, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=.15, y=5, label="Causal Forest"), color="#69b3a2") +
  geom_density( aes(x = `Linear Regression`, y = -..density..), fill= "#404080") +
  geom_label( aes(x=.05, y=-5, label="Linear Regression"), color="#404080") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(
    title = "Manager Misalignment",
    x = "Estimated Treatment Effects"
  )


# direct report viz -------------------------------------------------------------

dat_peer <- bind_rows(
  att_peer, lmer_peer,
  .id = "source"
) |> 
  mtt(source = case_when(
    source == 1 ~ "Causal Forest", 
    source == 2 ~ "Linear Regression"
  )) |> 
  arrange(construct) |> 
  select(construct, everything())

p2 <- dat_peer |> 
  pivot_wider(names_from = "source", values_from = "estimate") |> 
  select(construct, `Causal Forest`, `Linear Regression`) |> 
  group_by(construct) |> 
  reframe(across(everything(), ~mean(.x, na.rm=T))) |> 
  ggplot() +
  aes(x=x) +
  # geom_density(alpha = .4) 
  geom_density( aes(x = `Causal Forest`, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=.15, y=5, label="Causal Forest"), color="#69b3a2") +
  geom_density( aes(x = `Linear Regression`, y = -..density..), fill= "#404080") +
  geom_label( aes(x=.05, y=-5, label="Linear Regression"), color="#404080") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(
    title = "Peer Misalignment",
    x = "Estimated Treatment Effects"
  )


pacman::p_load(patchwork)

p1+p2



# visualization -----------------------------------------------------------


dat <- bind_rows(
  att_manager, lmer_manager,
  att_peer, lmer_peer, 
  att_direct_report, lmer_direct_report
) |> 
  mtt(Source = c(rep("Manager", 60), rep("Peer", 60), rep("Direct Report", 60)), 
      Model = rep(c("Causal Forest", "Linear Regression"), 3, each = 30)) 



#####
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
