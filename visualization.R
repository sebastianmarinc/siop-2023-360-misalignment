
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

## tx effects ----
dat <- bind_rows(
  att_manager, lmer_manager,
  att_peer, lmer_peer, 
  att_direct_report, lmer_direct_report
) |> 
  mtt(Source = c(rep("Manager", 60), rep("Peer", 60), rep("Direct Report", 60)), 
      Model = rep(c("Causal Forest", "Linear Regression"), 3, each = 30)) 

dat |> 
  ggplot() +
  aes(estimate, fill = Model,) +
  geom_density(alpha = .75, color = "black") +
  facet_grid(~Source) + 
  theme_classic() + 
  theme(
    text = element_text(size=15),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  xlab("Estimated Treatment Effects") + 
  scale_fill_brewer(palette = "Set1") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) 

ggsave('tx.png', dpi = 700, height = 4, width = 8)


## top 5 tx effects estimated by causal forest -----
dat |> 
  mtt(construct = snakecase::to_title_case(construct)) |>
  filter(Model == "Causal Forest") |> 
  group_by(Source) |> 
  slice_max(estimate, n = 5) |> 
  ggplot() + 
  aes(reorder(construct, estimate), estimate) + 
  geom_point(stat = "identity", size = 3, color = "black") +
  geom_linerange(aes(ymin = estimate - std_err, 
                     ymax = estimate + std_err), size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) + 
  coord_flip() + 
  theme_classic() + 
  theme(text = element_text(size = 14)) + 
  labs(
    y = "Estimated Treatment Effects",
    x = "Construct"
  ) + 
  facet_wrap(~Source, scales = "free_y")

ggsave('tx-top5.png', dpi = 700, height = 4, width = 8)
