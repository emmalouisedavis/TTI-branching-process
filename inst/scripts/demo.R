library(data.table)
library(tidyverse)
library(git2r)
library(tictoc)
library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(furrr)
library(sn)
library(ggrepel)
library(testthat)
library(svglite)

rm(list = ls()) #clear workspace
devtools::load_all() #load in ringbp package manually
options(future.rng.onMisuse="ignore")

no.samples <- 50 # number of iterations/simulations for each scenario
cap_cases <- 2000 # maximum number of cases before terminating simulation
max_days <- 300 # maximum number of days before terminating simulation

set.seed(200518)

# Example scenarios (30 in total)
# Variable control effectiveness: 0, 0.2, 0.4, 0.6, 0.8
# Variable index Rs: 1.5, 2.0, 2.5
# Precationary isolation of negative testing individuals: 0, 7 (days)
scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.7),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.5,2.0,2.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
  precaution = c(0,7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())


## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

future::plan("multicore")

## Run parameter sweep through scenarios
sweep_results <- ringbp::parameter_sweep(scenarios,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)

# Example plots:

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

res <- sweep_results

lower <- rep(NA,nrow(sweep_results))
upper <- rep(NA,nrow(sweep_results))

for(i in 1:nrow(sweep_results)){
  out <- prop.test(sweep_results$pext[i]*no.samples,no.samples,correct=FALSE)
  CIs <- as.numeric(unlist(out[6]))
  lower[i] <- CIs[1]
  upper[i] <- CIs[2]
}

sweep_results <- sweep_results %>% mutate(lower = lower,
                      upper = upper)
lower <- c()
upper <- c()

sweep_results$index_R0 <- factor(sweep_results$index_R0)
sweep_results$index_R0 <- factor(sweep_results$index_R0, levels = rev(levels(sweep_results$index_R0)))

Fig1 <- sweep_results %>%
  mutate(index_R0 = factor(index_R0, labels = c('Rs=2.5','2.0','1.5'))) %>%
  mutate(precaution = factor(precaution, labels = c('leave quarantine if negative', '7 day quarantine'))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(7,2,4)],guide="none") +
  geom_line() +
  geom_point(size=2) +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  facet_grid(index_R0 ~ precaution) +
  theme_cowplot(font_size = 16) +
  theme(strip.background =element_rect(fill="white")) +
  theme(legend.position=c(0.8,0.36),legend.title = element_text(size=14)) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak")

Fig1
