# Running the model and storing outputs

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

#########################
# General results

rm(list = ls())
devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# slow TTI (1 day each for test and trace)
# good implementation
scenarios1 <- tidyr::expand_grid(
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
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results1, file = "data-raw/res_1.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

tic()
# medium speed TTI (1 day total for test and trace)
# good implementation
# testing asymptomatics: false
scenarios2 <- tidyr::expand_grid(
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
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results2, file = "data-raw/res_2.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

tic()
# medium speed TTI (1 day total for test and trace)
# good implementation
# testing asymptomatics: true
scenarios2b <- tidyr::expand_grid(
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
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(TRUE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results2b <- ringbp::parameter_sweep(scenarios2b,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results2b, file = "data-raw/res_2b.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# fast TTI (instant testing and tracing)
# good implementation
scenarios3 <- tidyr::expand_grid(
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
  min_quar_delay = 0,
  max_quar_delay = c(0),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")


#+ full_run
tic()
## Run parameter sweep
sweep_results3 <- ringbp::parameter_sweep(scenarios3,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)

toc()


# #+ writeout
saveRDS(sweep_results3, file = "data-raw/res_3.rds")
rm(list=ls())

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# slow TTI (1 day each for test and trace)
# poor adherence
scenarios4 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.182),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.119),
  iso_adhere = c(0.109),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results4 <- ringbp::parameter_sweep(scenarios4,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)

toc()


# #+ writeout
saveRDS(sweep_results4, file = "data-raw/res_4.rds")
rm(list=ls())

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# medium speed TTI (1 day total for test and trace)
# poor adherence
# testing asymptomatics: false
scenarios5 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.182),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.119),
  iso_adhere = c(0.109),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())


cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results5 <- ringbp::parameter_sweep(scenarios5,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results5, file = "data-raw/res_5.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# medium speed TTI (1 day total for test and trace)
# poor adherence
# testing asymptomatics: true
scenarios5b <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.182),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.119),
  iso_adhere = c(0.109),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(TRUE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results5b <- ringbp::parameter_sweep(scenarios5b,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results5b, file = "data-raw/res_5b.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# fast TTI (instant testing and tracing)
# poor adherence
scenarios6 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.182),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 0,
  max_quar_delay = c(0),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.119),
  iso_adhere = c(0.109),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results6 <- ringbp::parameter_sweep(scenarios6,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results6, file = "data-raw/res_6.rds")
rm(list=ls())
Sys.sleep(120)



##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# slow TTI (1 day each for test and trace)
# good adherence with boosted traced isolation adherence
scenarios7 <- tidyr::expand_grid(
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
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.9),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results7 <- ringbp::parameter_sweep(scenarios7,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results7, file = "data-raw/res_7.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# medium speed TTI (1 day total for test and trace)
# good adherence with boosted traced isolation adherence
# testing asymptomatics: false
scenarios8 <- tidyr::expand_grid(
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
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.9),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results8 <- ringbp::parameter_sweep(scenarios8,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results8, file = "data-raw/res_8.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# medium speed TTI (1 day total for test and trace)
# good adherence with boosted traced isolation adherence
# testing asymptomatics: true
scenarios8b <- tidyr::expand_grid(
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
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.3),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.9),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(TRUE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results8b <- ringbp::parameter_sweep(scenarios8b,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results8b, file = "data-raw/res_8b.rds")
rm(list=ls())
Sys.sleep(120)

##################################################################

rm(list = ls())
devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

# fast TTI (instant testing and tracing)
# good adherence with boosted traced isolation adherence
scenarios9 <- tidyr::expand_grid(
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
  min_quar_delay = 0,
  max_quar_delay = c(0),
  index_R0 = c(1.3,1.5),
  prop.asym = c(0.31),
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.9),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed parameters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.23,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results9 <- ringbp::parameter_sweep(scenarios9,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut=FALSE)
toc()


# #+ writeout
saveRDS(sweep_results9, file = "data-raw/res_9.rds")
rm(list=ls())
Sys.sleep(120)

  ##################################################################

# Load in separate sets of scenarios and combine
sweep_results1 <- readRDS("data-raw/res_1.rds")
sweep_results2 <- readRDS("data-raw/res_2.rds")
sweep_results2b <- readRDS("data-raw/res_2b.rds")
sweep_results3 <- readRDS("data-raw/res_3.rds")
sweep_results4 <- readRDS("data-raw/res_4.rds")
sweep_results5 <- readRDS("data-raw/res_5.rds")
sweep_results5b <- readRDS("data-raw/res_5b.rds")
sweep_results6 <- readRDS("data-raw/res_6.rds")
sweep_results7 <- readRDS("data-raw/res_7.rds")
sweep_results8 <- readRDS("data-raw/res_8.rds")
sweep_results8b <- readRDS("data-raw/res_8b.rds")
sweep_results9 <- readRDS("data-raw/res_9.rds")
#

sweep_results <- as_tibble(rbind(sweep_results1,sweep_results2,sweep_results2b,sweep_results3,sweep_results4,sweep_results5,
                                sweep_results5b,sweep_results6,sweep_results7,sweep_results8,sweep_results8b,sweep_results9))
sweep_results$scenario <- 1:nrow(sweep_results)

# Clear redundant data frames from working memory
sweep_results1 = sweep_results2 = sweep_results3 = sweep_results4 = sweep_results5 = sweep_results5b = sweep_results6 = sweep_results7 = sweep_results8 = sweep_results8b = sweep_results9 = sweep_results2b = c()

# # # Save final combined results (DONE)
saveRDS(sweep_results, file = "data-raw/res_complete.rds")

