#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @inheritParams outbreak_model
#' @param n.sim number of simulations to run
#' @param num.initial.cases Initial number of cases in each initial cluster
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#'
#' @importFrom purrr safely
#' @return
#' @importFrom stats as.formula
#' @importFrom stats dist
#' @importFrom stats dweibull
#' @importFrom stats dnbinom
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats rweibull
#' @importFrom data.table :=
#' @importFrom utils data
#' @return A data.table object returning the results for multiple simulations using
#' the same set of parameters. The table has columns
#' * week: The week in the simulation.
#' * weekly_cases: The number of new cases that week.
#' * cumulative: The cumulative cases.
#' * effective_r0: The effective reproduction rate for the whole simulation
#' * cases_per_gen: A list column with the cases per generation. This is repeated each row.
#' * sim: Index column for which simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- scenario_sim(n.sim = 5,
#' num.initial.cases = 5,
#' cap_max_days = 365,
#' cap_cases = 2000,
#' r0isolated = 0,
#' r0community = 2.5,
#' disp.iso = 1,
#' disp.com = 0.16,
#' k = 0.7,
#' delay_shape = 2.5,
#' delay_scale = 5,
#' prop.asym = 0,
#' prop.ascertain = 0)
#' }
#' res <- scenario_sim(n.sim = 2,
#'                     num.initial.cases = 5,
#'                     cap_max_days = 365,
#'                     cap_cases = 400,
#'                     r0isolated = 0,
#'                     r0community = 2.5,
#'                     disp.iso = 1,
#'                     disp.com = 0.16,
#'                     delay_shape = 2.5,
#'                     delay_scale = 5,
#'                     inc_meanlog = 1,
#'                     inc_sdlog = 2,
#'                     inf_shape = 2,
#'                     inf_rate = 2,
#'                     inf_shift = -2,
#'                     prop.asym = 0,
#'                     prop.ascertain = 0)
#' }
#'
scenario_sim <- function(n.sim = NULL, prop.ascertain = NULL, cap_max_days = NULL, cap_cases = NULL,
                         r0isolated = NULL, r0community = NULL, disp.iso = NULL, disp.com = NULL,
                         delay_shape = NULL, delay_scale = NULL, inc_meanlog = NULL, inc_sdlog = NULL,
                         inf_shape = NULL, inf_rate = NULL, inf_shift = NULL, num.initial.cases = NULL,
                         min_quar_delay = 1, max_quar_delay = NULL, sensitivity = NULL, precaution = NULL,
                         self_report = NULL, test_delay = NULL, prop.asym = NULL, quarantine = NULL,
                         earlyOut = NULL, iso_adhere = NULL,test_asym = NULL) {

  if(sensitivity==0){
    testing = FALSE
  } else {
    testing = TRUE
  }

  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                             prop.ascertain = prop.ascertain,
                                             cap_max_days = cap_max_days,
                                             cap_cases = cap_cases,
                                             r0isolated = r0isolated,
                                             r0community = r0community,
                                             disp.iso = disp.iso,
                                             disp.com = disp.com,
                                             delay_shape = delay_shape,
                                             delay_scale = delay_scale,
                                             inc_meanlog = inc_meanlog,
                                             inc_sdlog = inc_sdlog,
                                             inf_shape = inf_shape,
                                             inf_rate = inf_rate,
                                             inf_shift = inf_shift,
                                             prop.asym = prop.asym,
                                             min_quar_delay = min_quar_delay,
                                             max_quar_delay = max_quar_delay,
                                             test_delay = test_delay,
                                             sensitivity = sensitivity,
                                             precaution = precaution,
                                             self_report = self_report,
                                             iso_adhere = iso_adhere,
                                             quarantine = quarantine,
                                             testing = testing,
                                             earlyOut = earlyOut,
                                             test_asym = test_asym))



  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[, sim := rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)), ]
  return(res)

  # res1 <- data.table::rbindlist(res[[1]])
  # res1[, sim := rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)), ]
  # res2 <- res[[2]]
  # return(list(res1,res2))
}


utils::globalVariables(c(".", ".N", ":=", "asym", "control_effectiveness", "cumulative", "data", "delay",
                         "disp",  "exposure", "extinct", "full_join", "incubfn_sample",
                         "index_R0", "infector_iso_time", "iqr_lower", "iqr_upper", "isolated", "isolated_time",
                         "label_extinct", "lower", "max_weekly_cases", "median_eff_r0", "missed",
                         "new_cases", "num.initial.cases", "onset", "pext", "prob_extinct", "prop.asym",
                         "r0", "rweibull", "samp", "samples", "scenario", "sim", "sims", "theta", "upper", "value",
                         "week", "weekly_cases", "x", "y", "y0", "y100", "y25", "y50", "y75"))

