
context("Test basic usage")


test_that("A basic sim returns the correct object", {

  set.seed(20200410)
  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,'weibull')

  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              sensitivity = 0.9,
                              precaution = 3,
                              self_report = 0,
                              prop.asym = 0,
                              iso_adhere = 0.9)

  # generate next generation of cases
  case_data2 <- outbreak_step(case_data = case_data,
                             disp.iso = 1,
                             disp.com = 0.16,
                             r0isolated = 0,
                             r0community = 500, # almost guarentees to get new cases
                             prop.asym = 0,
                             incfn = incfn,
                             delayfn = delayfn,
                             inf_shape = 2.115779,
                             inf_rate = 0.6898583,
                             inf_shift = 3,
                             max_quar_delay = 4,
                             min_quar_delay = 1,
                             prop.ascertain = 0,
                             quarantine = FALSE,
                             testing = FALSE,
                             sensitivity = 0.9,
                             precaution = 5,
                             self_report = 0,
                             test_delay = 0,
                             test_asym = F,
                             iso_adhere = 0.9)

  expect_true(nrow(case_data2$cases) > 1)
  expect_equal(as.vector(table(case_data2$cases$infector)), c(1, nrow(case_data2$cases) - 1))



  # With R0 = 0 we should get no additional cases.
  case_data3 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 0,
                              r0community = 0, # almost guarentees to get new cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 0,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(nrow(case_data3$cases) == 1)


})




test_that("Sim with multiple infectors makes senes", {

  set.seed(20212318)
  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,'weibull')

  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 2,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              sensitivity = 0.1,
                              precaution = 3,
                              self_report = 0,
                              prop.asym = 0,
                              iso_adhere = 0.9)

  # generate next generation of cases
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 1000, # almost guarentees to get new cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              prop.ascertain = 0,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(nrow(case_data2$cases) > 1)

  expect_true(as.vector(table(case_data2$cases$infector))[1] == 2)

  expect_true(all(as.vector(table(case_data2$cases$infector))[2:3] > 1))
})


test_that("R0isolated is working properly", {

  set.seed(20200410)
  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,'weibull')

  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              sensitivity = 0.9,
                              precaution = 3,
                              self_report = 0,
                              prop.asym = 0,
                              iso_adhere = 0.9)
  case_data$isolated <- TRUE

  # generate next generation of cases
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 0,
                              r0community = 1000, # almost guarentees to get new cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              prop.ascertain = 0,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)


  expect_true(nrow(case_data2$cases) == 1)



  # generate next generation of cases
  case_data3 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 500, # Shoiuld get lots of cases
                              r0community = 0, # Case is isolated so irrelevent
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 0,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(nrow(case_data3$cases) > 1)

})



test_that('Test self_report arg',{

  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,'weibull')
  # generate initial cases

  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              prop.asym=0,
                              self_report = 0.2,
                              iso_adhere = 0.9)

  # generate next generation of cases
  # Ascertain = 0
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Should get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 0.5,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)

    expect_true(all(case_data2$cases$missed))

    # Now ascertain = 1
    case_data3 <- outbreak_step(case_data = case_data,
                                disp.iso = 1,
                                disp.com = 1,
                                r0isolated = 0,
                                r0community = 500, # Shoiuld get lots of cases
                                prop.asym = 0,
                                incfn = incfn,
                                delayfn = delayfn,
                                inf_shape = 2.115779,
                                inf_rate = 0.6898583,
                                inf_shift = 3,
                                prop.ascertain = 0.5,
                                max_quar_delay = 4,
                                min_quar_delay = 1,
                                quarantine = FALSE,
                                testing = FALSE,
                                sensitivity = 0.9,
                                precaution = 5,
                                self_report = 1,
                                test_delay = 0,
                                test_asym = F,
                                iso_adhere = 1)


    # The index case should be missed but no others.
    #  This test relies on the index being symptomatic which I haven't forced.
    expect_true(sum(case_data3$cases$missed) == 1)


    case_data4 <- outbreak_step(case_data = case_data,
                                disp.iso = 1,
                                disp.com = 1,
                                r0isolated = 0,
                                r0community = 500, # Shoiuld get lots of cases
                                prop.asym = 0,
                                incfn = incfn,
                                delayfn = delayfn,
                                inf_shape = 2.115779,
                                inf_rate = 0.6898583,
                                inf_shift = 3,
                                prop.ascertain = 0.5,
                                max_quar_delay = 4,
                                min_quar_delay = 1,
                                quarantine = FALSE,
                                testing = FALSE,
                                sensitivity = 0.9,
                                precaution = 5,
                                self_report = 0.5,
                                test_delay = 0,
                                test_asym = F,
                                iso_adhere = 0.9)

    # After ignoring the index case we should still get both true and false.
    # This is more complicated with prop.asym and self_report.
    # I'll add more tests.
    expect_length(unique(case_data4$cases$missed[-1]), 2)


})




test_that('Test ascertain arg',{

  inc_meanlog = 1.434065
  inc_sdlog = 0.6612

  incfn <- dist_setup(dist_param1 = inc_meanlog,
                      dist_param2 = inc_sdlog,
                      dist_type = 'lognormal')

  delay_shape = 0.9
  delayfn <- dist_setup(delay_shape,
                        1, "adherence")

  # generate initial cases

  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              precaution = 2,
                              prop.asym=0,
                              self_report = 0.2,
                              iso_adhere = 0.9)

  # Start with a tracked individual, not in isolation. So I'm not sure how that will work.
  case_data$missed <- FALSE
  case_data$isolated_time <- 100

  # generate next generation of cases
  # Ascertain = 1 so all cases should be tracked.
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 1)

  expect_true(all(!case_data2$cases$missed))
  expect_true(nrow(case_data2$cases) > 1)



  # generate next generation of cases
  # Ascertain = 0 so all cases except the index should not be tracked.
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 0,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0)

  expect_true(all(case_data2$cases$missed[-1]))
  expect_true(nrow(case_data2$cases) > 1)

})



test_that('Test testing arg',{

  inc_meanlog = 1.434065
  inc_sdlog = 0.6612

  incfn <- dist_setup(dist_param1 = inc_meanlog,
                      dist_param2 = inc_sdlog,
                      dist_type = 'lognormal')

  delay_shape = 0.9
  delayfn <- dist_setup(delay_shape,
                        1, "adherence")

  # generate initial cases

  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = FALSE,
                              test_delay = 1,
                              prop.asym=0,
                              self_report = 0.2,
                              iso_adhere = 0.9)


  # generate next generation of cases
  # Testing = FALSE so all tests should be NA
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = FALSE,
                              sensitivity = 0.9,
                              precaution = 5,
                              self_report = 0,
                              test_delay = 0,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(all(is.na(case_data2$cases$test_result)))



  # generate next generation of cases
  # Testing = TRUE so same tests should be TRUE or FALSE

  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = TRUE,
                              test_delay = 1,
                              prop.asym=0,
                              self_report = 0.2,
                              iso_adhere = 0.9)

  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = TRUE,
                              sensitivity = 0.9,
                              test_delay = 1,
                              precaution = 5,
                              self_report = 0.5,
                              test_asym = F,
                              iso_adhere = 0.9)


  expect_true(all(c(TRUE, FALSE) %in% case_data2$cases$test_result))
})



test_that('Test sensitivity arg',{


  inc_meanlog = 1.434065
  inc_sdlog = 0.6612

  incfn <- dist_setup(dist_param1 = inc_meanlog,
                      dist_param2 = inc_sdlog,
                      dist_type = 'lognormal')

  delay_shape = 0.9
  delayfn <- dist_setup(delay_shape,
                        1, "adherence")



  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              testing = TRUE,
                              test_delay = 1,
                              prop.asym=0,
                              sensitivity = 0.9,
                              self_report = 0,
                              iso_adhere = 0.9)


  # generate next generation of cases
  # Sensitivity = 1 so should be NAs (missed) and TRUEs but not falses.
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = TRUE,
                              sensitivity = 1,
                              precaution = 5,
                              test_delay = 0.3,
                              self_report = 0.5,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(all(case_data2$cases$test_result %in% c(TRUE, NA)))
  # Missed cases should be na, not missed should be TRUE
  expect_true(all(case_data2$cases$test_result[case_data2$cases$missed == FALSE] == TRUE))
  expect_true(all(is.na(case_data2$cases$test_result[case_data2$cases$missed == TRUE])))



  # generate next generation of cases
  # Sensitivity = 0 so should be NAs (missed) and FALSE but no TRUES.
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = TRUE,
                              sensitivity = 0,
                              precaution = 5,
                              test_delay = 0.3,
                              self_report = 0.5,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(all(case_data2$cases$test_result %in% c(FALSE, NA)))
  # Missed cases should be na, not missed should be TRUE
  expect_true(all(case_data2$cases$test_result[case_data2$cases$missed == FALSE] == FALSE))
  expect_true(all(is.na(case_data2$cases$test_result[case_data2$cases$missed == TRUE])))



  # generate next generation of cases
  # Sensitivity = 0.5 so should be NAs (missed) and FALSE and TRUES.
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 1,
                              r0isolated = 0,
                              r0community = 500, # Shoiuld get lots of cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              inf_shape = 2.115779,
                              inf_rate = 0.6898583,
                              inf_shift = 3,
                              prop.ascertain = 1,
                              max_quar_delay = 4,
                              min_quar_delay = 1,
                              quarantine = FALSE,
                              testing = TRUE,
                              sensitivity = 0.5,
                              precaution = 5,
                              test_delay = 0.3,
                              self_report = 0.5,
                              test_asym = F,
                              iso_adhere = 0.9)

  expect_true(all(case_data2$cases$test_result %in% c(FALSE, TRUE, NA)))
  expect_true(all(c(FALSE, TRUE, NA) %in% case_data2$cases$test_result))

  # Missed cases should be na, not missed should be TRUE
  expect_true(all(c(TRUE, FALSE) %in% case_data2$cases$test_result[case_data2$cases$missed == FALSE]))
  expect_true(all(case_data2$cases$test_result[case_data2$cases$missed == FALSE] %in% c(TRUE, FALSE)))

  expect_true(all(is.na(case_data2$cases$test_result[case_data2$cases$missed == TRUE])))

})












