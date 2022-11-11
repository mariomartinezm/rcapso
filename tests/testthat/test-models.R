test_that("test intraspecific competition", {
  expect_equal(rcapso_mf_ic(0, 0), 0)
  expect_equal(rcapso_mf_ic(1, 0.1), 0.9, tolerance = 1e-5)
  expect_equal(rcapso_mf_ic(0.5, 0.002), 0.4995, tolerance = 1e-5)
})

test_that("test reproduction", {
  expect_equal(rcapso_mf_reproduction(0, 0, 0), 0)
  expect_equal(rcapso_mf_reproduction(0.5, 2, 3), 0.817993125, tolerance = 1e-9)
  expect_equal(rcapso_mf_reproduction(1, 1, 1), 1)
})

test_that("test death of predators", {
  expect_equal(rcapso_mf_death_of_predators(0, 0, 0, 0), 0)
  expect_equal(rcapso_mf_death_of_predators(0, 0, -1, 1), 0)
  expect_equal(rcapso_mf_death_of_predators(0.8, 0.3, -1, 1), 0.24, tolerance = 1e-9)
})

test_that("test death of preys", {
  expect_equal(rcapso_mf_death_of_preys(0, 0, use_reg = FALSE, d = 1, e = 0), 0)
  expect_equal(rcapso_mf_death_of_preys(0, 0, use_reg = TRUE,  d = 1, e = 0), 0)
  expect_equal(rcapso_mf_death_of_preys(1, 0.3, use_reg = FALSE, d = 1, e = 1),
               0.7, tolerance = 1.e-9)
  expect_equal(rcapso_mf_death_of_preys(1, 0.3, use_reg = TRUE,
                                        d = 2, e = 0.15),
               0.25, tolerance = 1.e-9)
})
