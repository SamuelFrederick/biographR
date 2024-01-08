test_that("Check whether clean_columns() returns proper output", {
  skip_on_cran()
  expect_message(tmp <- clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                        column_values = list(education = c("High School or less", "College", "Graduate School")),
                       openai_seed = 123))
  tmp |>
    expect_no_warning() |>
    expect_no_error()
  tmp |>
    expect_s3_class(c("tbl_df", "tbl", "data.frame"))
  tmp |>
    expect_type("list")
  tmp |>
    expect_equal(tibble::tibble(age = 50:54,
                                education = c("BA", "AB", "MA", "JD", "10th grade"),
                                education_gpt = c("College", "College", "Graduate School",
                                                  "Graduate School", "High School or less")))

})

