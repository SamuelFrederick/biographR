test_that("Test input checking for clean_columns", {
  clean_columns() |> expect_error(regexp = "Must input a data argument")
  clean_columns(data = c(1,2,3)) |> expect_error(regexp = "data argument must be a data.frame")
  clean_columns(data = data.frame(age = rnorm(5, 50, 10))) |>
    expect_error(regexp ="Must input at least one of the")
  clean_columns(data = data.frame(age = rnorm(5, 50, 10)),
                column_values = c(1,2,3)) |> expect_error(regexp = "column_values argument must be")
  clean_columns(data = data.frame(age = 50:60),
                column_values = list(education = c("High School or less", "College","Graduate School")))|>
    expect_error(regexp = "Names of column_values")
  clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_values = list(education = c("High School or less", "College", "Graduate School"),
                                     name = c("John Smith", "Sally Smith")),
                openai_model = 1) |>
    expect_warning(regexp = "Some values in column_values")|>
    expect_error(regexp = "Please input a valid OpenAI model")
  clean_columns(data = data.frame(education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_values = list(c("High School or less", "College", "Graduate School"),
                                     c("John Smith", "Sally Smith"))) |>
    expect_error(regexp = "column_values must be a named list")

  clean_columns(data = data.frame(age = rnorm(5, 50, 10)),
                column_formats = c(1,2,3)) |> expect_error(regexp = "column_formats argument must be")
  clean_columns(data = data.frame(age = 50:60),
                column_formats = list(education = c("High School or less", "College","Graduate School")))|>
    expect_error(regexp = "Names of column_formats")
  clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_formats = list(education = c("High School or less", "College", "Graduate School"),
                                     name = c("John Smith", "Sally Smith")),
                openai_model = 1) |>
    expect_warning(regexp = "Some values in column_formats")|>
    expect_error(regexp = "Please input a valid OpenAI model")
  clean_columns(data = data.frame(education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_formats = list(c("High School or less", "College", "Graduate School"),
                                     c("John Smith", "Sally Smith"))) |>
    expect_error(regexp = "column_formats must be a named list")

  clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_values = list(education = c("High School or less", "College", "Graduate School"),
                                     name = c("John Smith", "Sally Smith")),
                openai_temperature = "a") |>
    expect_error(regexp= "Please input a valid OpenAI model temperature")|>
    expect_warning("Some values in column_values")
  clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                column_values = list(education = c("High School or less", "College", "Graduate School"),
                                     name = c("John Smith", "Sally Smith")),
                openai_temperature = 5) |>
    expect_error(regexp= "Please input a valid OpenAI model temperature")|>
    expect_warning("Some values in column_values")

})
test_that("Check whether clean_columns() returns proper output", {
  skip_on_cran()
  tmp <- clean_columns(data = data.frame(age = 50:54,
                                  education = c("BA", "AB", "MA", "JD", "10th grade")),
                        column_values = list(education = c("High School or less", "College", "Graduate School")))
  tmp |>
    expect_no_warning() |>
    expect_no_error() |>
    expect_s3_class(c("tbl_df", "tbl", "data.frame"))
  tmp |>
    expect_type("list")
  tmp |>
    expect_equal(tibble::tibble(age = 50:54,
                                education = c("BA", "AB", "MA", "JD", "10th grade"),
                                education_gpt = c("College", "College", "Graduate School",
                                                  "Graduate School", "High School or less")))

})

