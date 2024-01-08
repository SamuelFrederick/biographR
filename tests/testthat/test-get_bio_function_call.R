test_that("Check inputs for get_bio_function_call()", {
  get_bio_function_call(bio = NULL) |>
    expect_error(regexp = "input a bio as a string")
  get_bio_function_call(bio = "Test", prompt_fields = 1:3)|>
    expect_warning(regexp = "not inputted a name") |>
    expect_error(regexp = "prompt_fields argument must be NULL")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_formats = c(1,2,3))|>
    expect_error(regexp = "prompt_fields_formats argument must be NULL")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_formats = list(college = 1))|>
    expect_error(regexp = "prompt_fields_formats argument must be NULL")

  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_values = c(1,2,3))|>
    expect_error(regexp = "prompt_fields_values argument must be NULL")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_values = list(college = 1))|>
    expect_error(regexp = "prompt_fields_values argument must be NULL")

  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_descriptions = c(1,2,3))|>
    expect_error(regexp = "prompt_fields_descriptions argument must be NULL")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_descriptions = list(college = 1))|>
    expect_error(regexp = "prompt_fields_descriptions argument must be NULL")


  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_formats = list(college = "Degree",
                                                     gender = "Gender"),
                        openai_model = 1)|>
    expect_warning(regexp = "Some names of prompt_fields_formats") |>
    expect_error(regexp = "Please input a valid OpenAI")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_values = list(college = "Degree",
                                                     gender = "Gender"),
                        openai_model = 1)|>
    expect_warning(regexp = "Some names of prompt_fields_values") |>
    expect_error(regexp = "Please input a valid OpenAI")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_descriptions = list(college = "Degree",
                                                    gender = "Gender"),
                        openai_model = 1)|>
    expect_warning(regexp = "Some names of prompt_fields_descriptions") |>
    expect_error(regexp = "Please input a valid OpenAI")

  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_formats = list("Degree", "Gender"),
                        openai_model = 1)|>
    expect_error(regexp = "If prompt_fields_formats is unnamed")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_values = list("Degree","Gender"),
                        openai_model = 1)|>
    expect_error(regexp = "If prompt_fields_values is unnamed")
  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_descriptions = list("Degree", "Gender"),
                        openai_model = 1)|>
    expect_error(regexp = "If prompt_fields_descriptions is unnamed")

  get_bio_function_call(bio = "Test", bio_name = "Test Bioname",
                        prompt_fields = c("college"),
                        prompt_fields_descriptions = list("Degree"),
                        openai_temperature = "a")|>
    expect_error(regexp = "Please input a valid OpenAI model temperature")

})
test_that("Check the output of a function call", {
  skip_on_cran()
  expect_message(out <- get_bio_function_call(bio = "John Smith went to Nowhere University where he earned his bachelor's degree. He also met his wife, Sally Smith, there.",
                               bio_name = "John Smith",
                               prompt_fields = c("gender", "undergraduate_education", "married"),
                               prompt_fields_formats = list(undergraduate_education = "{SCHOOL} - {DEGREE}"),
                               prompt_fields_values = list(married = c("Yes", "No")),
                               openai_seed = 9992))
  out|>
    expect_s3_class(c("tbl", "tbl_df", "data.frame"))
  out |>
    expect_type("list")
  out |>
    expect_equal(tibble::tibble(gender = "",
                                undergraduate_education = "Nowhere University - bachelor's degree",
                                married = "Yes"))
})
