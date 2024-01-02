test_that("Make sure input checking works properly.", {
  expect_error(get_bio(), regexp = "input a bio as a string")
  expect_error(get_bio(bio = NULL), regexp = "Please input a bio as a string")
  expect_error(get_bio(bio = "Test", bio_name = "Test", prompt_fields = 1),
               regexp = "prompt_fields argument must be NULL or a character")
  expect_error(get_bio(bio = "Test", bio_name = "Test", prompt_fields_format = 1),
               regexp = "prompt_fields_format argument must be NULL, a character")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = c("T1", "T2", "T3"))|>
    expect_error(regexp = "NULL or a list of character vectors")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_format = list(college = "{SCHOOL} - {YEAR} - {DEGREE}",
                                      occupation = "{OCCUPATION} - {YEAR}"),
          prompt_fields_values = as.list(c("T1", "T2", "T3"))) |>
    expect_error(regexp = "unnamed, it must be the same") |>
    expect_warning(regexp = "Some names of prompt_fields_format are not in")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_format = c("SCHOOL", "DEGREE", "YEAR"),
          prompt_fields_values = list(college = c("Yes", "No"),
                                      military_background = c("Yes", "No")))|>
    expect_error(regexp = "unnamed, it must be the same")|>
    expect_warning(regexp = "Some names of prompt_fields_values are not in")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = list(c("T1", "T2", "T3")))|>
    expect_error(regexp = "unnamed, it must be the same")

  get_bio(bio = "Test", openai_model = 1) |>
    expect_error(regexp = "Please input a valid OpenAI model as a string")|>
    expect_warning(regexp = "Consider inputting a name for better results")
  get_bio(bio = "Test", bio_name = "Test", prompt = "This is a test",
          openai_model = 1) |>
    expect_error(regexp = "Please input a valid OpenAI model as a string")|>
    expect_warning(regexp = "bio_name argument will be ignored")

  get_bio(bio = "Test", bio_name = "Test", openai_temperature = 7) |>
    expect_error(regexp = "valid OpenAI model temperature")
  get_bio(bio = "Test", bio_name = "Test", prompt = "This is a test",
          prompt_fields = c("college", "highest_level_of_education"))|>
    expect_error(regexp = "Custom prompt should include desired fields, values, and formats")
  get_bio(bio = "Test", bio_name = "Test", prompt = "This is a test",
          prompt_fields_format = c("college", "highest_level_of_education"))|>
    expect_error(regexp = "Custom prompt should include desired fields, values, and formats")
  get_bio(bio = "Test", bio_name = "Test", prompt = "This is a test",
          prompt_fields_values = c("college", "highest_level_of_education"))|>
    expect_error(regexp = "Custom prompt should include desired fields, values, and formats")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = c("T1", "T2", "T3"))|>
    expect_error(regexp = "The prompt_fields_values argument must be")

  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college"),
          post_process_fields = "b") |>
    expect_error(regexp = "Please input TRUE or FALSE")

})
test_that("Make sure get_bio() returns proper output", {
  skip_on_cran()
  bio_res <- get_bio(bio = "John Smith graduated from Nowhere College with a B.A. in 1962. He then went on to receive a Ph.D. from Nonexistent University. At the same time, his wife, Sally Smith, was earning her M.D. from Invisible University.",
          bio_name = "John Smith",
          prompt_fields = c("college", "graduate_school",
                            "highest_level_of_education", "gender",
                            "married"),
          prompt_fields_format = list(college = "{SCHOOL} - {DEGREE}",
                                      graduate_school = "{SCHOOL} - {DEGREE}",
                                      highest_level_of_education = "{DEGREE}"),
          prompt_fields_values = list(married = c("Yes", "No")),
          openai_model = "gpt-4")
  bio_res |>
    expect_no_warning() |>
    expect_no_error()|>
    expect_s3_class(c("tbl_df", "tbl", "data.frame"))
  bio_res |>
    expect_type("list")
  expect_equal(bio_res,
               tibble::tibble(college = "Nowhere College - B.A.", graduate_school = "Nonexistent University - Ph.D.",
                      highest_level_of_education = "Ph.D.", gender = "Male",
                      married = "Yes"))

})

