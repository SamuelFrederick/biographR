test_that("Make sure input checking works properly.", {
  expect_error(get_bio())
  expect_error(get_bio(bio = NULL), regexp = "Please input a bio as a string")
  expect_error(get_bio(bio = "Test", bio_name = "Test", prompt_fields = 1),
               regexp = "prompt_fields argument must be NULL or a character")
  expect_error(get_bio(bio = "Test", bio_name = "Test", prompt_fields_format = 1),
               regexp = "prompt_fields_format argument must be NULL, a character")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = c("T1", "T2", "T3"))|>
    expect_error(regexp = "NULL or a list of character vectors")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = as.list(c("T1", "T2", "T3")))|>
    expect_error(regexp = "unnamed, it must be the same")
  get_bio(bio = "Test", bio_name = "Test", prompt_fields = c("college", "highest_level_of_education"),
          prompt_fields_values = list(c("T1", "T2", "T3")))|>
    expect_error(regexp = "unnamed, it must be the same")
  get_bio(bio = "Test", openai_model = 1) |>
    expect_error(regexp = "Please input a valid OpenAI model as a string")|>
    expect_warning(regexp = "Consider inputting a name for better results")
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


})

