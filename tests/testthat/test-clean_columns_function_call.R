test_that("Test input checking for clean_columns_function_call()", {
  clean_columns_function_call(column_values = NULL,
                             column_formats = NULL,
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "Must input")
  clean_columns_function_call(data = list(x = 1:3),
                             column_values = NULL,
                             column_formats = NULL,
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "data argument must be a data.frame")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = NULL,
                             column_formats = NULL,
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "Must input at least one of the")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = data.frame(x = 1:3),
                             column_formats = NULL,
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "column_values argument must be a list")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(y = 1:3),
                             column_formats = NULL,
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "Names of column_values")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x = 1:3, y = 4:6),
                             column_formats = data.frame(x = 5:7),
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_warning()|>
    expect_error(regexp = "column_formats argument must be")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x = 1:3, y = 4:6),
                             column_formats = list(z = 5:7),
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_warning()|>
    expect_error(regexp = "Names of column_formats")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list( 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = NULL,
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error(regexp = "column_values must be a named list")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7, y = letters[4:6]),
                             column_descriptions = data.frame(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_warning()|>
    expect_error("column_descriptions argument must be")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7, y = letters[4:6]),
                             column_descriptions = list(y = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_warning()|>
    expect_error("Names of column_descriptions")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = 1,
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error("Please input a valid OpenAI")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error("column_descriptions must be a named")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = "a",
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error("Please input a valid")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 3,
                             openai_seed = 1,
                             openai_context_window = 4000)|>
    expect_error("Please input a valid")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1.2,
                             openai_context_window = 4000)|>
    expect_error("Please input an integer")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = "a",
                             openai_context_window = 4000)|>
    expect_error("Please input an integer")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1.2,
                             openai_context_window = 4.2)|>
    expect_error("Please input an integer")
  clean_columns_function_call(data = data.frame(x = 1:3),
                             column_values = list(x= 1:3),
                             column_formats = list(x = 5:7),
                             column_descriptions = list(x = 1:3),
                             prompt_fewshot = NULL,
                             prompt_fewshot_n = 1,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 0,
                             openai_seed = 1.2,
                             openai_context_window = "a")|>
    expect_error("Please input an integer")
})
test_that("Check clean_columns_function_call() output", {
  skip_on_cran()
  clean_columns_function_call(data = data.frame(education = c("BA", "B.A.", "M.Phil.", "Master's", "GED", "Diploma"),
                                               birthdate = c("9/13/1922", "10/12/1945", "June 14, 1976",
                                                             "16th of July 1988", "Aug. 21, 1956", "May 12, '57")),
                             column_values = list(education = c("High School or less", "College", "Graduate School")),
                             column_formats = NULL,
                             column_descriptions = NULL,
                             openai_seed = 90192)|>
    expect_equal(tibble::tibble(education = c("BA", "B.A.", "M.Phil.", "Master's", "GED", "Diploma"),
                                birthdate = c("9/13/1922", "10/12/1945", "June 14, 1976",
                                              "16th of July 1988", "Aug. 21, 1956", "May 12, '57"),
                                education_gpt = c("College", "College", "Graduate School",
                                                  "Graduate School", "High School or less",
                                                  "High School or less")))|>
    expect_message()
  clean_columns_function_call(data = data.frame(education = c("BA", "B.A.", "M.Phil.", "Master's", "GED", "Diploma"),
                                               birthdate = c("9/13/1922", "10/12/1945", "June 14, 1976",
                                                             "16th of July 1988", "Aug. 21, 1956", "May 12, '57")),
                             column_values = list(education = c("High School or less", "College", "Graduate School")),
                             column_formats = list(birthdate = "{MM}/{DD}/{YYYY}"),
                             column_descriptions = NULL,
                             openai_seed = 123)|>
    expect_equal(tibble::tibble(education = c("BA", "B.A.", "M.Phil.", "Master's", "GED", "Diploma"),
                                birthdate = c("9/13/1922", "10/12/1945", "June 14, 1976",
                                              "16th of July 1988", "Aug. 21, 1956", "May 12, '57"),
                                education_gpt = c("College", "College", "Graduate School",
                                                  "Graduate School", "High School or less",
                                                  "High School or less"),
                                birthdate_gpt = c("09/13/1922", "10/12/1945", "06/14/1976",
                                                  "07/16/1988", "08/21/1956", "05/12/1957")))|>
    expect_message()
})
