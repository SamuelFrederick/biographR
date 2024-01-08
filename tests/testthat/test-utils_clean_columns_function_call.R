test_that("Check that group_columns() works as expected", {
  group_columns(list(x = 2:3, y = 4:6, z = 15), openai_context_window = 30)|>
    expect_equal(append(list(c("x", "y")),
                      list(c("z"))))
})
test_that("Check get_prompt_clean_columns_fc() output", {
  get_prompt_clean_columns_fc(data = data.frame(education = c("BA", "BA", "BS", "High School", "10th Grade", "PhD", "MD")),
                              column_name = c("education"),
                              fewshot_lgl = F) |>
    expect_equal("Put this information into a dataframe. {\"education\":[\"10th Grade\",\"BA\",\"BS\",\"High School\",\"MD\",\"PhD\"]}")
  get_prompt_clean_columns_fc(data = data.frame(education = c("BA", "BA", "BS"),
                                                birthdate = c("Aug 10, 1979", "10/9/91", "1st of Sept. 1964")),
                              column_name = c("education", "birthdate"),
                              fewshot_lgl = F) |>
    expect_equal("Put this information into a dataframe. {\"education\":[\"BA\",\"BS\"],\"birthdate\":[\"10/9/91\",\"1st of Sept. 1964\",\"Aug 10, 1979\"]}")
})
test_that("Check split_fewshot_fc() output", {
  split_fewshot_fc(column_name = c("education"),
                   prompt_fewshot = list(education = c("BA", "BS"),
                                         education_gpt = c("College", "College", "Graduate School")),
                   prompt_fewshot_n = 1) |>
    expect_error(regexp = "Example inputs and outputs")
  split_fewshot_fc(column_name = c("education", "birthdate"),
                   prompt_fewshot = list(education = c("BA", "BS", "PhD"),
                                         education_gpt = c("College", "College", "Graduate School"),
                                         birthdate = c("August 10, 1989"),
                                         birthdate_gpt = c("08/10/1989")),
                   prompt_fewshot_n = 2) |>
    expect_error(regexp = "prompt_fewshot_n must be less than or equal to")
  split_fewshot_fc(column_name = c("education", "birthdate"),
                   prompt_fewshot = list(education = c( "BS","BA", "PhD"),
                                         education_gpt = c("College", "College", "Graduate School"),
                                         birthdate = c("August 10, 1989", "September 2, 1999"),
                                         birthdate_gpt = c("08/10/1989", "09/02/1999")),
                   prompt_fewshot_n = 1) |>
    expect_equal(append(
      append(
        list(list("role" = "user",
             "content" = "Put this information into a dataframe. {\"education\":[\"BS\",\"BA\",\"PhD\"],\"birthdate\":[\"August 10, 1989\",\"September 2, 1999\"]}")),
        list(list("role" = "assistant",
             "tool_calls" = list(list(
               "id" = "call_qwerty123",
               "function" = list(
                 "name" = "make_dataframe_from_values",
                 "arguments" = "{\"education\":[\"College\",\"College\",\"Graduate School\"],\"birthdate\":[\"08/10/1989\",\"09/02/1999\"]}"
               ),
               "type" = "function"
             ))))
      ),
      list(list("role" = "tool",
           "tool_call_id" = "call_qwerty123",
           "name" = "make_dataframe_from_values",
           "content" = "{\"education\":[\"College\",\"College\",\"Graduate School\"],\"birthdate\":[\"08/10/1989\",\"09/02/1999\"]}"))
    ))

})
test_that("Check get_prompt_clean_columns_fewshot_fc() output", {
  get_prompt_clean_columns_fewshot_fc(column_name = c("education", "birthdate"),
                                      prompt_fewshot = c("test"),
                                      prompt_fewshot_n = 1) |>
    expect_error("prompt_fewshot must be a tibble")
  get_prompt_clean_columns_fewshot_fc(column_name = c("education", "birthdate"),
                                      prompt_fewshot = list(education = c("BA", "BS"),
                                                            education_gpt = c("College", "College")),
                                      prompt_fewshot_n = list(education = 1))|>
    expect_error("prompt_fewshot_n must be an integer")
  get_prompt_clean_columns_fewshot_fc(column_name = c("education", "birthdate"),
                                      prompt_fewshot = list(education = c("BA", "BS"),
                                                            education_gpt = c("College", "College")),
                                      prompt_fewshot_n = 1)|>
    expect_error("Names from column_values")
  get_prompt_clean_columns_fewshot_fc(column_name = c("education", "birthdate"),
                                      prompt_fewshot = list(education = c( "BS","BA", "PhD"),
                                                            education_gpt = c("College", "College", "Graduate School"),
                                                            birthdate = c("August 10, 1989", "September 2, 1999"),
                                                            birthdate_gpt = c("08/10/1989", "09/02/1999")),
                                      prompt_fewshot_n = 1) |>
    expect_equal(append(
      append(
        list(list("role" = "user",
                  "content" = "Put this information into a dataframe. {\"education\":[\"BS\",\"BA\",\"PhD\"],\"birthdate\":[\"August 10, 1989\",\"September 2, 1999\"]}")),
        list(list("role" = "assistant",
                  "tool_calls" = list(list(
                    "id" = "call_qwerty123",
                    "function" = list(
                      "name" = "make_dataframe_from_values",
                      "arguments" = "{\"education\":[\"College\",\"College\",\"Graduate School\"],\"birthdate\":[\"08/10/1989\",\"09/02/1999\"]}"
                    ),
                    "type" = "function"
                  ))))
      ),
      list(list("role" = "tool",
                "tool_call_id" = "call_qwerty123",
                "name" = "make_dataframe_from_values",
                "content" = "{\"education\":[\"College\",\"College\",\"Graduate School\"],\"birthdate\":[\"08/10/1989\",\"09/02/1999\"]}"))
    ))
})
test_that("Check get_prompt_clean_columns_wrapper_fc() output", {
  get_prompt_clean_columns_wrapper_fc(output_vector = list(education = c("High School or Less", "College", "Graduate School")),
                                      output_format = NULL,
                                      output_description = NULL,
                                      column_name = c("education"),
                                      data = data.frame(education = c("BA", "BA", "College", "PhD", "High School diploma")),
                                      prompt_fewshot = NULL,
                                      prompt_fewshot_n = 1) |>
    expect_equal(append(
      append(list(
              list("role" = "system",
                   "content" = "You are a system for outputting user information as a dataframe. If user information is not in the correct format, reformat it to meet the requirements, and match the user input with the accepted values. The arguments do not all need to have the same length. Your ONLY output should be the tool call function arguments with the proper formatting and values.")
             ),
             NULL
    ),
    list(
      list("role" = "user",
           "content" = "Put this information into a dataframe. {\"education\":[\"BA\",\"College\",\"High School diploma\",\"PhD\"]}")
    )))

})
test_that("Check get_tool_clean_columns_fc() output", {
  get_tool_call_clean_columns_fc(output_vector = list(education = c("High School or Less", "College", "Graduate School")),
                                 output_format = NULL,
                                 output_description = NULL,
                                 column_name = c("education"))|>
    expect_equal(list(
      list("type" = "function",
           "function" = list(
             "name" = "make_dataframe_from_values",
             "description" = "Take arrays of values and return those arrays of values in a dataframe format.",
             "parameters" = list(
               "type" = "object",
               "properties" = list("education" = list("type" = "string",
                                                      "description" = "ARRAY containing education for ALL user inputs. Can ONLY contain values in [\"High School or Less\",\"College\",\"Graduate School\"].")),
               "required" = list("education")
             )
           ))
    ))
})
test_that("Check get_gpt_column_function_call() output", {
  get_gpt_column_function_call(data = data.frame(education = c("College", "BA", "PhD", "MA", "MS", "GED")),
                               column_name = c("education"),
                               output_vector = list(education = c(1, 2, 3)),
                               output_format = list(education = 1),
                               output_description = NULL,
                               prompt_fewshot = NULL,
                               prompt_fewshot_n = 1,
                               openai_api_key = Sys.getenv("OPENAI_APIKEY"),
                               openai_model = "gpt-3.5-turbo",
                               openai_temperature = 0,
                               openai_seed = 1239)|>
    expect_warning() |>
    expect_error(regexp = "Column formats")
  get_gpt_column_function_call(data = data.frame(education = c("College", "BA", "PhD", "MA", "MS", "GED")),
                               column_name = c("education"),
                               output_vector = list(education = c(1, 2, 3)),
                               output_format = NULL,
                               output_description = list(education = 1),
                               prompt_fewshot = NULL,
                               prompt_fewshot_n = 1,
                               openai_api_key = Sys.getenv("OPENAI_APIKEY"),
                               openai_model = "gpt-3.5-turbo",
                               openai_temperature = 0,
                               openai_seed = 1239)|>
    expect_warning() |>
    expect_error(regexp = "Column descriptions")
  get_gpt_column_function_call(data = data.frame(education = c("College", "BA", "PhD", "MA", "MS", "GED")),
                               column_name = c("education", "birthdate"),
                               output_vector = list(education = c("College", "High School or less", "Graduate School")),
                               output_format = NULL,
                               output_description = NULL,
                               prompt_fewshot = NULL,
                               prompt_fewshot_n = 1,
                               openai_api_key = Sys.getenv("OPENAI_APIKEY"),
                               openai_model = "gpt-3.5-turbo",
                               openai_temperature = 0,
                               openai_seed = 1239)|>
    expect_error(regexp = "All column names")
  skip_on_cran()
  get_gpt_column_function_call(data = data.frame(education = c("College", "BA", "PhD", "MA", "MS", "GED")),
                               column_name = c("education"),
                               output_vector = list(education = c("College", "High School or less", "Graduate School")),
                               output_format = NULL,
                               output_description = NULL,
                               prompt_fewshot = NULL,
                               prompt_fewshot_n = 1,
                               openai_api_key = Sys.getenv("OPENAI_APIKEY"),
                               openai_model = "gpt-3.5-turbo",
                               openai_temperature = 0,
                               openai_seed = 1239)|>
    expect_equal(tibble::tibble(education = c("College", "BA", "PhD", "MA", "MS", "GED"),
                                education_gpt = c("College", "College", "Graduate School",
                                                  "Graduate School", "Graduate School",
                                                  "High School or less"))) |>
    expect_message()
})
