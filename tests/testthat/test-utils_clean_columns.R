test_that("Test input checking for clean_columns", {

  check_inputs_clean_columns() |> expect_error(regexp = "Must input a data argument")
  check_inputs_clean_columns(data = c(1,2,3)) |> expect_error(regexp = "data argument must be a data.frame")
  check_inputs_clean_columns(data = data.frame(age = rnorm(5, 50, 10)),
                             column_values = NULL, column_formats = NULL) |>
    expect_error(regexp ="Must input at least one of the")
  check_inputs_clean_columns(data = data.frame(age = rnorm(5, 50, 10)),
                             column_values = c(1,2,3),
                             column_formats = NULL) |> expect_error(regexp = "column_values argument must be")
  check_inputs_clean_columns(data = data.frame(age = 50:60),
                             column_values = list(education = c("High School or less", "College","Graduate School")),
                             column_formats = NULL)|>
    expect_error(regexp = "Names of column_values")
  check_inputs_clean_columns(data = data.frame(age = 50:54,
                                               education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_values = list(education = c("High School or less", "College", "Graduate School"),
                                                  name = c("John Smith", "Sally Smith")),
                             column_formats = NULL,
                             openai_model = 1) |>
    expect_warning(regexp = "Some values in column_values")|>
    expect_error(regexp = "Please input a valid OpenAI model")
  check_inputs_clean_columns(data = data.frame(education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_values = list(c("High School or less", "College", "Graduate School"),
                                                  c("John Smith", "Sally Smith")),
                             column_formats = NULL) |>
    expect_error(regexp = "column_values must be a named list")

  check_inputs_clean_columns(data = data.frame(age = rnorm(5, 50, 10)),
                             column_formats = c(1,2,3),
                             column_values = NULL) |> expect_error(regexp = "column_formats argument must be")
  check_inputs_clean_columns(data = data.frame(age = 50:60),
                             column_formats = list(education = c("High School or less", "College","Graduate School")),
                             column_values = NULL)|>
    expect_error(regexp = "Names of column_formats")
  check_inputs_clean_columns(data = data.frame(age = 50:54,
                                               education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_values = NULL,
                             column_formats = list(education = c("High School or less", "College", "Graduate School"),
                                                   name = c("John Smith", "Sally Smith")),
                             openai_model = 1) |>
    expect_warning(regexp = "Some values in column_formats")|>
    expect_error(regexp = "Please input a valid OpenAI model")
  check_inputs_clean_columns(data = data.frame(education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_values = NULL,
                             column_formats = list(c("High School or less", "College", "Graduate School"),
                                                   c("John Smith", "Sally Smith"))) |>
    expect_error(regexp = "column_formats must be a named list")

  check_inputs_clean_columns(data = data.frame(age = 50:54,
                                               education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_values = list(education = c("High School or less", "College", "Graduate School"),
                                                  name = c("John Smith", "Sally Smith")),
                             column_formats = NULL,
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = "a") |>
    expect_error(regexp= "Please input a valid OpenAI model temperature")|>
    expect_warning("Some values in column_values")
  check_inputs_clean_columns(data = data.frame(age = 50:54,
                                               education = c("BA", "AB", "MA", "JD", "10th grade")),
                             column_formats = NULL,
                             column_values = list(education = c("High School or less", "College", "Graduate School"),
                                                  name = c("John Smith", "Sally Smith")),
                             openai_model = "gpt-3.5-turbo",
                             openai_temperature = 5) |>
    expect_error(regexp= "Please input a valid OpenAI model temperature")|>
    expect_warning("Some values in column_values")

})
test_that("Test main prompt function for clean_columns()", {
  p1 <- get_prompt_clean_columns(output_vector = c("High School or less", "College", "Graduate School"),
                                 output_format = NULL,
                                 data = data.frame(education = c("BA", "BFA", "9th grade", "MA", "JD")),
                                 column_name = "education")
  expect_true(inherits(p1, "character"))
  p1|>
    expect_type("character")|>
    expect_equal("Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"9th grade\",\"BA\",\"BFA\",\"JD\",\"MA\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON.")
  p2 <- get_prompt_clean_columns(output_format = "{MM}/{DD}/{YYYY}",
                           output_vector = NULL,
                           data = data.frame(birth_date = c("Aug. 13 of 1977", "March 12, 1992", "4/5/81", "9 - 12 - 1940")),
                           column_name ="birth_date")
  expect_true(inherits(p2, "character"))
  p2 |>
    expect_type("character")|>
    expect_equal("Output JSON with ONLY the field birth_date_gpt which should be an array containing each value in birth_date=[\"4/5/81\",\"9 - 12 - 1940\",\"Aug. 13 of 1977\",\"March 12, 1992\"] with format {MM}/{DD}/{YYYY}. Only output JSON.")
  p3 <- get_prompt_clean_columns(output_vector = c("Elephantidae", "Delphinidae", "Canidae"),
                                 output_format = "{FAMILY} - {ORDER} - {CLASS}",
                                 data = data.frame(animal = c("dolphin", "wolf", "elephant", "wolf")),
                                 column_name = "animal")
  expect_true(inherits(p3, "character"))
  p3 |>
    expect_type("character")|>
    expect_equal("Output JSON with ONLY the field animal_gpt which should be an array matching each value in animal=[\"dolphin\",\"elephant\",\"wolf\"] with a value in [\"Elephantidae\",\"Delphinidae\",\"Canidae\"] with format {FAMILY} - {ORDER} - {CLASS}. Only output JSON.")
})
test_that("Test few-shot prompt function for clean_columns()", {
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = NULL,
                                   prompt_fewshot_type = 1,
                                   prompt_fewshot_n = 1)|>
    expect_error(regexp = "prompt_fewshot_type should be either")
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = NULL,
                                   prompt_fewshot_type = "perfect",
                                   prompt_fewshot_n = 1)|>
    expect_error(regexp = "prompt_fewshot_type should be either")
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = c("College", "High School", "PHD"),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_error(regexp = "prompt_fewshot must be a tibble")
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = "a")|>
    expect_error(regexp = "prompt_fewshot_n must be an integer")
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1.2)|>
    expect_error(regexp = "prompt_fewshot_n must be an integer")
  get_prompt_clean_columns_fewshot(output_vector = NULL,
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_error(regexp = "education_gpt must be present in prompt_fewshot")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education1 = c("college", "high school", "phd"),
                                                         education_gpt = c("College", "High School or less", "Graduate School"),
                                                         education2 = c("PHD", "JD", "MA"),
                                                         education5_gpt = c("Graduate School", "College", "College")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_error(regexp = "education must be present in prompt_fewshot")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd"),
                                                         education_gpt = c("College", "High School or less", "Graduate School"),
                                                         education2 = c("PHD", "JD", "MA"),
                                                         education5_gpt = c("Graduate School", "College", "College")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_equal(list(
      list("role" = "user",
           "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"college\",\"high school\",\"phd\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
      list("role" = "assistant",
           "content" = "{\"education_gpt\":[\"College\",\"High School or less\",\"Graduate School\"]}")
    ))
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd"),
                                                         education_gpt = c("College", "High School or less", "Graduate School"),
                                                         education2 = c("PHD", "JD", "MA"),
                                                         education2_gpt = c("Graduate School", "Graduate School", "Graduate School")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_equal(
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"college\",\"high school\",\"phd\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"College\",\"High School or less\",\"Graduate School\"]}")
      ))
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education_gpt = c("College", "High School or less", "Graduate School",
                                                                           "Graduate School", "Graduate School", "College")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 2)|>
    expect_equal(append(
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"college\",\"high school\",\"phd\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"College\",\"High School or less\",\"Graduate School\"]}")
      ),
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"J.D.\",\"MBA\",\"BA\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"Graduate School\",\"Graduate School\",\"College\"]}")
      )
    ))
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education1 = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education_gpt = c("College", "High School or less", "Graduate School",
                                                                           "Graduate School", "Graduate School", "College")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 2)|>
    expect_error(regexp = "must be present in prompt_fewshot")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education1_gpt = c("College", "High School or less", "Graduate School",
                                                                           "Graduate School", "Graduate School", "College")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 2)|>
    expect_error(regexp = "must be present in prompt_fewshot")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education1_gpt = c("College", "High School or less", "Graduate School",
                                                                            "Graduate School", "Graduate School", "College")),
                                   prompt_fewshot_type = "general",
                                   prompt_fewshot_n = 2)|>
    expect_error(regexp = "prompt_fewshot must contain at least one")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education2 = letters[1:6],
                                                         education_gpt = c("College", "High School or less", "Graduate School",
                                                                            "Graduate School", "Graduate School", "College"),
                                                         education3_gpt = letters[4:9]),
                                   prompt_fewshot_type = "general",
                                   prompt_fewshot_n = 2)|>
    expect_message(regexp = "Some example inputs do not have")|>
    expect_message(regexp = "Some example gpt outputs")
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education_gpt = c("College", "High School or less", "Graduate School",
                                                                           "Graduate School", "Graduate School", "College")),
                                   prompt_fewshot_type = "general",
                                   prompt_fewshot_n = 2) |>
    expect_equal(append(
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"college\",\"high school\",\"phd\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"College\",\"High School or less\",\"Graduate School\"]}")
      ),
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"J.D.\",\"MBA\",\"BA\"] with a value in [\"High School or less\",\"College\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"Graduate School\",\"Graduate School\",\"College\"]}")
      )
    ))
  get_prompt_clean_columns_fewshot(output_vector = c("High School or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   prompt_fewshot = list(education = c("college", "high school", "phd", "J.D.", "MBA", "BA"),
                                                         education_gpt = c("College", "High School or less", "Graduate School",
                                                                           "Graduate School", "Graduate School", "College"),
                                                         education_values = c("College", "High School or less", "Graduate School"),
                                                         birthdate = c("Aug. 13 of 1977", "March 12, 1992", "4/5/81", "9 - 12 - 1940"),
                                                         birthdate_gpt = c("08/13/1977", "03/12/1992", "04/05/1981", "09/12/1940"),
                                                         birthdate_format = "{MM}/{DD}/{YYYY}"),
                                   prompt_fewshot_type = "general",
                                   prompt_fewshot_n = 2) |>
    expect_equal(append(
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field birthdate_gpt which should be an array containing each value in birthdate=[\"Aug. 13 of 1977\",\"March 12, 1992\",\"4/5/81\",\"9 - 12 - 1940\"] with format {MM}/{DD}/{YYYY}. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"birthdate_gpt\":[\"08/13/1977\",\"03/12/1992\",\"04/05/1981\",\"09/12/1940\"]}")
      ),
      list(
        list("role" = "user",
             "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"college\",\"high school\",\"phd\",\"J.D.\",\"MBA\",\"BA\"] with a value in [\"College\",\"High School or less\",\"Graduate School\"]. Only output JSON."),
        list("role" = "assistant",
             "content" = "{\"education_gpt\":[\"College\",\"High School or less\",\"Graduate School\",\"Graduate School\",\"Graduate School\",\"College\"]}")
      )
    ))|> expect_message()

})
test_that("Check get_prompt_clean_columns_wrapper() output", {
  system_prompt <- "You are a system that reformats an array of user values according to user specifications and matches user values to a specified array of new categories. Your output should be an array of the same length as the input array containing only the reformatted input values and newly matched categories."
  get_prompt_clean_columns_wrapper(output_vector = c("HS or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   data = data.frame(education = c("BA", "GED", "HS Diploma"),
                                                     birthdate = c("August 12, 1994", "March 21, 1954",
                                                                   "8/9/1983")),
                                   prompt_fewshot = NULL,
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_equal(list(
      list("role" = "system",
           "content" = system_prompt),
      list("role" = "user",
           "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"BA\",\"GED\",\"HS Diploma\"] with a value in [\"HS or less\",\"College\",\"Graduate School\"]. Only output JSON.")
    ))
  get_prompt_clean_columns_wrapper(output_vector = c("HS or less", "College", "Graduate School"),
                                   output_format = NULL,
                                   column_name = "education",
                                   data = data.frame(education = c("BA", "GED", "HS Diploma"),
                                                     birthdate = c("August 12, 1994", "March 21, 1954",
                                                                   "8/9/1983")),
                                   prompt_fewshot = list(married = c("Yes", "Yes", "No"),
                                                         married_gpt = c("Yes", "Yes", "No")),
                                   prompt_fewshot_type = "specific",
                                   prompt_fewshot_n = 1)|>
    expect_equal(list(
      list("role" = "system",
           "content" = system_prompt),
      list("role" = "user",
           "content" = "Output JSON with ONLY the field education_gpt which should be an array matching each value in education=[\"BA\",\"GED\",\"HS Diploma\"] with a value in [\"HS or less\",\"College\",\"Graduate School\"]. Only output JSON.")
    ))|>
    expect_message()
})
test_that("Check get_gpt_column() output", {
  skip_on_cran()
  get_gpt_column(data = data.frame(education = c("BA", "HS Diploma", "GED"),
                                   birthdate = c("August 12, 1994", "March 21, 1954",
                                                 "8/9/1983")),
                 column_name = "education",
                 output_vector = c("High School or less", "College", "Graduate School"),
                 output_format = NULL,
                 prompt_fewshot = list(education = c("BA", "MA", "9th Grade"),
                                       education_gpt = c("College", "Graduate School", "High School or less")),
                 prompt_fewshot_type = "specific",
                 prompt_fewshot_n = 1,
                 openai_model = "gpt-3.5-turbo",
                 openai_api_key = Sys.getenv("OPENAI_APIKEY"),
                 openai_temperature = 0,
                 openai_seed = 123) |>
    expect_equal(tibble::tibble(education = c("BA", "HS Diploma", "GED"),
                                birthdate = c("August 12, 1994", "March 21, 1954",
                                              "8/9/1983"),
                                education_gpt = c("College", "High School or less", "High School or less")))|>
    expect_message()
})
