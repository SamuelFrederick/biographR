test_that("Check get_prompt_fewshot() output", {
  get_prompt_fewshot(prompt_fewshot = list(x = 1:3, y = 5:7),
                     prompt = NULL,
                     prompt_fields = NULL,
                     prompt_fields_formats = NULL,
                     prompt_fields_values = NULL)|> expect_error(regexp = "prompt_fewshot must be a tibble or data.frame")
  get_prompt_fewshot(prompt_fewshot = data.frame(college = "B.A."),
                     prompt = NULL,
                     prompt_fields = NULL,
                     prompt_fields_formats = NULL,
                     prompt_fields_values = NULL) |> expect_error(regexp = "Please input prompt_fewshot with a bio column")
  get_prompt_fewshot(prompt_fewshot = data.frame(college = "B.A.", bio = "This is a test"),
                     prompt = NULL,
                     prompt_fields = c("college"),
                     prompt_fields_formats = NULL,
                     prompt_fields_values = NULL) |>
    expect_warning(regexp = "No bio_name in prompt_fewshot.")
  get_prompt_fewshot(prompt_fewshot = data.frame(college = "B.A.", bio = "This is a test",
                                                 bio_name = "Test Bioname"),
                     prompt = NULL,
                     prompt_fields = c("college", "previous_occupation"),
                     prompt_fields_formats = NULL,
                     prompt_fields_values = NULL) |>
    expect_error(regexp = "All fields in prompt_fields should be present")
  get_prompt_fewshot(prompt_fewshot = data.frame(college = "B.A.", bio = "This is a test",
                                                 bio_name = "Test Bioname"),
                     prompt = NULL,
                     prompt_fields = NULL,
                     prompt_fields_formats = NULL,
                     prompt_fields_values = NULL) |>
    expect_error(regexp = "All fields in prompt_fields should be present")
  get_prompt_fewshot(prompt_fewshot = data.frame(college = "B.A.", bio = "Test Bioname got his BA from Nowhere University.",
                                                 bio_name = "Test Bioname"),
                     prompt = NULL,
                     prompt_fields = c("college"),
                     prompt_fields_formats = list(college = "{DEGREE}"),
                     prompt_fields_values = NULL) |>
    expect_equal(list(
      list("role" = "user",
           "content" = "Extract biographical information about Test Bioname from the provided text. Return biographical information in a JSON format with ONLY these fields: college with format {DEGREE}. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: Test Bioname got his BA from Nowhere University."),
      list("role" = "assistant",
           "content" = "{\"college\":\"B.A.\"}")
    ))
  get_prompt_fewshot(prompt_fewshot = data.frame(college = c("B.A.", "Ph.D."),
                                                 gender = c("Male", "Female"),
                                                 bio = c("Test Bioname got his BA from Nowhere University.",
                                                         "Sally Smith got her Ph.D. from Hogwarts"),
                                                 bio_name = c("Test Bioname", "Sally Smith")),
                     prompt = NULL,
                     prompt_fields = c("college", "gender"),
                     prompt_fields_formats = list(college = "{DEGREE}"),
                     prompt_fields_values = NULL) |>
    expect_equal(list(
      list("role" = "user",
           "content" = "Extract biographical information about Test Bioname from the provided text. Return biographical information in a JSON format with ONLY these fields: college with format {DEGREE}, gender. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: Test Bioname got his BA from Nowhere University."),
      list("role" = "assistant",
           "content" = "{\"college\":\"B.A.\",\"gender\":\"Male\"}"),
      list("role" = "user",
           "content" = "Extract biographical information about Sally Smith from the provided text. Return biographical information in a JSON format with ONLY these fields: college with format {DEGREE}, gender. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: Sally Smith got her Ph.D. from Hogwarts"),
      list("role" = "assistant",
           "content" = "{\"college\":\"Ph.D.\",\"gender\":\"Female\"}")
    ))

})
