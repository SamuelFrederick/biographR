test_that("Check prompt helper function", {
  expect_equal(get_prompt(bio = "Test", bio_name = "Test 1",
                          prompt = "This is a test",
                          prompt_fields = NULL,
                          prompt_fields_formats = NULL,
                          prompt_fields_values = NULL),
               "This is a test TEXT: Test")
  expect_equal(get_prompt(bio = NULL, bio_name = "Test 1",
                          prompt = "This is a test",
                          prompt_fields = NULL,
                          prompt_fields_formats = NULL,
                          prompt_fields_values = NULL),
               "This is a test")
  expect_equal(get_prompt(bio = "Test Bio",
                          bio_name = "Test Bioname",
                          prompt = NULL,
                          prompt_fields = c("gender", "college"),
                          prompt_fields_formats = NULL,
                          prompt_fields_values = NULL),
               "Extract biographical information about Test Bioname from the provided text. Return biographical information in a JSON format with ONLY these fields: gender, college. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: Test Bio")
  expect_message(expect_equal(get_prompt(bio = "Test Bio",
                          bio_name = "Test Bioname",
                          prompt = NULL,
                          prompt_fields = NULL,
                          prompt_fields_formats = NULL,
                          prompt_fields_values = NULL),
               "Extract biographical information about Test Bioname from the provided text. Return biographical information in a JSON format with ONLY these fields: birth_date with format {MM}/{DD}/{YYYY}, highest_level_of_education with format {DEGREE}, college with format {SCHOOL} - {DEGREE}, graduate_school with format {SCHOOL} - {DEGREE}, previous_occupation with format {OCCUPATION} - {YEARS}, gender, town_of_birth, state_of_birth, married=[Yes,No]. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: Test Bio"))
})
