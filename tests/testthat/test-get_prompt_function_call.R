test_that("Check get_prompt_function_call output", {
  system_prompt <- "You are a system for outputting biographical information as a dataframe. If biographical information is not in the correct format, reformat it to meet the requirements, and match the biographical text input with the accepted values. Your ONLY output should be the tool call function arguments with the proper formatting and values."
  get_prompt_function_call(bio = "This is a test",
                           bio_name = NULL,
                           prompt_fields = c("gender", "college"),
                           prompt_fields_formats = NULL,
                           prompt_fields_values = NULL,
                           prompt_fields_descriptions = NULL,
                           prompt_fewshot = NULL,
                           openai_model = "gpt-3.5-turbo",
                           openai_temperature = 0,
                           openai_seed = 14892) |>
    expect_equal(list(
      model = "gpt-3.5-turbo",
      messages = list(
        list("role" = "system",
             "content" = system_prompt),
        list("role" = "user",
             "content" = "Return the requested biographical information from this text in a dataframe. Biographical Text: This is a test")
      ),
      tools = list(
        list("type" = "function",
        "function" = list(
          "name" = "make_dataframe_from_biographical_data",
          "description" = "Take biographical data and output in a dataframe. If multiple pieces of information fit into a single argument field, ALL relevant pieces of information for that field are joined by ';'. If no relevant information is present, argument is an empty string ''.",
          "parameters"= list(
            "type" = "object",
            "properties" = list(
              "gender" = list(
                "type" = "string",
                "description" = "Output gender information"
              ),
              "college" = list(
                "type" = "string",
                "description" = "Output college information"
              )
            ),
            "required" = list("gender", "college")
          )
        ))
      ), temperature = 0,
      seed = 14892
    ))
})
