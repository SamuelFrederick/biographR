#' Clean a series of columns using ChatGPT by mapping data into new categories and formats
#'
#' @description
#' Maps old data in names of column_values to new categories and formats using ChatGPT.
#' clean_columns() returns data with new columns corresponding to matched and reformatted
#' values. This can be helpful for processing messy or unstructured text data
#' in a column (e.g., open-ended survey responses, names, etc.). clean_columns()
#' is particularly helpful for post-processing the output of [get_bio()].
#'
#' clean_columns() uses the standard chat completion to reformat columns. It makes separate API calls for each column.
#' clean_columns_function_call() uses function calling to reformat columns. It tries to complete as many columns as possible in a single API call.
#'
#' @param data The data to be processed
#' @param column_values A named list with column names from data as names and values as vectors with the desired categories for the corresponding column
#' @param column_formats A named list with column names from data as names and values as strings with the desired format for the corresponding column
#' @param column_descriptions Only for clean_columns_function_call(). A named list with column names from data as names and values as strings with the desired description for the column
#' @param prompt_fewshot A data.frame, tibble, or named list containing example inputs and example outputs (names from input example with suffix "_gpt")
#' Example: list(education = c("J.D.", "BA", "GED", "Did not graduate high school"),
#'               education_gpt = c("Graduate School", "College, "High School or less", "High School or less"))
#' @param prompt_fewshot_n An integer or named list of integers (with names from example inputs in prompt_fewshot) giving the number of segments to divide each example input into. For example, prompt_fewshot_n=2 would divide inputted example vectors into two separate example prompts and outputs.
#' Note: for clean_columns_function_call(), this must be an integer.
#' @param prompt_fewshot_type Only for clean_columns(). A string, one of "specific" or "general", defaults to "specific".
#' If type is "specific", prompt_fewshot must have column names from data, and few-shot examples should correspond to the specific columns from the data.
#' If type is "general", examples in prompt_fewshot will be reused for each column. It is recommended that the few-shot examples in prompt_fewshot include example values or formats as well with suffix _values and _formats.
#' @param openai_api_key API key for OpenAI, a string. If this is NULL, clean_columns() searches .Renviron for API key.
#' @param openai_model ChatGPT model to use, defaults to "chatgpt-3.5-turbo"
#' @param openai_temperature Specifies the amount of randomness in ChatGPT, a number between 0 and 2 with more randomness for higher numbers, defaults to 0
#' @param openai_seed An integer, specifies a random seed for ChatGPT (this is in the development stage at OpenAI, so it might not work perfectly)
#' @param openai_context_window Only for clean_columns_function_call(). An integer, defaults to 4,096, specifies the context window for the ChatGPT model in use.
#' This is used to determine whether to split the columns to be cleaned into several portions. Note: this is a rough approximation of whether the prompt is too long. It is best to split your data into parts if needed or to use larger GPT models.
#'
#' @return Data with new columns for each entry in column_values containing new mappings
#' @export
#'
#' @examples
#' df <- data.frame(age = rnorm(4, 50, 10),
#'                  education = c("BA", "B.A.", "High School", "MBA"),
#'                  name = c("Wardell Stephen Curry II", "Michael J Jordan",
#'                  "James, LEBRON", "Shaq"))
#' clean_columns(data = df,
#'               column_values = list(education = c("High School", "College",
#'                                                  "Graduate School"),
#'                                    name = c("Steph Curry", "Michael Jordan",
#'                                             "LeBron James", "Shaquille O'Neal")))
#'
#' clean_columns(data = data.frame(birthday = c("08-13-1923",
#'                                              "05/15/1976",
#'                                              "March 13, 1998",
#'                                              "19th of March in 1994")),
#'               column_formats = list(birthday = "{MM}/{DD}/{YYYY}"))
#' clean_columns_function_call(data = df,
#'                             column_values = list(education = c("High School or less",
#'                                                                "College",
#'                                                                "Graduate School")))
clean_columns <- function(data, column_values,
                          column_formats,
                          prompt_fewshot = NULL,
                          prompt_fewshot_type = "specific",
                          prompt_fewshot_n = 1,
                          openai_api_key = NULL,
                          openai_model = "gpt-3.5-turbo",
                          openai_temperature = 0,
                          openai_seed = NULL) {

  if(missing(column_values)) column_values <- NULL
  if(missing(column_formats)) column_formats <- NULL

  if(!is.null(prompt_fewshot)&grepl("general", prompt_fewshot_type, ignore.case = T)&
     !any(grepl("_(values|format)$", names(prompt_fewshot)))) {
    if(length(union(names(column_values), names(column_formats)))>1){
      ch <- utils::menu(c("Yes", "No"), title = "When prompt_fewshot_type='general', it is recommended that you provide values (with suffix _values) or formats (with suffix _format) specific to your examples as separate entries in prompt_fewshot. Would you like to ignore this warning and proceed anyway?")
      if(ch!=1) stop("Please add specific values or formats for your prompt_fewshot data.", call. = F)
    }
  }

  # Check user inputs
  openai_api_key <- check_inputs_clean_columns(data = data,
                                               column_values = column_values,
                                               column_formats = column_formats,
                                               openai_api_key = openai_api_key,
                                               openai_model = openai_model,
                                               openai_temperature = openai_temperature,
                                               openai_seed = openai_seed)

  # Coerce data to data.frame
  data <- as.data.frame(data)

  # Iterate over every column in column_values and column_formats and add processed column to data
  for(i in union(names(column_values), names(column_formats))) {

    data <- as.data.frame(get_gpt_column(data = data,
                           column_name = i,
                           output_vector = column_values[[i]],
                           output_format = column_formats[[i]],
                           prompt_fewshot = prompt_fewshot,
                           prompt_fewshot_type = prompt_fewshot_type,
                           prompt_fewshot_n = prompt_fewshot_n,
                           openai_api_key = openai_api_key,
                           openai_model = openai_model,
                           openai_temperature = openai_temperature,
                           openai_seed = openai_seed))
  }

  # Return data with new, processed columns
  return(tibble::tibble(data))
}
