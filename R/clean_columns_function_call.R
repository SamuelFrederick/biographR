#' @rdname clean_columns
#' @export
clean_columns_function_call <- function(data,
                                       column_values,
                                       column_formats,
                                       column_descriptions = NULL,
                                       prompt_fewshot = NULL,
                                       prompt_fewshot_n = 1,
                                       openai_api_key = NULL,
                                       openai_model = "gpt-3.5-turbo",
                                       openai_temperature = 0,
                                       openai_seed = NULL,
                                       openai_context_window = 4096) {

  if(missing(column_values)) column_values <- NULL
  if(missing(column_formats)) column_formats <- NULL

  prompt_fields <- union(names(column_values), union(names(column_formats), names(column_descriptions)))

  # Check user inputs
  openai_api_key <- check_inputs_clean_columns(data = data,
                                               column_values = column_values,
                                               column_formats = column_formats,
                                               column_descriptions = column_descriptions,
                                               openai_api_key = openai_api_key,
                                               openai_model = openai_model,
                                               openai_temperature = openai_temperature,
                                               openai_seed = openai_seed,
                                               openai_context_window = openai_context_window)

  # Each OpenAI token is about 4 characters; used to determine groupings of functions
  openai_context_window <- openai_context_window*4

  # Check length of user input
  prompt_fields_len <- lapply(stats::setNames(1:length(prompt_fields), prompt_fields),
                              function(x) sum(nchar(unique(data[,prompt_fields[x]]))) + nchar(prompt_fields[x]))

  # Check if length of API prompt and output is likely to be greater than the GPT model's context window using a rough proxy
  if(sum(unlist(prompt_fields_len))>(openai_context_window/1.34)){
    prompt_fields <- group_columns(prompt_fields_len, openai_context_window)
    if(any(lapply(prompt_fields, function(x) sum(unlist(prompt_fields_len[x]))>openai_context_window))) stop("It looks like your input may produce prompts longer than OpenAI's context window. It is recommended that either use a different GPT model or that you split your data into different pieces and input each piece separately.", call. = F)
    ch <- utils::menu(choices = c("Yes", "No"),
                      title = sprintf("It looks like your desired task might produce prompts longer than OpenAI's context window. If you choose to proceed, your columns will be split into %s different function calls. Would you like to proceed?", length(prompt_fields)))
    if(ch!=1) stop("Please split your task into smaller parts or call a different GPT model.", call. = F)
    for(i in 1:length(prompt_fields)) {
      data <- as.data.frame(get_gpt_column_function_call(data = data,
                                                         column_name = prompt_fields[[i]],
                                                         output_vector = column_values[prompt_fields[[i]]],
                                                         output_format = column_formats[prompt_fields[[i]]],
                                                         prompt_fewshot = prompt_fewshot,
                                                         prompt_fewshot_n = prompt_fewshot_n,
                                                         openai_api_key = openai_api_key,
                                                         openai_model = openai_model,
                                                         openai_temperature = openai_temperature,
                                                         openai_seed = openai_seed))
    }
  }else{
    data <- get_gpt_column_function_call(data = data,
                                         column_name = prompt_fields,
                                         output_vector = column_values,
                                         output_format = column_formats,
                                         output_description = column_descriptions,
                                         prompt_fewshot = prompt_fewshot,
                                         prompt_fewshot_n = prompt_fewshot_n,
                                         openai_api_key = openai_api_key,
                                         openai_model = openai_model,
                                         openai_temperature = openai_temperature,
                                         openai_seed = openai_seed)
  }

  # Return data with new, processed columns
  return(data)
}
