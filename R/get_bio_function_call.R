#' @rdname get_bio
#' @export
get_bio_function_call <- function(bio, bio_name = NULL,
                    openai_api_key = NULL,
                    openai_model = "gpt-3.5-turbo",
                    openai_temperature = 0,
                    openai_seed = NULL,
                    prompt_fields = NULL,
                    prompt_fields_formats = NULL,
                    prompt_fields_values = NULL,
                    prompt_fields_descriptions = NULL,
                    prompt_fewshot = NULL) {

  if(missing(bio)) bio <- NULL
  if(!is.null(prompt_fewshot)) prompt_fewshot <- get_prompt_fewshot_function_call(prompt_fewshot = prompt_fewshot,
                                                                                  prompt_fields = prompt_fields)


  # Check that inputs are valid and return OpenAI API key
  openai_api_key <- check_inputs(bio = bio, bio_name = bio_name,
                                 openai_api_key = openai_api_key,
                                 openai_model = openai_model,
                                 openai_temperature = openai_temperature,
                                 openai_seed = openai_seed,
                                 prompt = NULL,
                                 prompt_fields = prompt_fields,
                                 prompt_fields_formats = prompt_fields_formats,
                                 prompt_fields_values = prompt_fields_values,
                                 prompt_fields_descriptions = prompt_fields_descriptions)
  if(!is.null(prompt_fields_values)&
     all(is.null(names(prompt_fields_values)))) names(prompt_fields_values) <- prompt_fields
  if(!is.null(prompt_fields_formats)&
     all(is.null(names(prompt_fields_formats)))) names(prompt_fields_formats) <- prompt_fields
  if(!is.null(prompt_fields_descriptions)&
     all(is.null(names(prompt_fields_descriptions)))) names(prompt_fields_descriptions) <- prompt_fields

  # Generate final prompt from user inputs
  final_prompt <- get_prompt_function_call(bio = bio,
                                           bio_name = bio_name,
                                           prompt_fields = prompt_fields,
                                           prompt_fields_formats = prompt_fields_formats,
                                           prompt_fields_values = prompt_fields_values,
                                           prompt_fields_descriptions = prompt_fields_descriptions,
                                           prompt_fewshot = prompt_fewshot,
                                           openai_model = openai_model,
                                           openai_temperature = openai_temperature,
                                           openai_seed = openai_seed)

  # ChatGPT API call with user inputs
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(final_prompt) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  message(sprintf("Input Tokens: %s\nOutput Tokens: %s\nTotal Tokens: %s",
                  resp$usage$prompt_tokens,
                  resp$usage$completion_tokens,
                  resp$usage$total_tokens))

  # Return tibble if default prompt
  return(tibble::as_tibble(jsonlite::fromJSON(resp$choices[[1]]$message$tool_calls[[1]]$'function'$arguments)))

}
