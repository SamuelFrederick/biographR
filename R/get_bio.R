#' Get structured biographical data from unstructured text
#'
#' @param bio The bio to be processed, a string
#' @param bio_name The name of the individual whose biographical information is desired, a string
#' @param openai_api_key API key for OpenAI, a string. If this is NULL, get_bio() searches .Renviron for API key.
#' @param openai_model ChatGPT model to use, defaults to "chatgpt-3.5-turbo"
#' @param openai_temperature Specifies the amount of randomness in ChatGPT, a number between 0 and 2 with more randomness for higher numbers, defaults to 0
#' @param prompt If desired, input a custom prompt. This overrides the default prompt and should include any desired prompt fields, formats, and values.
#' @param prompt_fields A character vector of desired biographical output fields (e.g., "college", "graduate_school")
#' @param prompt_fields_format A named list or character vector of desired formats for output fields (e.g., "\{SCHOOL\} - \{YEAR\} - \{DEGREE\}"). If a character vector, it must be the same length as prompt_fields with unformatted fields taking the value of NA. Names must be present in prompt_fields.
#' @param prompt_fields_values A list of character vectors of desired output values for each prompt field, must be same length as prompt_fields if unnamed. Named lists do not have to be the same length as prompt_fields, but names must be present in prompt_fields.
#' @param post_process_fields TRUE or FALSE, defaults FALSE. Should get_bio() use ChatGPT to map outputted values for each field to specified values?
#'
#' @return A tibble containing desired biographical information or unprocessed API output from custom prompt
#' @export
#'
#' @examples
#' get_bio(bio = "MCCARTHY, KEVIN, a Representative from California;
#'               born in Bakersfield, Kern County, Calif., January 26,
#'               1965; graduated from Bakersfield High School,
#'               Bakersfield, Calif., 1983; attended Bakersfield College,
#'               Bakersfield. Calif., 1983-1986; B.S., California State
#'               University, Bakersfield, Calif., 1989; M.B.A., California
#'              State University, Bakersfield, Calif., 1994; staff,
#'              United States Representative William Thomas of California,
#'              1987-2002; member of the California state assembly,
#'              2002-2007, minority leader, 2004-2006; elected as a
#'              Republican to the One Hundred Tenth and to the eight
#'              succeeding Congresses (January 3, 2007-present); majority
#'              whip (One Hundred Twelfth and One Hundred Thirteenth
#'              Congresses); majority leader (One Hundred Thirteenth
#'              through One Hundred Fifteenth Congresses); minority
#'              leader (One Hundred Sixteenth and One Hundred Seventeenth
#'              Congress); Speaker of the House (One Hundred Eighteenth
#'              Congress).",
#'       bio_name = "Kevin McCarthy")
get_bio <- function(bio, bio_name = NULL,
                    openai_api_key = NULL,
                    openai_model = "gpt-3.5-turbo",
                    openai_temperature = 0,
                    prompt = NULL,
                    prompt_fields = NULL,
                    prompt_fields_format = NULL,
                    prompt_fields_values = NULL,
                    post_process_fields = FALSE) {

  if(missing(bio)) bio <- NULL

  # Check that inputs are valid and return OpenAI API key
  openai_api_key <- check_inputs(bio = bio, bio_name = bio_name,
                                 openai_api_key = openai_api_key,
                                 openai_model = openai_model,
                                 openai_temperature = openai_temperature,
                                 prompt = prompt, prompt_fields = prompt_fields,
                                 prompt_fields_format = prompt_fields_format,
                                 prompt_fields_values = prompt_fields_values,
                                 post_process_fields = post_process_fields)

  # Generate final prompt from user inputs
  final_prompt <- get_prompt(bio = bio, bio_name = bio_name,
                             prompt = prompt, prompt_fields = prompt_fields,
                             prompt_fields_format = prompt_fields_format,
                             prompt_fields_values = prompt_fields_values)

  # ChatGPT API call with user inputs
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(list(model = openai_model,
                              messages = list(
                                list("role" = "user",
                                     "content"= final_prompt)
                              ),
                              temperature = openai_temperature)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Return unprocessed output if user inputted custom prompt
  if(!is.null(prompt)) return(resp$choices[[1]]$message$content)

  # Return tibble if default prompt
  return(tibble::as_tibble(jsonlite::fromJSON(resp$choices[[1]]$message$content)))

}
