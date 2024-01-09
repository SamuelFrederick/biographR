#' Get structured biographical data from unstructured text
#'
#' @description
#' `get_bio()` uses standard ChatGPT chat completions to retrieve structured data from input text and allows for fully customizable prompts.
#'
#' `get_bio_function_call()` uses ChatGPT function calling to retrieve structured data from input text.
#'
#' @param bio The bio to be processed, a string
#' @param bio_name The name of the individual whose biographical information is desired, a string. For `get_bio()`, bio_name can be a vector of strings containing the names of all individuals for whom biographical information is desired
#' @param prompt Only for use in `get_bio()`. A string. If desired, a custom prompt. This overrides the default prompt and should include any desired prompt fields, formats, and values.
#' @param prompt_fields A character vector of desired biographical output fields (e.g., "college", "graduate_school")
#' @param prompt_fields_formats A named list of strings giving desired formats for output fields (e.g., "\{SCHOOL\} - \{DEGREE\}"). Names should be present in prompt_fields.
#' @param prompt_fields_values A named list of character vectors of desired output values for each prompt field. Names should be present in prompt_fields.
#' @param prompt_fields_descriptions Only for use in `get_bio_function_call()`. A named list of strings with additional text describing each prompt field. Names should be present in prompt_fields.
#' @param prompt_fewshot A data.frame or tibble with complete example data. Should have a column called 'bio' containing unstructured example text, a column called 'bio_name' containing the name of the individual in the example (if applicable), and columns with outputs for every field in prompt_fields
#'    * `get_bio()` Example: data.frame(bio = "John Smith went to Nowhere University, and he graduated with a B.A.",
#'                                      bio_name = "John Smith",
#'                                      gender = "Male",
#'                                      college = "Nowhere University - B.A.")
#' @param openai_api_key API key for OpenAI, a string. If this is NULL, `get_bio()` searches .Renviron for API key.
#' @param openai_model ChatGPT model to use, defaults to "chatgpt-3.5-turbo"
#' @param openai_temperature A number between 0 and 2, specifies the amount of randomness in ChatGPT, with more randomness for higher numbers, defaults to 0
#' @param openai_seed An integer, pecifies a random seed for ChatGPT (this is in the development stage at OpenAI, so it might not work perfectly).
#'
#' @return A tibble containing desired biographical information or unprocessed API output from custom prompt
#' @export
#'
#' @examples
#' # Biographical Information about Kevin McCarthy from
#' # https://bioguide.congress.gov/search/bio/M001165
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
#' get_bio_function_call(bio = "MCCARTHY, KEVIN, a Representative from California;
#'                              born in Bakersfield, Kern County, Calif., January 26,
#'                              1965; graduated from Bakersfield High School,
#'                              Bakersfield, Calif., 1983; attended Bakersfield College,
#'                              Bakersfield. Calif., 1983-1986; B.S., California State
#'                              University, Bakersfield, Calif., 1989; M.B.A., California
#'                              State University, Bakersfield, Calif., 1994; staff,
#'                              United States Representative William Thomas of California,
#'                              1987-2002; member of the California state assembly,
#'                              2002-2007, minority leader, 2004-2006; elected as a
#'                              Republican to the One Hundred Tenth and to the eight
#'                              succeeding Congresses (January 3, 2007-present); majority
#'                              whip (One Hundred Twelfth and One Hundred Thirteenth
#'                              Congresses); majority leader (One Hundred Thirteenth
#'                              through One Hundred Fifteenth Congresses); minority
#'                              leader (One Hundred Sixteenth and One Hundred Seventeenth
#'                              Congress); Speaker of the House (One Hundred Eighteenth
#'                              Congress).",
#'                        bio_name = "Kevin McCarthy",
#'                        prompt_fields = c("highest_level_of_education",
#'                                          "previous_occupation", "birth_date"),
#'                        prompt_fields_formats = list(highest_level_of_education = "{DEGREE}",
#'                        previous_occupation = "{OCCUPATION} - {YEARS}",
#'                        birth_date = "{MM}/{DD}/{YYYY}"))
get_bio <- function(bio, bio_name = NULL,
                    prompt = NULL,
                    prompt_fields = NULL,
                    prompt_fields_formats = NULL,
                    prompt_fields_values = NULL,
                    prompt_fewshot = NULL,
                    openai_api_key = NULL,
                    openai_model = "gpt-3.5-turbo",
                    openai_temperature = 0,
                    openai_seed = NULL) {

  if(missing(bio)) bio <- NULL

  # If bios for multiple individuals requested, collapse to string
  bio_vec <- FALSE
  if(length(bio_name)>1) {
    bio_name_original <- bio_name
    bio_name <- paste(
      paste(bio_name[1:(length(bio_name)-1)], collapse = ", "),
      bio_name[length(bio_name)], sep = ", and"
    )
    bio_vec <- TRUE
  }

  # Check that inputs are valid and return OpenAI API key
  openai_api_key <- check_inputs(bio = bio, bio_name = bio_name,
                                 openai_api_key = openai_api_key,
                                 openai_model = openai_model,
                                 openai_temperature = openai_temperature,
                                 openai_seed = openai_seed,
                                 prompt = prompt, prompt_fields = prompt_fields,
                                 prompt_fields_formats = prompt_fields_formats,
                                 prompt_fields_values = prompt_fields_values)
  if(!is.null(prompt_fields_values)&
     all(is.null(names(prompt_fields_values)))) names(prompt_fields_values) <- prompt_fields
  if(!is.null(prompt_fields_formats)&
     all(is.null(names(prompt_fields_formats)))) names(prompt_fields_formats) <- prompt_fields

  # Get main prompt body
  prompt_in <- get_prompt_wrapper(bio = bio,
                                  bio_name = bio_name,
                                  prompt = prompt,
                                  prompt_fields = prompt_fields,
                                  prompt_fields_formats = prompt_fields_formats,
                                  prompt_fields_values = prompt_fields_values,
                                  prompt_fewshot = prompt_fewshot)

  # ChatGPT API call with user inputs
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(list(model = openai_model,
                              messages = prompt_in,
                              temperature = openai_temperature,
                              seed = openai_seed)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  message(sprintf("Input Tokens: %s\nOutput Tokens: %s\nTotal Tokens: %s",
                  resp$usage$prompt_tokens,
                  resp$usage$completion_tokens,
                  resp$usage$total_tokens))

  # Return unprocessed output if user inputted custom prompt
  if(!is.null(prompt)) return(resp$choices[[1]]$message$content)
  if(bio_vec&is.null(prompt_fewshot)) {
    bio_dat <- jsonlite::fromJSON(resp$choices[[1]]$message$content)
    bio_dat <- do.call(rbind,
            lapply(stats::setNames(1:length(bio_dat), names(bio_dat)),
               function(x){
                 tmp <- as.data.frame(
                   bio_dat[[x]]
                 )
                 tmp$name <- names(bio_dat)[x]
                 return(tmp)
            }))|> tibble::as_tibble()
    return(bio_dat)
  }
  if(bio_vec&!is.null(prompt_fewshot)) {
    bio_dat <- tibble::as_tibble(
      jsonlite::fromJSON(resp$choices[[1]]$message$content)
    )
    bio_dat$bio_name <- bio_name_original
    return(bio_dat)
  }

  # Return tibble if default prompt
  return(tibble::as_tibble(jsonlite::fromJSON(resp$choices[[1]]$message$content)))

}
