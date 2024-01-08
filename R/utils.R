################################################################################
# Check inputs for get_bio() and get_bio_function_call() and return API Key
################################################################################
check_inputs <- function(bio, bio_name,
                         openai_api_key,
                         openai_model,
                         openai_temperature,
                         openai_seed,
                         prompt, prompt_fields,
                         prompt_fields_formats,
                         prompt_fields_values,
                         prompt_fields_descriptions){

  # Set descriptions to NULL if missing
  if(missing(prompt_fields_descriptions)) prompt_fields_descriptions <- NULL

  if(is.null(prompt)) {

    # Error if bio is not a character
    if(!is.character(bio)) stop("Please input a bio as a string.", call. = F)

    # Error if bio_name is not a character
    if(!is.character(bio_name)) warning("You have not inputted a name for the individual. Consider inputting a name for better results.", call. = F)

    # Error if prompt_fields is not NULL or a character
    if(!is.null(prompt_fields)&!is.character(prompt_fields)) stop("The prompt_fields argument must be NULL or a character vector.", call. = F)

    # Error if prompt_fields_formats, prompt_fields_values, and prompt_fields_descriptions not NULL and not a list of characters
    if(!is.null(prompt_fields_formats)&
       (!is.list(prompt_fields_formats)|
       any(unlist(lapply(prompt_fields_formats, function(x) !all(is.na(x))&!is.character(x)))))) stop("The prompt_fields_formats argument must be NULL or a named list of strings.", call. = F)
    if(!is.null(prompt_fields_values)&
       (!is.list(prompt_fields_values)|
       any(unlist(lapply(prompt_fields_values, function(x) !all(is.na(x))&!is.character(x)))))) stop("The prompt_fields_values argument must be NULL or a named list of character vectors.", call. = F)
    if(!is.null(prompt_fields_descriptions)&
       (!is.list(prompt_fields_descriptions)|
       any(unlist(lapply(prompt_fields_descriptions, function(x) !all(is.na(x))&!is.character(x)))))) stop("The prompt_fields_descriptions argument must be NULL or a named list of strings", call. = F)

    # Warn users if some values from their lists are not present in prompt_fields
    if(!is.null(prompt_fields_formats)&
       !all(is.null(names(prompt_fields_formats)))&
       any(!names(prompt_fields_formats)%in%prompt_fields)) warning("Some names of prompt_fields_formats are not in prompt_fields and will be ignored.", call. = F)
    if(!is.null(prompt_fields_values)&
       !all(is.null(names(prompt_fields_values)))&
       any(!names(prompt_fields_values)%in%prompt_fields)) warning("Some names of prompt_fields_values are not in prompt_fields and will be ignored.", call. = F)
    if(!is.null(prompt_fields_descriptions)&
       !all(is.null(names(prompt_fields_descriptions)))&
       any(!names(prompt_fields_descriptions)%in%prompt_fields)) warning("Some names of prompt_fields_descriptions are not in prompt_fields and will be ignored.", call. = F)

    # Error if input lists are unnamed
    if(!is.null(prompt_fields_values)&
      all(is.null(names(prompt_fields_values)))&
       length(prompt_fields_values)!=length(prompt_fields)) stop("If prompt_fields_values is unnamed, it must be the same length as prompt_fields.", call. = F)
    if(!is.null(prompt_fields_formats)&
       all(is.null(names(prompt_fields_formats)))&
       length(prompt_fields_formats)!=length(prompt_fields)) stop("If prompt_fields_formats is unnamed, it must be the same length as prompt_fields.", call. = F)
    if(!is.null(prompt_fields_descriptions)&
       all(is.null(names(prompt_fields_descriptions)))&
        length(prompt_fields_descriptions)!=length(prompt_fields)) stop("If prompt_fields_descriptions is unnamed, it must be the same length as prompt_fields.", call. = F)

  } else{

    # If custom prompt provided, make sure that custom prompt includes formats and values, if applicable, for the prompt.
    # Alert users that bio_name is ignored.
    if(!is.null(prompt_fields)|!is.null(prompt_fields_formats)|!is.null(prompt_fields_values)) stop("Custom prompt should include desired fields, values, and formats.", call. = F)
    if(!is.null(bio_name)) warning("Custom prompt provided, bio_name argument will be ignored.", call. = F)

  }

  # Error if ChatGPT model is not a character
  if(!is.character(openai_model)) stop("Please input a valid OpenAI model as a string.", call. = F)

  # Error if OpenAI temperature is not a number between 0 and 2
  if(suppressWarnings({!is.numeric(openai_temperature)|
     is.na(as.numeric(openai_temperature))|
     !((as.numeric(openai_temperature)>=0&as.numeric(openai_temperature)<=2))})) stop("Please input a valid OpenAI model temperature between 0 and 2.", call. = F)

  # Error if openai_seed is not NULL or an integer
  if(!is.null(openai_seed)&!is.numeric(openai_seed)) stop("Please input an integer for openai_seed.", call. = F)
  if(any(openai_seed%%1!=0)) stop("Please input an integer for openai_seed.", call. = F)

  # If no API key provided, check .Renviron
  if(is.null(openai_api_key)) {
    tmp <- Sys.getenv()
    if(sum(grepl("openai", names(tmp), ignore.case = T)==1)) {
      openai_api_key <- Sys.getenv(names(tmp)[grepl("openai", names(tmp), ignore.case =T)])
    }else {
      stop("Must input OpenAI API Key", call. = F)
    }
  }
  return(openai_api_key)
}
################################################################################
# Returns main user prompt for OpenAI API call, a string
################################################################################
get_prompt <- function(bio,
                       bio_name,
                       prompt,
                       prompt_fields,
                       prompt_fields_formats,
                       prompt_fields_values){
  field <- NULL

  # Check whether bio is provided in addition to custom prompt; paste together
  if(!is.null(prompt)&!is.null(bio)) return(paste(prompt, " TEXT: ", bio, sep = ""))

  # Check whether custom prompt is provided without bio; return custom prompt
  if(!is.null(prompt)) return(prompt)

  # Add bio_name into prompt
  name_info <- ifelse(!is.null(bio_name)&!all(is.na(bio_name)), paste(" about ", bio_name, sep = ""), "")

  # If no prompt_fields provided, add default fields and formatting
  if(is.null(prompt_fields)) {
    message("No prompt_fields argument provided. Defaulting to: birth_date, highest_level_of_education, college, graduate school, previous_occupation, gender, town_of_birth, state_of_birth, married.")
    if(is.null(prompt_fields_formats)) {
      prompt_fields_formats <- list(birth_date = "{MM}/{DD}/{YYYY}",
                                    highest_level_of_education = "{DEGREE}",
                                    college = "{SCHOOL} - {DEGREE}",
                                    graduate_school = "{SCHOOL} - {DEGREE}",
                                    previous_occupation = "{OCCUPATION} - {YEARS}")
    }
    if(is.null(prompt_fields_values)) {
      prompt_fields_values <- list(married = c("Yes", "No"))
    }
    prompt_fields <- c("birth_date", "highest_level_of_education",
                       "college", "graduate_school",
                       "previous_occupation", "gender", "town_of_birth",
                       "state_of_birth", "married")
  }

  # If no prompt_fields_values provided, default to NA
  if(is.null(prompt_fields_values)) {
    prompt_fields_values <- rep(NA, length(prompt_fields))
  } else {
    # Turn values into a character vector of desired values
    prompt_fields_values <- lapply(prompt_fields,
                                   function(x) {
                                     ifelse(is.null(prompt_fields_values[[x]])|
                                              all(is.na(prompt_fields_values[[x]])), NA,
                                            sprintf("[%s]", paste(prompt_fields_values[[x]], collapse =",")))
                                   }) |>
      unlist()
  }

  # If no prompt_fields_formats (and not default prompt_fields), default to NA
  if(is.null(prompt_fields_formats)) {
    prompt_fields_formats <- rep(NA, length(prompt_fields))
  }else {
    # Turn formats into character vector
    prompt_fields_formats <- lapply(prompt_fields,
                                    function(x) ifelse(is.null(prompt_fields_formats[[x]])|
                                                         all(is.na(prompt_fields_formats[[x]])), NA,
                                                       prompt_fields_formats[[x]])) |>
      unlist()
  }

  # Final check that prompt_fields, values, and formats have same length
  max_len <- max(sapply(c(prompt_fields, prompt_fields_formats, prompt_fields_values),
                        FUN = function(x) length(x)))
  if(!all(sapply(c(prompt_fields, prompt_fields_formats, prompt_fields_values),
                 FUN = function(x) is.null(x)|length(x)==max_len))) stop("The arguments prompt_fields, prompt_fields_formats, and prompt_fields_values must have the same lengths.", call. = F)

  # Format fields to match {FIELD}={VALUES} with format {FORMAT}
  fields <- data.frame(prompt_fields = prompt_fields,
                       prompt_fields_formats = prompt_fields_formats,
                       prompt_fields_values = prompt_fields_values) |>
    tidyr::unite("field", prompt_fields, prompt_fields_values, sep = "=", na.rm = T) |>
    tidyr::unite("field", field, prompt_fields_formats, sep = " with format ", na.rm = T)

  # Generate and return the final prompt
  final_prompt <- sprintf("Extract biographical information%s from the provided text. Return biographical information in a JSON format with ONLY these fields: %s. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: %s",
                    name_info, paste(fields$field, collapse = ", "), bio)
  return(final_prompt)
}
################################################################################
# Returns example input and output for few-shot prompting in list form
################################################################################
get_prompt_fewshot <- function(prompt_fewshot,
                               prompt,
                               prompt_fields,
                               prompt_fields_formats,
                               prompt_fields_values) {

  # Error if prompt_fewshot not a tibble or data.frame
  if(!inherits(prompt_fewshot, "data.frame")) stop("prompt_fewshot must be a tibble or data.frame.", call. = F)

  # Error if no bio provided
  if(!"bio"%in%colnames(prompt_fewshot)) stop("Please input prompt_fewshot with a bio column.", call. = F)

  # Warning if no bio_name provided
  if(!"bio_name"%in%colnames(prompt_fewshot)) {
    warning("No bio_name in prompt_fewshot. Adding a bio_name may improve performance.", call. = F)
    prompt_fewshot[,'bio_name'] <- NA
  }

  # Error if not all prompt_fields in few-shot examples
  if(!all(is.null(prompt_fields))&
     !all(prompt_fields%in%colnames(prompt_fewshot))) stop("All fields in prompt_fields should be present in columns of prompt_fewshot.", call. = F)

  # Error if prompt_fields not provided and default fields not present in examples
  if(all(is.null(prompt_fields))&
     !all(c("birth_date", "highest_level_of_education",
            "college", "graduate_school",
            "previous_occupation", "gender", "town_of_birth",
            "state_of_birth", "married")%in% colnames(prompt_fewshot))) stop("All fields in prompt_fields should be present in columns of prompt_fewshot.", call. = F)

  # Return prompt examples
  do.call(append, list(lapply(1:nrow(prompt_fewshot),
         function(x) {
           if(length(prompt_fields)>1) {
             asst <- prompt_fewshot[x, prompt_fields]
           } else {
             asst <- list()
             asst[[prompt_fields[1]]] <- prompt_fewshot[x, prompt_fields[1]]
           }
           if(nrow(prompt_fewshot)>1){
             asst <- jsonlite::unbox(asst)
           }
            list(
              list("role" = "user",
                   "content" = get_prompt(bio = prompt_fewshot[x, 'bio'],
                                          bio_name = prompt_fewshot[x,'bio_name'],
                                          prompt = prompt,
                                          prompt_fields = prompt_fields,
                                          prompt_fields_formats =prompt_fields_formats,
                                          prompt_fields_values = prompt_fields_values)),
              list("role" = "assistant",
                   "content" = as.character(
                     jsonlite::toJSON(asst, auto_unbox = T)
                   ))
            )
         }), value = NULL))|>
    unlist(recursive = FALSE)
}
################################################################################
# Return fully formatted message field with few-shot examples and main prompt
################################################################################
get_prompt_wrapper <- function(bio,
                               bio_name,
                               prompt,
                               prompt_fields,
                               prompt_fields_formats,
                               prompt_fields_values,
                               prompt_fewshot) {

  # Error if custom prompt, fewshot-prompting, and no bio provided
  if(!is.null(prompt)&!is.null(prompt_fewshot)&is.null(bio)) stop("If you are using fewshot prompting, you should input a bio separately from your prompt.", call. = F)

  # Get few-shot prompt if applicable
  if(!is.null(prompt_fewshot)) prompt_fewshot <- get_prompt_fewshot(prompt_fewshot = prompt_fewshot,
                                                                    prompt = prompt,
                                                                    prompt_fields = prompt_fields,
                                                                    prompt_fields_formats = prompt_fields_formats,
                                                                    prompt_fields_values = prompt_fields_values)

  # Generate final prompt from user inputs
  final_prompt <- get_prompt(bio = bio, bio_name = bio_name,
                             prompt = prompt, prompt_fields = prompt_fields,
                             prompt_fields_formats = prompt_fields_formats,
                             prompt_fields_values = prompt_fields_values)

  # Return API messages field in list form
  append(
    append(
      list(
        list("role" = "system",
             "content" = "You are a system for extracting biographical data from text and returning it in a structured format.")
      ),
      prompt_fewshot
    ),
    list(
      list("role" = "user",
           "content" = final_prompt)
    )
  )
}
################################################################################
# Gets full prompt for few-shot function call, returns a list
################################################################################
get_prompt_fewshot_function_call <- function(prompt_fewshot,
                                             prompt_fields) {

  # Error if few-shot data is not a tibble or data.frame
  if(!inherits(prompt_fewshot, "data.frame")) stop("prompt_fewshot must be a tibble or data.frame.", call. = F)

  # Error if bio text is not provided for few-shot examples
  if(!"bio"%in%colnames(prompt_fewshot)) stop("Please input prompt_fewshot with a bio column.", call. = F)

  # Warning if bio_name not provided for few-shot examples
  if(!"bio_name"%in%colnames(prompt_fewshot)) {
    warning("No bio_name in prompt_fewshot. Adding a bio_name may improve performance.", call. = F)
    prompt_fewshot[,'bio_name'] <- NA
  }

  # Error if not all fields in prompt_fields are in example data
  if(!all(is.null(prompt_fields))&
     !all(prompt_fields%in%colnames(prompt_fewshot))) stop("All fields in prompt_fields should be present in columns of prompt_fewshot.", call. = F)

  # Error if prompt_fields NULL and not all default values are in example data
  if(all(is.null(prompt_fields))&
     !all(c("birth_date", "highest_level_of_education",
            "college", "graduate_school",
            "previous_occupation", "gender", "town_of_birth",
            "state_of_birth", "married")%in% colnames(prompt_fewshot))) stop("All fields in prompt_fields should be present in columns of prompt_fewshot.", call. = F)

  # Returns few-shot prompt with inputs and outputs in a list
  do.call(append, list(lapply(1:nrow(prompt_fewshot),
                              function(x) {
                                if(length(prompt_fields)>1) {
                                  asst <- prompt_fewshot[x, prompt_fields]
                                } else {
                                  asst <- list()
                                  asst[[prompt_fields[1]]] <- prompt_fewshot[x, prompt_fields[1]]
                                }
                                name_info <- ifelse(!all(is.null(prompt_fewshot[,'bio_name']))&!is.na(prompt_fewshot[x, 'bio_name']),
                                                    sprintf(" about %s", prompt_fewshot[x, 'bio_name']),
                                                    "")
                                list(
                                  list("role" = "user",
                                       "content" = sprintf("Return the requested biographical information%s from this text. Biographical Text: %s",
                                                           name_info, prompt_fewshot[x,'bio'])
                                  ),
                                  list("role" = "assistant",
                                       "tool_calls" = list(
                                         list("id" = "call_qwerty123",
                                              "function" = list(
                                                "name" = "make_dataframe_from_biographical_data",
                                                "arguments" = as.character(
                                                    jsonlite::toJSON(asst, auto_unbox = T)
                                                  )
                                                ),
                                              "type" = "function"
                                              )
                                         )
                                       ),
                                  list("role" = "tool",
                                       "tool_call_id" = "call_qwerty123",
                                       "name" = "make_dataframe_from_biographical_data",
                                       "content" =  as.character(
                                         jsonlite::toJSON(asst, auto_unbox = T)
                                       ))
                                )
                              }), value = NULL))|>
    unlist(recursive = FALSE)
}
################################################################################
# Returns the full prompt for the API call in a list
################################################################################
get_prompt_function_call <- function(bio,
                                     bio_name,
                                     prompt_fields,
                                     prompt_fields_formats,
                                     prompt_fields_values,
                                     prompt_fields_descriptions,
                                     prompt_fewshot,
                                     openai_model,
                                     openai_temperature,
                                     openai_seed){
  field <- NULL

  # Add bio_name into prompt
  name_info <- ifelse(!is.null(bio_name), paste(" about ", bio_name, sep = ""), "")

  # If no prompt_fields provided, add default fields and formatting
  if(is.null(prompt_fields)) {
    if(is.null(prompt_fields_formats)) {
      prompt_fields_formats <- list(birth_date = "{MM}/{DD}/{YYYY}",
                                    highest_level_of_education = "{DEGREE}",
                                    college = "{SCHOOL} - {DEGREE}",
                                    graduate_school = "{SCHOOL} - {DEGREE}",
                                    previous_occupation = "{OCCUPATION} - {YEARS}")
    }

    if(is.null(prompt_fields_values)) {
      prompt_fields_values <- list(married = c("Yes", "No"))
    }

    prompt_fields <- c("birth_date", "highest_level_of_education",
                       "college", "graduate_school",
                       "previous_occupation", "gender", "town_of_birth",
                       "state_of_birth", "married")
  }

  # Format function call parameters
  fields <- lapply(stats::setNames(1:length(prompt_fields), prompt_fields),
                   function(x) {
                     inner_out <- list("type" = "string")
                     descript <- ""
                     if(!all(is.null(prompt_fields_descriptions[[prompt_fields[x]]]))&
                        !all(is.na(prompt_fields_descriptions[[prompt_fields[x]]]))) {
                        descript <- sprintf("%s%s", descript,
                                            prompt_fields_descriptions[[prompt_fields[x]]])
                     }
                     if(!all(is.null(prompt_fields_formats[[prompt_fields[x]]]))&
                        !all(is.na(prompt_fields_formats[[prompt_fields[x]]]))) {
                       descript <- sprintf("%s Output %s information with format %s.",
                                           descript,
                                           prompt_fields[x],
                                           prompt_fields_formats[[prompt_fields[x]]])

                     }
                     if(descript == "") descript <- sprintf("Output %s information", prompt_fields[x])
                     inner_out[["description"]] <- descript
                     if(!all(is.null(prompt_fields_values[[prompt_fields[x]]]))&
                        !all(is.na(prompt_fields_values[[prompt_fields[x]]]))){
                       inner_out[["enum"]] <- as.list(prompt_fields_values[[prompt_fields[x]]])
                     }
                     return(inner_out)
                   })

  # get and return full OpenAI API call in list form
  final_prompt <- list(model = openai_model,
                       messages = append(
                         append(
                           list(
                             list("role" = "system",
                                  # "content" = "You are a system for extracting biographical data from text and returning it in JSON.")
                                  "content" = "You are a system for outputting biographical information as a dataframe. If biographical information is not in the correct format, reformat it to meet the requirements, and match the biographical text input with the accepted values. Your ONLY output should be the tool call function arguments with the proper formatting and values.")

                           ),
                           prompt_fewshot
                         ),
                         list(
                           list("role" = "user",
                                "content" = sprintf("Return the requested biographical information%s from this text in a dataframe. Biographical Text: %s", name_info, bio))
                         )
                       ),
                       tools = list(
                         list(
                           "type" = "function",
                           "function" = list(
                             "name" = "make_dataframe_from_biographical_data",
                             "description" = sprintf("Take biographical data%s and output in a dataframe. If multiple pieces of information fit into a single argument field, ALL relevant pieces of information for that field are joined by ';'. If no relevant information is present, argument is an empty string ''.", name_info),
                             "parameters" = list(
                               "type" = "object",
                               "properties" = fields,
                               "required" = as.list(prompt_fields)
                             )

                             )
                         )
                       ),
                       temperature = openai_temperature,
                       seed = openai_seed)

  return(final_prompt)
}
