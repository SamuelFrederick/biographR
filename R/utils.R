check_inputs <- function(bio, bio_name,
                         openai_api_key,
                         openai_model,
                         openai_temperature,
                         prompt, prompt_fields, prompt_fields_format,
                         prompt_fields_values,
                         post_process_fields){

  if(is.null(prompt)) {

    # If no custom prompt, check that prompt parameters are valid.
    if(!is.character(bio)) stop("Please input a bio as a string.")
    if(!is.character(bio_name)) warning("You have not inputted a name for the individual. Consider inputting a name for better results.")
    if(!is.null(prompt_fields)&!is.character(prompt_fields)) stop("The prompt_fields argument must be NULL or a character vector.")
    if(!is.null(prompt_fields_format)&
       !(is.character(prompt_fields_format)|is.list(prompt_fields_format))) stop("The prompt_fields_format argument must be NULL, a character vector, or a named list.")
    if(!is.null(prompt_fields_values)&!is.list(prompt_fields_values)|any(unlist(lapply(prompt_fields_values, function(x) !is.na(x)&!is.character(x))))) stop("The prompt_fields_values argument must be NULL or a list of character vectors.")
    if(!is.null(prompt_fields_format)&
       !all(is.null(names(prompt_fields_format)))&
       any(!names(prompt_fields_format)%in%prompt_fields)) warning("Some names of prompt_fields_format are not in prompt_fields and will be ignored.")
    if(!is.null(prompt_fields_values)&
       !all(is.null(names(prompt_fields_values)))&
       any(!names(prompt_fields_values)%in%prompt_fields)) warning("Some names of prompt_fields_values are not in prompt_fields and will be ignored.")
    if(!is.null(prompt_fields_values)&
      (all(is.null(names(prompt_fields_values)))&
       length(prompt_fields_values)!=length(prompt_fields))) stop("If prompt_fields_values is unnamed, it must be the same length as prompt_fields.")
    if(!is.null(prompt_fields_format)&
       all(is.null(names(prompt_fields_format)))&
       length(prompt_fields_format)!=length(prompt_fields)) stop("If prompt_fields_format is unnamed, it must be the same length as prompt_fields.")

  } else{

    # If custom prompt provided, make sure that custom prompt includes
    # formats and values, if applicable, for the prompt.
    # Alert users that bio_name is ignored.
    if(!is.null(prompt_fields)|!is.null(prompt_fields_format)|!is.null(prompt_fields_values)) stop("Custom prompt should include desired fields, values, and formats.")
    if(!is.null(bio_name)) warning("Custom prompt provided, bio_name argument will be ignored.")

  }

  # Check that ChatGPT model is a character
  if(!is.character(openai_model)) stop("Please input a valid OpenAI model as a string.")

  # Check that openai_temperature is a number between 0 and 2
  if(!is.numeric(openai_temperature)|
     is.na(as.numeric(openai_temperature))|
     !((as.numeric(openai_temperature)>=0&as.numeric(openai_temperature)<=2))) stop("Please input a valid OpenAI model temperature between 0 and 2.")

  # Check that post_process fields is a logical
  if(!is.logical(post_process_fields)) stop("Please input TRUE or FALSE for post_process_fields.")

  # If no API key provided, check .Renviron
  if(is.null(openai_api_key)) {
    tmp <- Sys.getenv()
    if(sum(grepl("openai", names(tmp), ignore.case = T)==1)) {
      openai_api_key <- Sys.getenv(names(tmp)[grepl("openai", names(tmp), ignore.case =T)])
    }else {
      stop("Must input OpenAI API Key")
    }
  }
  return(openai_api_key)
}

get_prompt <- function(bio, bio_name, prompt, prompt_fields, prompt_fields_format,
                       prompt_fields_values){
  field <- NULL

  # Check whether bio is provided in addition to custom prompt; paste together
  if(!is.null(prompt)&!is.null(bio)) return(paste(prompt, " TEXT: ", bio, sep = ""))

  # Check whether custom prompt is provided without bio; return custom prompt
  if(!is.null(prompt)) return(prompt)

  # Add bio_name into prompt
  name_info <- ifelse(!is.null(bio_name), paste(" about ", bio_name, sep = ""), "")

  # If no prompt_fields provided, add default fields and formatting
  if(is.null(prompt_fields)) {
    if(is.null(prompt_fields_format)) {
      prompt_fields_format <- c("{MM}/{DD}/{YYYY}", "{DEGREE}",
        "{SCHOOL} - {YEAR} - {DEGREE}", "{SCHOOL} - {YEAR} - {DEGREE}",
        "{OCCUPATION} - {YEARS}",
        NA, NA, NA, NA, NA)
    }

    prompt_fields <- c("birth_date", "highest_level_of_education",
                       "college", "graduate_school",
                       "previous_occupation", "gender", "town_of_birth",
                       "state_of_birth", "married", "number_of_children")
  }

  # If no prompt_fields_values provided, default to NA
  if(is.null(prompt_fields_values)) {
    prompt_fields_values <- as.list(rep(NA, length(prompt_fields)))
  }

  # If no prompt_fields_format (and not default prompt_fields), default to NA
  if(is.null(prompt_fields_format)) {
    prompt_fields_format <- rep(NA, length(prompt_fields))
  }

  if(!all(is.null(names(prompt_fields_values)))){

    # Turn values into a character vector of desired values
    prompt_fields_values <- lapply(prompt_fields,
                                   function(x) {
                                     ifelse(is.null(prompt_fields_values[[x]])|
                                        all(is.na(prompt_fields_values[[x]])), NA,
                                        sprintf("[%s]", paste(prompt_fields_values[[x]], collapse =",")))
                                   }) |>
      unlist()

  } else{

    # Turn values into a character vector
    prompt_fields_values <- lapply(prompt_fields_values,
                                   function(x) {
                                     ifelse(is.null(x)|all(is.na(x)),NA,
                                            sprintf("[%s]", paste(x, collapse =",")))
                                     }) |> unlist()

  }

  if(is.list(prompt_fields_format)) {
    # Turn formats into character vector
    prompt_fields_format <- lapply(prompt_fields,
                                   function(x) ifelse(is.null(prompt_fields_format[[x]])|
                                                        all(is.na(prompt_fields_format[[x]])), NA,
                                                      prompt_fields_format[[x]])) |>
      unlist()
  }

  # Final check that prompt_fields, values, and format have same length
  max_len <- max(sapply(c(prompt_fields, prompt_fields_format, prompt_fields_values),
                        FUN = function(x) length(x)))
  if(!all(sapply(c(prompt_fields, prompt_fields_format, prompt_fields_values),
                 FUN = function(x) is.null(x)|length(x)==max_len))) stop("The arguments prompt_fields, prompt_fields_formats, and prompt_fields_values must have the same lengths.")

  # Format fields to match {FIELD}={VALUES} with format {FORMAT}
  fields <- data.frame(prompt_fields = prompt_fields,
                       prompt_fields_format = prompt_fields_format,
                       prompt_fields_values = prompt_fields_values) |>
    tidyr::unite("field", prompt_fields, prompt_fields_values, sep = "=", na.rm = T) |>
    tidyr::unite("field", field, prompt_fields_format, sep = " with format ", na.rm = T)

  # Generate and return the final prompt
  final_prompt <- sprintf("Extract biographical information%s from the provided text. Return biographical information in a JSON format with ONLY these fields: %s. If multiple pieces of information fit into a single JSON field, return all pieces of information for that field joined by ';'. If no information is available for a given field, return an empty string ''. TEXT: %s",
                    name_info, paste(fields$field, collapse = ", "), bio)
  return(final_prompt)
}

