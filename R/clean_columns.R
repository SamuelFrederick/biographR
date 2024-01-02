get_gpt_column <- function(data, column_name,
                           output_vector,
                           output_format,
                           openai_api_key = NULL,
                           openai_model = "gpt-3.5-turbo",
                           openai_temperature = 0,
                           check_inputs = TRUE) {

  if(check_inputs){
    # Throw error if data argument is not provided
    if(missing(data)) stop("Must input a data argument")
    if(!inherits(data, "data.frame")) stop("data argument must be a data.frame or tibble")

    # Throw an error if column_name is not provided
    if(missing(column_name)) stop("Must input a column_name argument")
    if(!is.character(column_name)&
       !is.numeric(column_name)) stop("column_name argument must be a character or numeric")

    # Check that ChatGPT model is a character
    if(!is.character(openai_model)) stop("Please input a valid OpenAI model as a string.")

    # Check that openai_temperature is a number between 0 and 2
    if(!is.numeric(openai_temperature)|
       is.na(as.numeric(openai_temperature))|
       !((as.numeric(openai_temperature)>=0&as.numeric(openai_temperature)<=2))) stop("Please input a valid OpenAI model temperature between 0 and 2.")

    # If no API key provided, check .Renviron
    if(is.null(openai_api_key)) {
      tmp <- Sys.getenv()
      if(sum(grepl("openai", names(tmp), ignore.case = T)==1)) {
        openai_api_key <- Sys.getenv(names(tmp)[grepl("openai", names(tmp), ignore.case =T)])
      }else {
        stop("Must input OpenAI API Key")
      }
    }
  }

  # Warn user if output_vector is not a character vector
  if(!is.character(output_vector)&!all(is.null(output_vector))) {
    warning("output_vector will be coerced to character vector.")
    suppressWarnings({output_vector <- as.character(output_vector)})
  }

  # Throw an error if output_format is not a character
  if(!(is.null(output_format)|is.character(output_format))) stop(sprintf("%s format must be a character", column_name))

  # Check whether column_name can be matched to column in data
  if(is.numeric(column_name)) {
    if(column_name>ncol(data)|column_name<1) stop("column_name must be a character or numeric value less than or equal to the number of columns")
    column_name <- colnames(data)[column_name]
  }
  if(!column_name%in%colnames(data)) stop("column_name must be present in data.")

  # Add an NA value for unmatched values
  # if(!all(is.null(output_vector))) output_vector <- unique(c(output_vector, NA))

  # Generate ChatGPT prompt
  if(!all(is.null(output_vector))&
     !all(is.null(output_format))){
    prompt <- sprintf("Output JSON with ONLY fields %s (the original values in an array) and %s (the corresponding matched values in an array) matching each value in %s=[%s] with a value in %s=[%s] with format %s. Only output JSON. Example output: {%s:['val1', 'val2', 'val3'], %s:['val2_gpt', 'val2_gpt', 'val1_gpt']}",
                      column_name,
                      paste(unique(data[,column_name]), collapse=","),
                      paste(column_name, "_gpt", sep = ""),
                      column_name,
                      paste(unique(data[,column_name]), collapse=","),
                      paste(column_name, "_gpt", sep = ""),
                      paste(output_vector, collapse = ","),
                      output_format,
                      column_name,
                      column_name, paste(column_name, "_gpt", sep = ""))
  } else if(!all(is.null(output_vector))&
            all(is.null(output_format))){
    prompt <- sprintf("Output JSON with fields %s and %s matching each value in %s=[%s] with a value in %s=[%s]. Only output JSON. Example output: {%s:['val1', 'val2', 'val3'], %s:['val2_gpt', 'val2_gpt', 'val1_gpt']}",
                      column_name,
                      # paste(unique(data[,column_name]), collapse=", "),
                      paste(column_name, "_gpt", sep = ""),
                      column_name,
                      paste(unique(data[,column_name]), collapse=", "),
                      paste(column_name, "_gpt", sep = ""),
                      paste(output_vector, collapse = ", "),
                      column_name, paste(column_name, "_gpt", sep = ""))
  } else if(all(is.null(output_vector))&
            !all(is.null(output_format))){
    prompt <- sprintf("Output JSON with ONLY fields %s (the original values) and %s formatting each value in %s=[%s] with format %s in a new column %s. Only output JSON.",
                      column_name,
                      paste(column_name, "_gpt", sep = ""),
                      column_name,
                      paste(unique(data[,column_name]), collapse=","),
                      output_format,
                      paste(column_name, "_gpt", sep = ""))
  } else{
    stop("Not a recognized configuration")
  }

  # print(prompt)

  # Make API call to ChatGPT
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(list(model = openai_model,
                              messages = list(
                                list("role" = "user",
                                     "content"= prompt)
                              ),
                              temperature = openai_temperature)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # print(resp$choices[[1]]$message$content)

  # Format API call result as a tibble
  tryCatch({
    out <- jsonlite::fromJSON(resp$choices[[1]]$message$content)|>
      tibble::as_tibble()
  }, error = function(cond){
    message(sprintf("ChatGPT output does not seem to work for prompt %s",
            prompt))
    return(resp$choices[[1]]$message$content)
  })


  # Return old data with new column
  return(
    data |>
      dplyr::left_join(out, by = c(column_name))|>
      tibble::as_tibble()
  )
}
#' Clean a series of columns using ChatGPT by mapping data into new categories and formats
#'
#' Maps old data in names of column_values to new categories and formats using ChatGPT.
#' clean_columns() returns data with new columns corresponding to mapped values.
#' This can be helpful for processing messy or unstructured text data
#' in a column (e.g., open-ended survey responses, names, etc.). clean_columns()
#' is particularly helpful for post-processing the output of [get_bio()].
#'
#' @param data The data to be processed
#' @param column_values A named list with column names from data as names and values as vectors with the desired categories for the corresponding column
#' @param column_formats A named list with column names from data as names and values as strings with the desired format for the corresponding column
#' @param openai_api_key API key for OpenAI, a string. If this is NULL, clean_columns() searches .Renviron for API key.
#' @param openai_model ChatGPT model to use, defaults to "chatgpt-3.5-turbo"
#' @param openai_temperature Specifies the amount of randomness in ChatGPT, a number between 0 and 2 with more randomness for higher numbers, defaults to 0
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
clean_columns <- function(data, column_values, column_formats,
                          openai_api_key = NULL,
                          openai_model = "gpt-3.5-turbo",
                          openai_temperature = 0) {

  # Throw an error if data is not provided
  if(missing(data)) stop("Must input a data argument")
  if(!inherits(data, "data.frame")) stop("data argument must be a data.frame or tibble")

  if(missing(column_values)&missing(column_formats)) stop("Must input at least one of the column_values or column_formats arguments")

  # Check column_values input
  if(missing(column_values)) {
    column_values <- NULL
  } else{
    if(!inherits(column_values, "list")) stop("column_values argument must be a list")
    if(!all(is.null(names(column_values)))&
       !any(names(column_values)%in%colnames(data))) stop("Names of column_values must be present in data.")
    if(!all(is.null(names(column_values)))&
       !all(names(column_values)%in%colnames(data))) warning("Some values in column_values not in data, will be ignored")
    if(all(is.null(names(column_values)))) stop("column_values must be a named list.")
  }

  # Check column_formats input
  if(missing(column_formats)){
    column_formats <- NULL
  }else{
    if(!inherits(column_formats, "list")) stop("column_formats argument must be a list")
    if(!all(is.null(names(column_formats)))&
       !any(names(column_formats)%in%colnames(data))) stop("Names of column_formats must be present in data.")
    if(!all(is.null(names(column_formats)))&
       !all(names(column_formats)%in%colnames(data))) warning("Some values in column_formats not in data, will be ignored")
    if(all(is.null(names(column_formats)))) stop("column_formats must be a named list.")

  }

  # Check that ChatGPT model is a character
  if(!is.character(openai_model)) stop("Please input a valid OpenAI model as a string.")

  # Check that openai_temperature is a number between 0 and 2
  if(suppressWarnings({!is.numeric(openai_temperature)|
     is.na(as.numeric(openai_temperature))|
     !((as.numeric(openai_temperature)>=0&as.numeric(openai_temperature)<=2))})) stop("Please input a valid OpenAI model temperature between 0 and 2.")

  # If no API key provided, check .Renviron
  if(is.null(openai_api_key)) {
    tmp <- Sys.getenv()
    if(sum(grepl("openai", names(tmp), ignore.case = T)==1)) {
      openai_api_key <- Sys.getenv(names(tmp)[grepl("openai", names(tmp), ignore.case =T)])
    }else {
      stop("Must input OpenAI API Key")
    }
  }

  # Iterate over every column in column_values and column_formats and add processed column to data
  for(i in union(names(column_values), names(column_formats))) {

    data <- get_gpt_column(data = data,
                           column_name = i,
                           output_vector = column_values[[i]],
                           output_format = column_formats[[i]],
                           openai_api_key = openai_api_key,
                           openai_model = openai_model,
                           openai_temperature = openai_temperature,
                           check_inputs = F)
  }

  # Return data with new, processed columns
  return(data)
}
