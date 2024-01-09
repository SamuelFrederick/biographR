################################################################################
# Checks inputs for clean_columns() and clean_columns_function_call() and
#   returns OpenAI API Key from .Renviron if none inputted
################################################################################
check_inputs_clean_columns <- function(data,
                                       column_values,
                                       column_formats,
                                       column_descriptions = NULL,
                                       openai_api_key,
                                       openai_model,
                                       openai_temperature,
                                       openai_seed,
                                       openai_context_window = NULL){

  # Error if data argument not provided
  if(missing(data)) stop("Must input a data argument", call. = F)

  # Error if data argument not data.frame or tibble
  if(!inherits(data, "data.frame")) stop("data argument must be a data.frame or tibble", call. = F)

  # Error if column_values, column_formats, and column_descriptions are not provided
  if((all(is.null(column_values))&
      all(is.null(column_formats))&
      all(is.null(column_descriptions)))) stop("Must input at least one of the column_values, column_formats, or column_descriptions arguments", call. = F)

  # Check column_values input
  if(!all(is.null(column_values))){

    # Error if column_values is not a list
    if(!inherits(column_values, "list")) stop("column_values argument must be a list", call. = F)

    # Error if column_values names are not in data
    if(!all(is.null(names(column_values)))&
       !any(names(column_values)%in%colnames(data))) stop("Names of column_values must be present in data.", call. = F)

    # Warning if some column_values names are not in data
    if(!all(is.null(names(column_values)))&
       !all(names(column_values)%in%colnames(data))) warning("Some values in column_values not in data, will be ignored", call. = F)

    # Error if column_values has no names
    if(all(is.null(names(column_values)))) stop("column_values must be a named list.", call. = F)
  }

  # Check column_formats input
  if(!all(is.null(column_formats))){

    # Error if column_formats is not a list
    if(!inherits(column_formats, "list")) stop("column_formats argument must be a list", call. = F)

    # Error if column_formats names are not in data
    if(!all(is.null(names(column_formats)))&
       !any(names(column_formats)%in%colnames(data))) stop("Names of column_formats must be present in data.", call. = F)

    # Warning if some column_formats names are not in data
    if(!all(is.null(names(column_formats)))&
       !all(names(column_formats)%in%colnames(data))) warning("Some values in column_formats not in data, will be ignored", call. = F)

    # Error if column_formats has no names
    if(all(is.null(names(column_formats)))) stop("column_formats must be a named list.", call. = F)
  }

  # Check column_descriptions input
  if(!all(is.null(column_descriptions))){

    # Error if column_descriptions is not a list
    if(!inherits(column_descriptions, "list")) stop("column_descriptions argument must be a list", call. = F)

    # Error if column_descriptions names are not in data
    if(!all(is.null(names(column_descriptions)))&
       !any(names(column_descriptions)%in%colnames(data))) stop("Names of column_descriptions must be present in data.", call. = F)

    # Warning if some column_descriptions names are not in data
    if(!all(is.null(names(column_descriptions)))&
       !all(names(column_descriptions)%in%colnames(data))) warning("Some values in column_descriptions not in data, will be ignored", call. = F)

    # Error if column_descriptions has no names
    if(all(is.null(names(column_descriptions)))) stop("column_descriptions must be a named list.", call. = F)
  }

  # Error if ChatGPT model is not a character
  if(!is.character(openai_model)) stop("Please input a valid OpenAI model as a string.", call. = F)

  # Error if openai_temperature is not a number between 0 and 2
  if(suppressWarnings({!is.numeric(openai_temperature)|
      is.na(as.numeric(openai_temperature))|
      !((as.numeric(openai_temperature)>=0&as.numeric(openai_temperature)<=2))})) stop("Please input a valid OpenAI model temperature between 0 and 2.", call. = F)

  # Error if openai_seed is not null or an integer
  if(!is.null(openai_seed)&!is.numeric(openai_seed)) stop("Please input an integer for openai_seed.", call. = F)
  if(any(openai_seed%%1!=0)) stop("Please input an integer for openai_seed.", call. = F)

  # Error if openai_context_window is not an integer
  if(!is.null(openai_context_window)) {
    if(!is.numeric(openai_context_window)) stop("Please input an integer for openai_context_window.", call. = F)
    if(any(openai_context_window%%1!=0)) stop("Please input an integer for openai_context_window.", call. = F)
  }

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
# Gets user prompt for clean_columns()
################################################################################
get_prompt_clean_columns <- function(output_vector,
                                     output_format,
                                     data,
                                     column_name,
                                     fewshot_lgl = F){

  # Make main prompt framework, depending on presence of output_vector
  prompt_frame <- ifelse(!all(is.null(output_vector)),
                         "Output JSON with ONLY the field %s which should be an array matching each value in %s=%s%s%s. Only output JSON.", #Example output: {%s:['val2_gpt', 'val2_gpt','val3_gpt']}",
                         "Output JSON with ONLY the field %s which should be an array containing each value in %s=%s%s%s. Only output JSON.") #Example output: {%s:['val1_gptformat', 'val2_gptformat', 'val3_gptformat']}")

  # Make format framework using user format
  fmt_out <- ifelse(all(is.null(output_format)), "",
                    paste(" with format ", output_format, sep = ""))

  # Make output column_name
  nm_out <- paste(column_name, "_gpt", sep = "")

  # Make values framework using user values
  vals_out <- ifelse(all(is.null(output_vector)), "",
                     sprintf(" with a value in %s",
                             as.character(jsonlite::toJSON(output_vector))))

  # Make values framework using original user values
  vals_in <- ifelse(all(is.null(data[,column_name]))|
                      all(is.na(data[,column_name])),
                    stop(sprintf("No data for %s", column_name), call. = F),
                    ifelse(fewshot_lgl, as.character(jsonlite::toJSON(data[,column_name])),
                           as.character(jsonlite::toJSON(sort(unique(data[,column_name]))))))

  # Generate ChatGPT prompt
  prompt <- sprintf(prompt_frame,
                    nm_out,
                    column_name,
                    vals_in,
                    vals_out,
                    fmt_out)

  return(prompt)
}
################################################################################
# Returns list splitting few-shot prompt input into prompt_fewshot_n parts
################################################################################
split_fewshot <- function(column_name,
                          prompt_fewshot,
                          prompt_fewshot_n,
                          output_vector,
                          output_format) {

  # Get example input column
  c_orig <- prompt_fewshot[[column_name]]

  # Get example output column
  c_gpt <- prompt_fewshot[[paste(column_name, "_gpt", sep = "")]]

  # Error if length of input and output columns are unequal
  if(length(c_orig)!=length(c_gpt)) stop("Example inputs and outputs must be the same length", call. = F)

  # Error if prompt_fewshot_n is greater than length of input column
  if(prompt_fewshot_n>length(c_orig)) stop("prompt_fewshot_n must be less than or equal to the number of examples.", call. = F)

  # Get column indices, breaking input column into prompt_fewshot_n parts
  c_h <- split(1:length(c_orig), ceiling(1:length(c_orig)/(length(c_orig)/prompt_fewshot_n)))

  # Output list of example inputs and outputs in prompt_fewshot_n parts
  do.call(append, list(lapply(1:prompt_fewshot_n,
                              function(x) {
                                df <- data.frame(c_orig[c_h[[x]]])
                                colnames(df) <- column_name
                                out <- list()
                                out[[paste(column_name, "_gpt", sep = "")]] <- c_gpt[c_h[[x]]]
                                out <- as.character(jsonlite::toJSON(out, auto_unbox = T))
                                list(
                                  list("role" = "user",
                                       "content" = get_prompt_clean_columns(output_vector = output_vector,
                                                                            output_format = output_format,
                                                                            data = df,
                                                                            column_name = column_name,
                                                                            fewshot_lgl = T)),
                                  list("role" = "assistant",
                                       "content" = out)
                                )
                              }), value = NULL))|>
    unlist(recursive = FALSE)
}
################################################################################
# Returns few-shot prompt for clean_columns()
################################################################################
get_prompt_clean_columns_fewshot <- function(output_vector,
                                             output_format,
                                             column_name,
                                             prompt_fewshot,
                                             prompt_fewshot_type,
                                             prompt_fewshot_n) {

  # Error if prompt_fewshot_type is not 'specific' or 'general'
  if(!is.character(prompt_fewshot_type)) stop("prompt_fewshot_type should be either 'specific' or 'general'.", call. = F)
  if(!grepl("specific|general", prompt_fewshot_type, ignore.case = T)) stop("prompt_fewshot_type should be either 'specific' or 'general'", call. = F)

  # Error if prompt_fewshot is not a data.frame, tibble, or list
  if(!inherits(prompt_fewshot, "data.frame")&
     !inherits(prompt_fewshot, "list")) stop("prompt_fewshot must be a tibble, data.frame, or list.", call. = F)

  # Error if prompt_fewshot_n is not a list and not an integer
  if(!inherits(prompt_fewshot_n, "list")){
    if(!is.numeric(prompt_fewshot_n)) stop("prompt_fewshot_n must be an integer.", call. = F)
    if(prompt_fewshot_n%%1!=0) stop("prompt_fewshot_n must be an integer.", call. = F)
  }

  prompt_fewshot <- as.list(prompt_fewshot)

  ##############################################################################
  ############################# SPECIFIC FEW-SHOT  #############################
  ##############################################################################
  if(grepl("specific", prompt_fewshot_type, ignore.case = T)){

    # Error if column_name not in prompt_fewshot
    if(!column_name%in%names(prompt_fewshot)) stop(sprintf("%s must be present in prompt_fewshot.", column_name), call. = F)

    # Error if column_name example output not in prompt_fewshot
    if(!paste(column_name, "_gpt",sep = "")%in%names(prompt_fewshot)) stop(sprintf("%s must be present in prompt_fewshot",
                                                                                   paste(column_name, "_gpt", sep = "")), call. = F)

    # Get number for column_name
    if(inherits(prompt_fewshot_n, 'list')){
      # Error if column_name not in list version of prompt_fewshot_n
      if(!column_name%in%names(prompt_fewshot_n)) stop("If prompt_fewshot_n is a list, names from prompt_fewshot must be provided.", call. = F)
      fs_n <- prompt_fewshot_n[[column_name]]
    }else{
      fs_n <- prompt_fewshot_n
    }

    # Call split_fewshot() to get prompt and example output for column_name
    split_fewshot(column_name = column_name,
                  prompt_fewshot = prompt_fewshot,
                  prompt_fewshot_n = fs_n,
                  output_vector = output_vector,
                  output_format = output_format)

  } else{
    #############################################################################
    ############################# GENERAL FEW-SHOT  #############################
    #############################################################################

    # Get example inputs and outputs
    c_orig <- names(prompt_fewshot)[!grepl("_gpt|format|values$", names(prompt_fewshot))]
    c_gpt <- names(prompt_fewshot)[grepl("_gpt$", names(prompt_fewshot))]

    # Error if no overlap between example inputs and outputs
    if(length(intersect(paste(c_orig, "_gpt", sep = ""), c_gpt))==0) stop("prompt_fewshot must contain at least one example input and one example output.", call. = F)

    # Message if there are data in inputs without example outputs
    if(any(!paste(c_orig, "_gpt", sep = "")%in%intersect(c_gpt, paste(c_orig, "_gpt", sep = "")))) {
      message(sprintf("Some example inputs do not have example gpt outputs. %s will be ignored.",
                      paste(c_orig[!paste(c_orig, "_gpt", sep = "")%in%intersect(c_gpt, paste(c_orig, "_gpt", sep = ""))],
                            collapse = ", ")))

    }

    # Message if there are data in outputs without example inputs
    if(any(!c_gpt%in%intersect(c_gpt, paste(c_orig, "_gpt", sep = "")))) {
      message(sprintf("Some example gpt outputs do not have corresponding example inputs. %s will be ignored",
                      paste(c_gpt[!c_gpt%in%intersect(c_gpt, paste(c_orig, "_gpt", sep = ""))])))
    }

    # Sort column names which are in both example inputs and outputs
    c_orig <- sort(c_orig[paste(c_orig, "_gpt", sep = "")%in%intersect(c_gpt, paste(c_orig, "_gpt", sep = ""))])

    # Error if input and output examples are not the same length
    if(any(unlist(lapply(1:length(c_orig), function(x) length(prompt_fewshot[[c_orig[x]]])!=length(prompt_fewshot[[paste(c_orig[x], "_gpt", sep = "")]]))))){
      stop("Example inputs and outputs must be the same length: problem with %s", call. = F)
    }

    if(length(c_orig)>1){

      # Message that prompt_fewshot_n will be ignored
      if(inherits(prompt_fewshot_n, "list")) message("When prompt_fewshot_type='general' and more than one example provided, prompt_fewshot_n is ignored.")
      if(is.numeric(prompt_fewshot_n)) {
        if(prompt_fewshot_n!=1) {
          message("When prompt_fewshot_type='general' and more than one example provided, prompt_fewshot_n is ignored.")
        }
      }

      # Return example input and output prompts from prompt_fewshot
      do.call(append, list(lapply(1:length(c_orig),
                                  function(x) {
                                    out <- list()
                                    out[[paste(c_orig[x], "_gpt", sep = "")]] <- prompt_fewshot[[paste(c_orig[x], "_gpt", sep = "")]]
                                    out <- as.character(jsonlite::toJSON(out, auto_unbox = T))
                                    if(paste(c_orig[x], "_values", sep = "")%in%names(prompt_fewshot)){
                                      vec_in <- prompt_fewshot[[paste(c_orig[x], "_values", sep = "")]]
                                      if(paste(c_orig[x], "_format", sep = "")%in%names(prompt_fewshot)){
                                        form_in <- prompt_fewshot[[paste(c_orig[x], "_format", sep = "")]]
                                      }else{
                                        form_in <- NULL
                                      }
                                    }else if(paste(c_orig[x], "_format", sep = "")%in%names(prompt_fewshot)){
                                      vec_in <- NULL
                                      form_in <- prompt_fewshot[[paste(c_orig[x], "_format", sep = "")]]
                                    }else{
                                      vec_in <- output_vector
                                      form_in <- output_format
                                    }
                                    list(
                                      list("role" = "user",
                                           "content" = get_prompt_clean_columns(output_vector = vec_in,
                                                                                output_format = form_in,
                                                                                data = as.data.frame(prompt_fewshot[c_orig[x]]),
                                                                                column_name = c_orig[x],
                                                                                fewshot_lgl = T)),
                                      list("role" = "assistant",
                                           "content" = out)
                                    )
                                  }), value = NULL))|>
        unlist(recursive = FALSE)
    }else {

      # Get the prompt_fewshot_n for each example input and output column
      if(inherits(prompt_fewshot_n, 'list')){
        if(!column_name%in%names(prompt_fewshot_n)) stop("If prompt_fewshot_n is a list, names from prompt_fewshot must be provided.", call. = F)
        fs_n <- prompt_fewshot_n[[column_name]]
      }else{
        fs_n <- prompt_fewshot_n
      }

      # Get values and formats for example inputs and outputs if any
      if(paste(c_orig, "_values", sep = "")%in%names(prompt_fewshot)){
        vec_in <- prompt_fewshot[[paste(c_orig, "_values", sep = "")]]
        if(paste(c_orig, "_format", sep = "")%in%names(prompt_fewshot)){
          form_in <- prompt_fewshot[[paste(c_orig, "_format", sep = "")]]
        }else{
          form_in <- NULL
        }
      }else if(paste(c_orig, "_format", sep = "")%in%names(prompt_fewshot)){
        vec_in <- NULL
        form_in <- prompt_fewshot[[paste(c_orig, "_format", sep = "")]]
      }else{
        vec_in <- output_vector
        form_in <- output_format
      }

      # Call split_fewshot() to get prompt and example output for column_name
      split_fewshot(column_name = column_name,
                    prompt_fewshot = prompt_fewshot,
                    prompt_fewshot_n = fs_n,
                    output_vector = vec_in,
                    output_format = form_in)
    }
  }
}
################################################################################
# Returns full message prompt for ChatGPT API for clean_columns()
################################################################################
get_prompt_clean_columns_wrapper <- function(output_vector,
                                             output_format,
                                             column_name,
                                             data,
                                             prompt_fewshot,
                                             prompt_fewshot_type,
                                             prompt_fewshot_n){

  # Message if few-shot prompting not conducted for a column
  if(prompt_fewshot_type=="specific"&!is.null(prompt_fewshot)&!column_name%in%names(prompt_fewshot)) {
    message(sprintf("%s not in prompt_fewshot fields. Skipping few-shot prompting for this column.", column_name))
    prompt_fewshot <- NULL
  }

  # If prompt_fewshot provided, get prompt for few-shot prompting
  if(!is.null(prompt_fewshot)) prompt_fewshot <- get_prompt_clean_columns_fewshot(output_vector = output_vector,
                                                                                  output_format = output_format,
                                                                                  column_name = column_name,
                                                                                  prompt_fewshot = prompt_fewshot,
                                                                                  prompt_fewshot_type = prompt_fewshot_type,
                                                                                  prompt_fewshot_n = prompt_fewshot_n)

  # Get the main prompt for a given column
  main_prompt <- get_prompt_clean_columns(output_vector = output_vector,
                                          output_format = output_format,
                                          data = data,
                                          column_name = column_name)

  # Return full message prompt for ChatGPT API
  append(
    append(
      list(
        list("role" = "system",
             "content" = "You are a system that reformats an array of user values according to user specifications and matches user values to a specified array of new categories. Your output should be an array of the same length as the input array containing only the reformatted input values and newly matched categories.")
      ),
      prompt_fewshot
    ),
    list(
      list("role" = "user",
           "content" = main_prompt)
    )
  )
}
################################################################################
# Returns data with processed column added
################################################################################
get_gpt_column <- function(data, column_name,
                           output_vector,
                           output_format,
                           prompt_fewshot,
                           prompt_fewshot_type,
                           prompt_fewshot_n,
                           openai_api_key,
                           openai_model,
                           openai_temperature,
                           openai_seed) {

  # Warn user if output_vector is not a character vector
  if(!is.character(output_vector)&!all(is.null(output_vector))) {
    warning(sprintf("output_vector for %s will be coerced to character vector.", column_name), call. = F)
    suppressWarnings({output_vector <- as.character(output_vector)})
  }

  # Error if output_format is not NULL or a character
  if(!(is.null(output_format)|is.character(output_format))) stop(sprintf("%s format must be NULL or a character.", column_name), call. = F)

  # Error if column_name is numeric and not present in data columns
  if(is.numeric(column_name)) {
    if(column_name>ncol(data)|column_name<1) stop("column_name must be a character or numeric value less than or equal to the number of columns.", call. = F)
    column_name <- colnames(data)[column_name]
  }

  # Error if column_name is not present in data columns
  if(!column_name%in%colnames(data)) stop(sprintf("column_name %s must be present in data.", column_name), call. = F)

  # Get messages prompt for ChatGPT API
  prompt_in <- get_prompt_clean_columns_wrapper(output_vector = output_vector,
                                                output_format= output_format,
                                                column_name = column_name,
                                                data = data,
                                                prompt_fewshot = prompt_fewshot,
                                                prompt_fewshot_type = prompt_fewshot_type,
                                                prompt_fewshot_n = prompt_fewshot_n)

  # Make API call to ChatGPT
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(list(model = openai_model,
                              messages = prompt_in,
                              temperature = openai_temperature,
                              seed = openai_seed)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Message informing user of tokens use for each column
  message(sprintf("%s\nInput Tokens: %s\nOutput Tokens:%s\nTotal Tokens:%s",
                  column_name,
                  resp$usage$prompt_tokens,
                  resp$usage$completion_tokens,
                  resp$usage$total_tokens))

  # Try to format API call result as a tibble
  tryCatch({
    out <- as.data.frame(jsonlite::fromJSON(resp$choices[[1]]$message$content))
    n_col_in <- length(unique(data[,column_name]))
    if(all(dim(out)==c(n_col_in, 1))){
      colnames(out) <- paste(column_name, "_gpt", sep = "")
      out[,column_name] <- sort(unique(data[,column_name]))
    } else if (all(dim(out)==c(n_col_in, 2))){
      colnames(out)[apply(out, 2, FUN = function(x) all(unique(data[,column_name])%in%x))]<- column_name
      colnames(out)[apply(out, 2, FUN = function(x) !all(unique(data[,column_name])%in%x))]<- paste(column_name, "_gpt", sep = "")
    } else if (all(dim(out)==rep(n_col_in, 2))) {
      tmp <- apply(out, 2, FUN = function(x) x[!is.na(x)])
      out <- list()
      out[[column_name]] <- names(tmp)
      out[[paste(column_name, "_gpt", sep = "")]] <- unname(tmp)
      out <- as.data.frame(out)
    } else{
      stop("Output does not have proper dimensions. Few-shot prompting is recommended.", call. = F)
    }
  }, error = function(cond){
    message(sprintf("ChatGPT output does not seem to work for prompt %s", prompt_in))
    message(resp$choices[[1]]$message$content)
    return(resp$choices[[1]]$message$content)
  })

  # Return old data with new column
  return(
    data |>
      dplyr::left_join(out, by = c(column_name))|>
      tibble::as_tibble()
  )
}
