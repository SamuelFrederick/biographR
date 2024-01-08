################################################################################
# Greedy grouping of columns to make sure that each group of columns has fewer
#   than openai_context_window/1.34 tokens; returns a list with column groups
################################################################################
group_columns <- function(x, openai_context_window = 4096) {
  grp <- c(); unmatched <- c(); out <- list()
  if(length(x)==1) return(list(names(x)))
  for(i in 1:length(x)) {
    if(sum(unlist(x[grp]))+sum(x[[i]])<((openai_context_window)/1.34)) {
      grp <- c(grp, names(x)[i])
    }else{
      unmatched <- c(unmatched, names(x)[i])
    }
  }
  if(length(unmatched)>0) {
    out <- group_columns(x[unmatched], openai_context_window = openai_context_window)
    return(append(list(grp), out))
  }
  return(list(grp))
}
################################################################################
# Return user prompt which contains original input
################################################################################
get_prompt_clean_columns_fc <- function(data, column_name,
                                        fewshot_lgl = F){
  data <- as.list(data)
  sprintf("Put this information into a dataframe. %s",
          as.character(jsonlite::toJSON(lapply(stats::setNames(1:length(column_name), column_name),
                                               function(x) {
                                                 if(fewshot_lgl) {
                                                   data[[column_name[x]]]
                                                 }else{
                                                   sort(unique(data[[column_name[x]]]))
                                                 }
                                               }))))

}
################################################################################
# Return example inputs and outputs including function calls for few-shot prompt
################################################################################
split_fewshot_fc <- function(column_name,
                             prompt_fewshot,
                             prompt_fewshot_n) {

  # Get example outputs and inputs
  c_orig <- prompt_fewshot[column_name]
  c_gpt <- prompt_fewshot[paste(column_name, "_gpt", sep = "")]

  # Error if outputs and inputs not same length
  if(any(unlist(lapply(column_name,
                       function(x) length(c_orig[[x]])!=length(c_gpt[[paste(x, "_gpt", sep = "")]]))))) stop("Example inputs and outputs must be the same length.", call. = F)

  # Error if prompt_fewshot_n too high for example inputs and outputs
  example_n <- min(unlist(lapply(c_orig, function(x) length(x))))
  if(prompt_fewshot_n>example_n) stop("prompt_fewshot_n must be less than or equal to the minimum number of examples.", call. = F)

  # Get part splits for each example input
  c_h <- lapply(stats::setNames(1:length(column_name), column_name),
                function(x) {
                  example_n <- length(c_orig[[column_name[x]]])
                  split(1:example_n, ceiling(1:example_n/(example_n/prompt_fewshot_n)))
                })

  names(c_gpt) <- gsub("_gpt", "", names(c_gpt))

  # Return example input prompts and function call output
  do.call(append, list(lapply(1:prompt_fewshot_n,
                              function(x) {

                                df <- lapply(stats::setNames(1:length(column_name), column_name),
                                             function(y) c_orig[[column_name[y]]][c_h[[column_name[y]]][[x]]])
                                out <- lapply(stats::setNames(1:length(column_name), column_name),
                                              function(y) c_gpt[[column_name[y]]][c_h[[column_name[y]]][[x]]])
                                out <- as.character(jsonlite::toJSON(out))
                                list(
                                  list("role" = "user",
                                       "content" = get_prompt_clean_columns_fc(data = df,
                                                                               column_name = column_name,
                                                                               fewshot_lgl = T)),
                                  list("role" = "assistant",
                                       "tool_calls" = list(
                                         list("id" = "call_qwerty123",
                                              "function" = list(
                                                "name" = "make_dataframe_from_values",
                                                "arguments" = out
                                              ),
                                              "type" = "function"
                                         )
                                       )
                                  ),
                                  list("role" = "tool",
                                       "tool_call_id" = "call_qwerty123",
                                       "name" = "make_dataframe_from_values",
                                       "content" =  out)
                                )
                              }), value = NULL))|>
    unlist(recursive = FALSE)
}
################################################################################
# Calls split_fewshot_fc to get prompts for few-shot examples
################################################################################
get_prompt_clean_columns_fewshot_fc <- function(column_name,
                                                prompt_fewshot,
                                                prompt_fewshot_n) {

  # Error if prompt_fewshot is not data.frame, tibble, or list
  if(!inherits(prompt_fewshot, "data.frame")&
     !inherits(prompt_fewshot, "list")) stop("prompt_fewshot must be a tibble, data.frame, or list.", call. = F)

  # Error if prompt_fewshot_n is not an integer
  if(!is.numeric(prompt_fewshot_n)) stop("prompt_fewshot_n must be an integer.", call. = F)
  if(prompt_fewshot_n%%1!=0) stop("prompt_fewshot_n must be an integer.", call. = F)

  prompt_fewshot <- as.list(prompt_fewshot)

  # Error if names from user input not in prompt_fewshot
  if(any(!column_name%in%names(prompt_fewshot))|any(!paste(column_name, "_gpt", sep = "")%in%names(prompt_fewshot))){
    stop("Names from column_values, column_formats, and column_descriptions must be in prompt_fewshot, and example output must be provided with a name containing _gpt.", call. = F)
  }

  # Get prompts for few-shot examples
  split_fewshot_fc(column_name = column_name,
                   prompt_fewshot = prompt_fewshot,
                   prompt_fewshot_n = prompt_fewshot_n)

}
################################################################################
# Returns full prompt for clean_columns_function_call()
################################################################################
get_prompt_clean_columns_wrapper_fc <- function(output_vector,
                                                output_format,
                                                output_description,
                                                column_name,
                                                data,
                                                prompt_fewshot,
                                                prompt_fewshot_n){

  # Get prompt for prompt_fewshot
  if(!is.null(prompt_fewshot)) prompt_fewshot <- get_prompt_clean_columns_fewshot_fc(column_name = column_name,
                                                                                     prompt_fewshot = prompt_fewshot,
                                                                                     prompt_fewshot_n = prompt_fewshot_n)

  # Get the main prompt for a given column
  main_prompt <- get_prompt_clean_columns_fc(data = data,
                                             column_name = column_name,
                                             fewshot_lgl = F)

  # Return the prompt for ChatGPT
  append(
    append(
      list(
        list("role" = "system",
             "content" = "You are a system for outputting user information as a dataframe. If user information is not in the correct format, reformat it to meet the requirements, and match the user input with the accepted values. The arguments do not all need to have the same length. Your ONLY output should be the tool call function arguments with the proper formatting and values.")
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
# Returns a list with the tool_call prompt
################################################################################
get_tool_call_clean_columns_fc <- function(output_vector,
                                           output_format,
                                           output_description,
                                           column_name) {

  # Combine user values, formats, and descriptions into a user prompt
  output_info <- lapply(stats::setNames(1:length(column_name), column_name),
                        function(x) {
                          vec_in <- ifelse(all(is.null(output_vector[[column_name[x]]])),
                                           "",
                                           sprintf(" Can ONLY contain values in %s.",
                                                   as.character(jsonlite::toJSON(output_vector[[column_name[x]]]))))
                          form_in <- ifelse(all(is.null(output_format[[column_name[x]]])),
                                            "",
                                            sprintf(" MUST HAVE format %s.", output_format[[column_name[x]]]))
                          desc_in <- ifelse(all(is.null(output_description[[column_name[x]]])),
                                            sprintf("ARRAY containing %s for ALL user inputs.", column_name[x]),
                                            output_description[[column_name[x]]])
                          sprintf("%s%s%s", desc_in, vec_in, form_in)
                        })

  # Output function tool call
  list(
    list(
      "type" = "function",
      "function" = list(
        "name" = "make_dataframe_from_values",
        "description" = "Take arrays of values and return those arrays of values in a dataframe format.",
        "parameters" = list(
          "type" = "object",
          "properties" = lapply(stats::setNames(1:length(column_name), column_name),
                                function(x) {
                                  list(
                                    "type" = "string",
                                    "description" = output_info[[column_name[x]]]
                                  )
                                }),
          "required" = as.list(column_name)
        )
      )
    )
  )
}
################################################################################
# Calls ChatGPT API and returns data with processed columns appended
################################################################################
get_gpt_column_function_call <- function(data, column_name,
                                         output_vector,
                                         output_format,
                                         output_description,
                                         prompt_fewshot,
                                         prompt_fewshot_n,
                                         openai_api_key,
                                         openai_model,
                                         openai_temperature,
                                         openai_seed) {

  # Warn user if output_vector is not a character vector
  if(!(all(is.null(output_vector))|
     (inherits(output_vector, "list")&
      all(unlist(lapply(column_name, function(x) is.character(output_vector[[x]])|is.null(output_vector[[x]]))))))) {
    warning("Column values will be coerced to character vector.", call. = F)
    suppressWarnings({output_vector <- lapply(stats::setNames(1:length(output_vector), names(output_vector)),
                                              function(x) as.character(output_vector[[names(output_vector)[x]]]))})
  }

  # Error if output_format is not NULL or a character
  if(!(all(is.null(output_format))|
       (inherits(output_format, "list")&
        all(unlist(lapply(column_name, function(x) is.character(output_format[[x]])|is.null(output_format[[x]]))))))) stop("Column formats must be NULL or a list of strings.", call. = F)

  # Error if output_description is not NULL or a character
  if(!(all(is.null(output_description))|
       (inherits(output_description, "list")&
        all(unlist(lapply(column_name, function(x) is.character(output_description[[x]])|is.null(output_description[[x]]))))))) stop("Column descriptions must be NULL or a character.", call. = F)

  # Error if column not in data
  if(any(is.numeric(column_name))) {
    if(any(unlist(lapply(column_name, function(x) ifelse(is.numeric(x), x>ncol(data)|x<1, F))))) stop("Column names must be character values or numeric values less than or equal to the number of columns.", call. = F)
    column_name <- colnames(data)[column_name]
  }
  if(!all(column_name%in%colnames(data))) stop("All column names must be present in data.", call. = F)

  # Get tool call for API
  tools_in <- get_tool_call_clean_columns_fc(output_vector = output_vector,
                                             output_format = output_format,
                                             output_description = output_description,
                                             column_name = column_name)
  # Get prompt for API
  prompt_in <- get_prompt_clean_columns_wrapper_fc(output_vector = output_vector,
                                                   output_format= output_format,
                                                   output_description = output_description,
                                                   column_name = column_name,
                                                   data = data,
                                                   prompt_fewshot = prompt_fewshot,
                                                   prompt_fewshot_n = prompt_fewshot_n)

  # Make API call to ChatGPT
  resp <- httr2::request("https://api.openai.com/v1/chat/completions")|>
    httr2::req_headers(`Content-Type` = "application/json",
                       Authorization = sprintf("Bearer %s", openai_api_key)) |>
    httr2::req_body_json(list(model = openai_model,
                              messages = prompt_in,
                              tools = tools_in,
                              temperature = openai_temperature,
                              seed = openai_seed,
                              tool_choice = list(
                                "type" = "function",
                                "function" = list(
                                  "name" = "make_dataframe_from_values"
                                )
                              ))) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Message describing API call tokens
  message(sprintf("%s\nInput Tokens: %s\nOutput Tokens: %s\nTotal Tokens: %s",
                  paste(column_name, collapse = ", "),
                  resp$usage$prompt_tokens,
                  resp$usage$completion_tokens,
                  resp$usage$total_tokens))

  # Process output and append processed columns to data
  tryCatch({
    out <- jsonlite::fromJSON(resp$choices[[1]]$message$tool_calls[[1]]$`function`$arguments)
    out <- lapply(stats::setNames(1:length(out), names(out)),
                  function(x){
                    tmp <- list()
                    tmp[[paste(names(out)[x], "_gpt", sep = "")]] <- out[[names(out)[x]]]
                    tmp[[names(out)[x]]] <- sort(unique(data[,names(out)[x]]))
                    as.data.frame(tmp)
                  })
    for(i in 1:length(out)){

      data <- data |>
        dplyr::left_join(out[[i]], by = c(names(out)[i]))
    }

    # Return old data with new columns
    return(tibble::tibble(data))
  }, error = function(cond){
    message("ChatGPT outputted an unconventional format. Few-shot prompting may improve output.")
    return(resp$choices[[1]]$message)
  })
}
