# biographR (development version)

* Initial Github package.

# biographR 0.0.0.9001

* Added function [clean_columns()] to post-process output from [get_bio()] using ChatGPT
* Updated vignette for [get_bio()] function

# biographR 0.0.0.9002

* Added function [get_bio_function_call()] to use function calling for data extraction
* Added function [clean_columns_function_call()] to use function calling for column cleaning
* Added option for few-shot prompting for [get_bio()], [get_bio_function_call()], [clean_columns()], and [clean_columns_function_call()]
* Added option to return biographical information about multiple individuals from one biographical text in [get_bio()]
* Added optional random seed for ChatGPT output through openai_seed argument
* Added article about improving model outputs
* Added article for [clean_columns()]
* Updated article for [get_bio()]
