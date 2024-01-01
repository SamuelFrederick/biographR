
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biographR

<!-- badges: start -->
<!-- badges: end -->

The biographR package is designed to help extract structured
biographical data from unstructured text using ChatGPT’s API.

## Installation

You can install the latest version of biographR using the following
code:

``` r
remotes::install_github("SamuelFrederick/biographR")
```

## Example

Say we have a biography for an individual named John Smith. The
biography might look something like:

    John Smith graduated from Nowhere College with a B.A. in 1962. 
    He then went on to receive a Ph.D. from Nonexistent University. 
    At the same time, his wife, Sally Smith, was earning her M.D. from Invisible University.

It would be challenging, if not impossible, to extract structured
biographical data about John Smith from this text without reading the
text and getting the information by hand. Instead, we can use the
`get_bio()` function from the `biographR` package, which calls ChatGPT’s
API to extract the data for us.

``` r
library(biographR)
get_bio(bio = "John Smith graduated from Nowhere College with a B.A. in 1962. He then went on to receive a Ph.D. from Nonexistent University. At the same time, his wife, Sally Smith, was earning her M.D. from Invisible University.", 
        bio_name = "John Smith", 
        prompt_fields = c("college", "graduate_school", 
                          "highest_level_of_education", "gender",
                          "married"))
#> # A tibble: 1 × 5
#>   college         graduate_school          highest_level_of_edu…¹ gender married
#>   <chr>           <chr>                    <chr>                  <chr>  <chr>  
#> 1 Nowhere College Nonexistent University;… Ph.D.                  ""     Sally …
#> # ℹ abbreviated name: ¹​highest_level_of_education
```

We can also customize the output further using the
`prompt_fields_format` and `prompt_fields_values` arguments.
Additionally, while the package defaults to ChatGPT 3.5, ChatGPT 4 often
appears to perform somewhat better in extracting subtler information
(e.g., gender from pronouns).

``` r
get_bio(bio = "John Smith graduated from Nowhere College with a B.A. in 1962. He then went on to receive a Ph.D. from Nonexistent University. At the same time, his wife, Sally Smith, was earning her M.D. from Invisible University.", 
        bio_name = "John Smith", 
        prompt_fields = c("college", "graduate_school", 
                          "highest_level_of_education", "gender",
                          "married"), 
        prompt_fields_format = list(college = "{SCHOOL} - {DEGREE}", 
                                    graduate_school = "{SCHOOL} - {DEGREE}", 
                                    highest_level_of_education = "{DEGREE}"), 
        prompt_fields_values = list(married = c("Yes", "No")), 
        openai_model = "gpt-4")
#> # A tibble: 1 × 5
#>   college                graduate_school   highest_level_of_edu…¹ gender married
#>   <chr>                  <chr>             <chr>                  <chr>  <chr>  
#> 1 Nowhere College - B.A. Nonexistent Univ… Ph.D.                  Male   Yes    
#> # ℹ abbreviated name: ¹​highest_level_of_education
```
