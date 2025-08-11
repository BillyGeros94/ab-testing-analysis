prepare_data <- function(data) {
  
     # Transform to factors
     data <- data|>
       mutate(
         group = factor(group),
         landing_page = factor(landing_page)
       )
  
     # Print mismatches for inspection
     cat("\nMismatches found:\n")
     print(data |>
             mutate(mismatches = case_when(
               group == "control" & landing_page == "new_page"  ~ "Control Mismatch",
               group == "treatment" & landing_page == "old_page" ~ "Treatment Mismatch",
               TRUE ~ NA_character_
             )) |>
             filter(!is.na(mismatches)) |>
             count(mismatches))
  
     # Exclude mismatches
     data <- data |>
       filter(!(
         (group == "control" & landing_page == "new_page") |
           (group == "treatment" & landing_page == "old_page")
       ))
  
     # Print duplicates for inspection
     cat("\nDuplicate user_ids found:\n")
     print(data |>
             count(user_id) |>
             filter(n > 1))
  
     # Drop duplicates
     data <- data |>
       arrange(user_id, timestamp) |>
       distinct(user_id, .keep_all = TRUE)
  
     # Return the cleaned data
     return(data)
}