## INDIA

# Load necessary libraries
library(readxl)
library(dplyr)

# Step 1: Read the Excel file
df_india <- read_excel("data/India.xlsx", sheet = "India Carbon Tax")

# Step 2: Rename and select the relevant columns
df_clean <- df_india %>%
    rename(
        Technological = starts_with("Technological"),
        Economic = starts_with("Economic"),
        Social = starts_with("Social"),
        Institutional = starts_with("Institutional")
    ) %>%
    select(Technological, Economic, Social, Institutional)

# Step 3: Simplify response strings to standard labels
simplify_response <- function(x) {
    case_when(
        grepl("Not a barrier", x, ignore.case = TRUE) ~ "Not a barrier",
        grepl("Small", x, ignore.case = TRUE) ~ "Small",
        grepl("Moderate", x, ignore.case = TRUE) ~ "Moderate",
        grepl("Significant", x, ignore.case = TRUE) ~ "Significant",
        TRUE ~ NA_character_
    )
}

df_clean <- df_clean %>%
    mutate(across(everything(), simplify_response)) %>%
    na.omit()

# Step 4: Convert to list format
surveyresponse_list_india <- lapply(df_clean, as.character)

# Step 5 (optional): Save to file for reuse
saveRDS(surveyresponse_list_india, "data/surveyresponse_list_india.rds")




## CHINA

# the correct chart-aligned version
surveyresponse_list_china <- list(
    Technological = c(
        rep("Not a barrier", 2),
        rep("Small", 10),
        rep("Moderate", 7),
        rep("Significant", 11)
    ),
    Economic = c(
        rep("Not a barrier", 1),
        rep("Small", 4),
        rep("Moderate", 14),
        rep("Significant", 11)
    ),
    Social = c(
        rep("Not a barrier", 1),
        rep("Small", 4),
        rep("Moderate", 14),
        rep("Significant", 11)
    ),
    Institutional = c(
        rep("Not a barrier", 3),
        rep("Small", 6),
        rep("Moderate", 9),
        rep("Significant", 12)
    )
)

saveRDS(surveyresponse_list_china, "data/surveyresponse_list_china.rds")

