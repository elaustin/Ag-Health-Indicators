# Download fatality data from www.bls.gov and save as one Excel file per state,
# then merge all four datasets and save as one CSV file

# Attach packages, installing as needed
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, httr, rvest, readxl, tidyxl, dplyr, tidyr, stringr, 
               purrr, readr, tidycensus)

# Set options
options(readr.show_col_types = FALSE, readr.show_progress = FALSE)

# Define variables
data_dir <- here("Fatality Data NW")
state_abb <- c('AK', 'ID', 'OR', 'WA')
file_base_name <- "bls-fatal-aff-11-21"
output_file_name <- paste0(file_base_name, ".csv")

# Create data folder if missing
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Get FIPS codes and create file names for saving state XLSX files
nw_states <- fips_codes %>% as_tibble() %>% 
  distinct(state, state_code, state_name) %>% 
  filter(state %in% state_abb) %>% arrange(state_code) %>% 
  mutate(row = row_number()) %>% 
  mutate(file_name = paste(row, 
                           tolower(state), 
                           paste0(file_base_name, ".xlsx"), 
                           sep = "-"))

# Get industry codes
ind_codes_fn <- here(data_dir, "ind_codes.csv")
if (!file.exists(ind_codes_fn)) {
  source(here(data_dir, "download_bls_industry_codes.R"))
}
ind_codes <- read_csv(ind_codes_fn)

# Define functions

# Return a vector of URLs that replicate the manual XLSX download process
get_urls <- function(state_code, ind_code) {
  c(
    # (1) Go to starting page
    "https://www.bls.gov/iif/data.htm",
    # (2) Select "MULTI SCREEN" for:
    #     "Occupational Injuries and Illnesses and Fatal Injuries Profiles"  
    "https://data.bls.gov/gqt/InitialPage",
    # (3) Select "Fatal Injuries Numbers" and press "Continue"
    paste0("https://data.bls.gov/gqt/ProfileYears?", 
           "report_id=4&Continue=Continue"),
    # (4) Select "Year": "2021" and "Continue"
    paste0("https://data.bls.gov/gqt/ProfileState?", 
           "year=2021&Continue=Continue"),
    # (5) Select "Area": [State Name], "Beginning year": "2011" and "Continue"
    paste0("https://data.bls.gov/gqt/ProfileSpecs?", 
           "stateCode=S", state_code, "&multipleYears=2011&Continue=Continue"),
    # (6) Select "Characteristic Type": "Industry", 
    #     "Order": "Name or description" and press "Continue"
    paste0("https://data.bls.gov/gqt/ProfileCharacteristics?", 
           "characteristic=MID&order=0&Continue=Continue"),
    # (7) Select "Subcharacteristic": "Agriculture, forestry, [...] GP2AFH",
    #     "Ownership": "All ownerships" and press "Continue"
    paste0("https://data.bls.gov/gqt/ProfileData?", 
           "subcharacteristic=", ind_code, "&ownership=0&Continue=Continue"),
    # (8) "Please select output type": "Generate Excel table"
    paste0("https://data.bls.gov/gqt/RequestData?",
           "LastStep=Generate+Excel+table")
  )
}

# Return a dataframe of indentation levels for 1st column of XLSX file
get_indent <- function(file_path, n_lines_skip) {
  cells <- xlsx_cells(file_path)
  formats <- xlsx_formats(file_path)
  indented_cells <- which(formats$local$alignment$indent > 0)
  cells %>% 
    filter(col == 1, local_format_id %in% indented_cells) %>%
    select(row, local_format_id) %>%
    mutate(row = row - n_lines_skip - 1) %>% 
    mutate(indent = local_format_id - 9) %>%
    select(row, indent)
}

# Create XLSX folder, if missing
if (!dir.exists(here(data_dir, "xlsx"))) { 
   dir.create(here(data_dir, "xlsx"), recursive = TRUE, showWarnings = FALSE)
}

# Fetch data from website and save as XLSX files, one per state
res <- map(ind_codes$code, ~ {
  ind_code = .x
  map(nw_states$row, ~ {
    file_path <- 
      here(data_dir, "xlsx", paste0(ind_code, "-", nw_states$file_name[.x]))
    if (! file.exists(file_path)) {
      urls <- get_urls(nw_states$state_code[.x], ind_code)
      result <- session(urls[1]) %>% 
        session_jump_to(urls[2]) %>% 
        session_jump_to(urls[3]) %>% 
        session_jump_to(urls[4]) %>%
        session_jump_to(urls[5]) %>%
        session_jump_to(urls[6]) %>%
        session_jump_to(urls[7]) %>%
        session_jump_to(urls[8])
      writeBin(result$response$content, file_path)
    }
  })
})

# Set number of lines to skip and include when importing from XLSX
nw_states <- nw_states %>% mutate(n_lines_skip = 3, n_lines_max = 195)

# Join nw_states with ind_codes to get all combinations of rows of each
# and modify filename to include industry code
df_files <- nw_states %>% 
  cross_join(ind_codes) %>% 
  mutate(file_name = file.path("xlsx", paste0(code, "-", file_name))) %>%
  mutate(row = row_number())
  
# Merge XLSX datasets
df_merged <- df_files %>% 
  mutate(data = map(.x = row, .f = ~ {
    # Set file path
    file_path <- here(data_dir, df_files$file_name[.x])
    
    # Get indentation levels for Characteristic variable
    df_indent <- get_indent(file_path, df_files$n_lines_skip[.x])
    
    # Import data from XLSX and cleanup
    suppressMessages(
      read_xlsx(file_path, col_names = TRUE, na = c("", "–"),
                skip = df_files$n_lines_skip[.x], 
                n_max = df_files$n_lines_max[.x])) %>% 
      mutate(row = row_number()) %>%
      left_join(df_indent, by = "row") %>% 
      rename("Characteristic" = "...1", 
             "All industries 2021" = "2021...2",
             "2021" = "2021...13") %>%
      drop_na(Characteristic) %>% 
      mutate(across(.cols = c(Characteristic), 
                    .fns = ~ str_remove(.x, '(?:[()\\d]{2,})?:?$'))) %>% 
      mutate(Char_1 = ifelse(is.na(indent), Characteristic, NA)) %>%
      fill(Char_1) %>%
      mutate(Char_2 = ifelse(indent == 1, Characteristic, NA)) %>%
      group_by(Char_1) %>% fill(Char_2) %>% ungroup() %>% 
      mutate(Char_3 = ifelse(indent == 2, Characteristic, NA)) %>%
      group_by(Char_1, Char_2) %>% fill(Char_3) %>% ungroup() %>%
      mutate(Char_4 = ifelse(indent == 3, Characteristic, NA)) %>%
      group_by(Char_1, Char_2, Char_3) %>% fill(Char_4) %>% ungroup() %>%
      mutate(Char_5 = ifelse(indent == 4, Characteristic, NA)) %>%
      select(c(starts_with('Char_'), Characteristic, 
               `All industries 2021`, `2011`:`2021`))
  })) %>% 
  unnest(c(data)) %>% select(c(-row:-n_lines_max))

# Confirm that you have the first row from all datasets
df_merged %>% 
  filter(Char_1 == "Total", is.na(Char_2)) %>% 
  nrow() == nrow(nw_states) * nrow(ind_codes)

# Confirm that you have the last row from all datasets
df_merged %>% 
  filter(Char_2 == "Military specific occupations", is.na(Char_3)) %>% 
  nrow() == nrow(nw_states) * nrow(ind_codes)

# Save merged dataset
write_csv(df_merged, here(data_dir, output_file_name))

# Save long dataset
df_long <- df_merged %>% 
  select(c(-`All industries 2021`, -state, -state_code, -name)) %>%
  pivot_longer(cols = c(`2011`:`2021`), 
               names_to = 'Year', values_to = 'Fatalities') %>%
  rename("State" = "state_name") %>%
  filter(Char_1 == 'Total' | !is.na(Char_2))
write_csv(df_long, here(data_dir, paste0("long-", output_file_name)))
