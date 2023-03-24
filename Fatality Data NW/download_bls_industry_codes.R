# Download Agriculture, forestry, fishing and hunting industry codes from 
# www.bls.gov and save as a CSV file

# Attach packages, installing as needed
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, httr, rvest, dplyr, tidyr, stringr, readr)

# Define variables
data_dir <- here("Fatality Data NW")

# Create data folder if missing
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Define vector of URLs
urls <- c("https://www.bls.gov/iif/data.htm", "https://data.bls.gov/gqt/InitialPage", 
          "https://data.bls.gov/gqt/ProfileYears?report_id=4&Continue=Continue", 
          "https://data.bls.gov/gqt/ProfileState?year=2021&Continue=Continue", 
          "https://data.bls.gov/gqt/ProfileSpecs?stateCode=S53&multipleYears=2011&Continue=Continue", 
          "https://data.bls.gov/gqt/ProfileCharacteristics?characteristic=MID&order=1&Continue=Continue")

# Follow URLs
result <- session(urls[1]) %>%
  session_jump_to(urls[2]) %>%
  session_jump_to(urls[3]) %>%
  session_jump_to(urls[4]) %>%
  session_jump_to(urls[5]) %>%
  session_jump_to(urls[6])

# Extract industry codes
pg <- result$response$content %>% read_html()
sel_opt <- pg %>% html_node("select#subcharacteristic") %>% 
  html_nodes("option") %>% html_text()
sel_opt_affh <- sel_opt[str_detect(sel_opt, '^\\s*(?:GP2AFH|11)')]
ind_codes <- tibble(string = sel_opt_affh) %>%
  separate(string, c("code", "name"), "\\s{2,}") %>% 
  mutate(across(everything(), str_trim))

# Save industry codes
write_csv(ind_codes, here(data_dir, "ind_codes.csv"))
