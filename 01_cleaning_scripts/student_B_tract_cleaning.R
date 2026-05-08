# =============================================================================
# Student B: Census Tract-Level Eviction Data Cleaning
# =============================================================================
# Input:  00_raw_data/tract_proprietary_valid_2000_2018_y2024m12.csv
# Output: 02_clean_data/ca_tract_evictions.csv
#
# Your job: clean and prepare census tract-level eviction data for California.
# Your cleaned file will be used later to build choropleth maps at the
# tract level. Student A is working in parallel on county-level data.
#
# NOTE: This file is large (~34 MB). read_csv() may take 20-30 seconds.
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Load the raw data
# -----------------------------------------------------------------------------

# col_types speeds up loading and prevents FIPS codes from losing leading zeros
tract_raw <- read_csv(
  "00_raw_data/tract_proprietary_valid_2000_2018_y2024m12.csv",
  col_types = cols(
    fips  = col_character(),
    cofips = col_character(),
    .default = col_guess()
  )
)

glimpse(tract_raw)


# -----------------------------------------------------------------------------
# 2. Filter to California only
# -----------------------------------------------------------------------------

# TASK: Keep only rows where state == "California"
ca_tract <- tract_raw %>%
  filter(_____________)

nrow(ca_tract)
n_distinct(ca_tract$fips)   # how many unique tracts?


# -----------------------------------------------------------------------------
# 3. Verify and fix FIPS codes
# -----------------------------------------------------------------------------

# Census tract FIPS codes should be exactly 11 characters: "06001400100"
#   - First 2 digits: state (06 = California)
#   - Next 3 digits: county
#   - Last 6 digits: tract
#
# The county-level FIPS (cofips) should be exactly 5 characters: "06001"

# Check: are all tract fips codes 11 characters?
ca_tract %>%
  mutate(fips_len = nchar(fips)) %>%
  count(fips_len)

# If any are shorter, pad them:
ca_tract <- ca_tract %>%
  mutate(
    fips   = str_pad(fips,   width = 11, pad = "0"),
    cofips = str_pad(cofips, width = 5,  pad = "0")
  )

# Verify all start with "06"
stopifnot(all(str_starts(ca_tract$fips, "06")))


# -----------------------------------------------------------------------------
# 4. Focus on years 2005–2018
# -----------------------------------------------------------------------------

# Years before 2005 have sparse coverage in California.
# TASK: Filter to year >= 2005

ca_tract <- ca_tract %>%
  filter(_____________)

# How many rows remain?
nrow(ca_tract)


# -----------------------------------------------------------------------------
# 5. Handle the "type" column
# -----------------------------------------------------------------------------

# The variable "type" can be "observed" or "imputed".
# Imputed values were modeled — observed values come from actual court records.

# How many of each?
ca_tract %>% count(type)

# TASK: Keep only "observed" rows for now
ca_tract <- ca_tract %>%
  filter(type == _____________)


# -----------------------------------------------------------------------------
# 6. Create a county name variable (clean version)
# -----------------------------------------------------------------------------

# Remove " County" from county names, same as Student A did
ca_tract <- ca_tract %>%
  mutate(county = str_remove(county, _____________))


# -----------------------------------------------------------------------------
# 7. Flag high-eviction tracts
# -----------------------------------------------------------------------------

# A tract with filing_rate above the 75th percentile is considered
# "high eviction" — useful for mapping and comparison later.

p75 <- quantile(ca_tract$filing_rate, 0.75, na.rm = TRUE)

ca_tract <- ca_tract %>%
  mutate(high_eviction = if_else(filing_rate > p75, 1L, 0L, missing = NA_integer_))

# What share of tract-year observations are "high eviction"?
mean(ca_tract$high_eviction, na.rm = TRUE)


# -----------------------------------------------------------------------------
# 8. Compute a tract-level average across all years
# -----------------------------------------------------------------------------

# For the map, we'll want one row per tract (not one per tract-year).
# Compute the average filing_rate per tract across all available years.

ca_tract_avg <- ca_tract %>%
  group_by(fips, cofips, county, tract) %>%
  summarise(
    avg_filing_rate     = mean(filing_rate, na.rm = TRUE),
    avg_judgement_rate  = mean(judgement_rate, na.rm = TRUE),
    years_observed      = n(),
    .groups = "drop"
  )

glimpse(ca_tract_avg)


# -----------------------------------------------------------------------------
# 9. Save both outputs to 02_clean_data
# -----------------------------------------------------------------------------

# Panel version (one row per tract-year) — for trend analysis
write_csv(ca_tract,     "02_clean_data/ca_tract_evictions_panel.csv")

# Averaged version (one row per tract) — ready to join with shapefile for mapping
write_csv(ca_tract_avg, "02_clean_data/ca_tract_evictions_map.csv")

message("Done! Saved:")
message("  ca_tract_evictions_panel.csv — ", nrow(ca_tract), " rows")
message("  ca_tract_evictions_map.csv   — ", nrow(ca_tract_avg), " rows (one per tract)")
