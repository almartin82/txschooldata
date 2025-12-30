# Claude Code Instructions for txschooldata

## Commit and PR Guidelines

- Do NOT include “Generated with Claude Code” in commit messages
- Do NOT include “Co-Authored-By: Claude” in commit messages
- Do NOT mention Claude or AI assistance in PR descriptions
- Keep commit messages clean and professional

## Project Context

This is an R package for fetching and processing Texas school data from
the Texas Education Agency (TEA).

### Key Data Characteristics

- **Data Source**: Texas Education Agency (TEA) at
  <https://tea.texas.gov/reports-and-data>
- **ID System**:
  - District IDs: 6 digits (e.g., 101912 for Austin ISD)
  - Campus IDs: 9 digits (district ID + 3-digit campus number)
- **Primary Data System**: PEIMS (Public Education Information
  Management System)
- **Number of Districts**: ~1,209

## Package Structure

The package follows the same patterns as ilschooldata: -
`fetch_enrollment.R` - Main user-facing function -
`get_raw_enrollment.R` - Download raw data from TEA -
`process_enrollment.R` - Process raw data into standard schema -
`tidy_enrollment.R` - Transform to long format - `cache.R` - Local
caching functions
