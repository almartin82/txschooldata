# Texas School Data Expansion Research

**Last Updated:** 2025-01-10 **Theme Researched:** Assessment (STAAR)

## Executive Summary

**⚠️ CRITICAL FINDING:** Texas assessment data access has fundamentally
changed. Starting with 2024 data, TEA has **discontinued downloadable
aggregate data files** and moved exclusively to an interactive Research
Portal that **cannot be automated**.

**Recommendation:** Implement STAAR data for **2018-2023 only**. Future
years (2024+) are not feasible for automated collection without
significant manual intervention or browser automation.

------------------------------------------------------------------------

## Data Sources Found

### Source 1: Legacy STAAR Aggregate Data Files (2018-2023)

- **URL:**
  <https://tea.texas.gov/student-assessment/student-assessment-results/staar-aggregate-data>
- **HTTP Status:** 200 (Accessible)
- **Format:** Comma-delimited text files (CSV) and zipped SAS data files
- **Years:** 2018, 2019, 2021, 2022, 2023 (Note: 2020 was COVID year
  with no STAAR)
- **Access:** Direct download from TEA website
- **Levels:** State, Region (20 ESC regions), District (~1,200), Campus
  (~9,000)
- **Update Frequency:** Annually (summer release)

### Source 2: Texas Assessment Research Portal (2024+)

- **URL:** <https://txresearchportal.com/results> (via
  TexasAssessment.gov)
- **HTTP Status:** Requires JavaScript, interactive login
- **Format:** CSV download (but requires manual interaction)
- **Years:** 2024 onwards (2024 data released August 31, 2024)
- **Access:** **NOT PROGRAMMATIC** - Requires:
  1.  Upload .TXT file with CDC (County District Campus) codes
  2.  Manually select variables and demographics
  3.  Generate and download reports
- **Update Frequency:** Annually (August 31 each year)

### Source 3: Analytic Portal

- **URL:** <https://txreports.emetric.net/>
- **HTTP Status:** Requires JavaScript, interactive portal
- **Format:** Interactive web interface only
- **Access:** **NOT PROGRAMMATIC** - No API endpoint
- **Use Case:** Manual data exploration, not automated collection

------------------------------------------------------------------------

## Critical Barrier: 2024+ Data Access

### The Problem

From the TEA website:

> “NOTE: STAAR aggregate data for 2024 will be available in the Research
> Portal on August 31, 2024. **Aggregate data files like the ones below
> will no longer be produced.**”

This means: - **2018-2023:** Direct CSV downloads possible ✅ - **2024+
:** Only through interactive Research Portal ❌

### Research Portal Workflow

To access 2024+ STAAR data, the user must: 1. Navigate to
<https://txresearchportal.com/results> 2. Create a .TXT file with CDC
codes (one per line) 3. Upload the file to the portal 4. Select test
administrations, subjects, student groups 5. Generate report 6. Download
as CSV

**This cannot be automated** without: - Browser automation
(Selenium/Puppeteer) - fragile and against TEA ToS - API access (not
publicly available) - Manual intervention each year

### Technical Barriers

- **JavaScript rendering:** Portal requires JS, static HTML scraping
  won’t work
- **Authentication:** May require TEAL login (Texas Education Agency
  LOGIN)
- **Rate limiting:** Aggressive automation would trigger security blocks
- **Terms of Service:** Automated scraping likely violates TEA usage
  policies

------------------------------------------------------------------------

## Schema Analysis

### File Structure (2018-2023)

**Available files per year:** - **Grade levels:** 3, 4, 5, 6, 7, 8
(English and Spanish versions where applicable) - **EOC assessments:**
Algebra I, English I, English II, Biology, U.S. History - **Geographic
levels:** State, Region, District, Campus - **Formats:** Comma-delimited
text files (.csv) and zipped SAS files (.zip)

**Example file naming:** - `2023_Grade_3_District_csv.txt`
(Comma-delimited) - `2023_Grade_3_District.zip` (Zipped SAS files)

### Variable Documentation

TEA provides “Variables, Formats, and Descriptions” for each year: -
Excel files with variable names, formats, descriptions - Text files with
same information - Available by grade/subject for each year

**Note:** TEA warns: “the number of variables in these files is too
great to import into Microsoft Access or some versions of Microsoft
Excel without significant truncation.”

### Data Dimensions

Based on TEA documentation, each file contains: - **Campus/District
identifiers:** Campus ID, District ID, Campus Name, District Name -
**Demographic groups:** Race/ethnicity, gender, economic status, special
populations - **Performance levels:** Did Not Meet, Approaches, Meets,
Masters Grade Level - **Score columns:** Raw scores, scale scores,
percentages by performance level - **Columns:** 100+ variables per file
(exact count varies by year/grade)

------------------------------------------------------------------------

## Implementation Options

### Option 1: Legacy-Only Implementation (RECOMMENDED)

**Scope:** 2018-2023 STAAR data only

**Pros:** - ✅ Fully automated - ✅ Direct CSV downloads - ✅ No manual
intervention - ✅ Stable, predictable URLs - ✅ 6 years of historical
data

**Cons:** - ❌ Will not have 2024+ data - ❌ Gap will grow each year -
❌ Package becomes less useful over time

**Complexity:** MEDIUM **Estimated effort:** 10-15 hours

**Implementation approach:** 1. Implement
[`get_raw_staar()`](https://almartin82.github.io/txschooldata/reference/get_raw_staar.md)
function for 2018-2023 2. Parse CSV files with column mapping 3. Create
tidy transformation 4. Document limitation (no 2024+ data)

------------------------------------------------------------------------

### Option 2: Hybrid Manual + Automated

**Scope:** 2018-2023 automated, 2024+ via manual import

**Pros:** - ✅ Historical data automated - ✅ Can add new years
manually - ✅ Package remains useful

**Cons:** - ❌ Requires manual work each year - ❌ User must download
files manually - ❌ `import_local_staar()` fallback needed - ❌ Not
truly automated

**Complexity:** MEDIUM **Estimated effort:** 12-18 hours

**Implementation approach:** 1. Implement automated fetch for 2018-2023
2. Create `import_local_staar(path)` function 3. Document manual
download process for 2024+ 4. Add vignette: “How to download STAAR data
from Research Portal”

------------------------------------------------------------------------

### Option 3: Browser Automation (NOT RECOMMENDED)

**Scope:** Attempt to automate Research Portal with Selenium/Puppeteer

**Pros:** - ✅ Could theoretically access 2024+ data

**Cons:** - ❌ Extremely fragile (UI changes break automation) - ❌
Likely violates TEA Terms of Service - ❌ Requires maintenance of
browser automation scripts - ❌ May trigger IP bans/rate limiting - ❌
Requires authentication handling - ❌ High maintenance burden

**Complexity:** VERY HARD **Estimated effort:** 40+ hours (plus ongoing
maintenance)

**Recommendation:** DO NOT PURSUE this approach without explicit TEA
permission.

------------------------------------------------------------------------

### Option 4: Advocate for API Access

**Scope:** Petition TEA to provide API or automated access

**Pros:** - ✅ Long-term solution - ✅ Benefits all researchers/analysts

**Cons:** - ❌ No guarantee of success - ❌ Timeline uncertain
(months/years) - ❌ Requires political advocacy

**Approach:** 1. Contact TEA Student Assessment Division 2. Request API
access or bulk download option 3. Cite research use case 4. Get other
researchers to support request

------------------------------------------------------------------------

## Recommended Implementation: Option 1 (Legacy-Only)

Given the barriers, I recommend **implementing STAAR data for 2018-2023
only**.

### Justification

1.  **6 years of data is valuable:** Researchers can analyze COVID
    impacts, pre/post pandemic trends
2.  **Fully automated:** No manual intervention required
3.  **Low maintenance:** Stable URLs, no fragile automation
4.  **Clear limitation:** Users will understand data stops at 2023
5.  **Future-proof:** If TEA adds API access, can extend then

### Implementation Plan

#### Phase 1: Schema Discovery (2 hours)

- Download 3 sample files (2018, 2021, 2023)
- Document column differences between years
- Create column mapping tables
- Identify STAAR-specific ID formats

#### Phase 2: Raw Data Fetch (3 hours)

- Implement `get_raw_staar(end_year, grade, level, subject)`
- Handle different grades (3-8) and EOC subjects
- Handle Spanish vs English assessments
- Cache downloaded files

#### Phase 3: Data Processing (4 hours)

- Implement `process_staar(raw_data)`
- Standardize column names across years
- Handle schema changes
- Create consistent tidy format

#### Phase 4: Testing (3 hours)

- Write fidelity tests for each year (2018, 2019, 2021, 2022, 2023)
- Verify major districts present (Houston, Dallas, Austin, Fort Worth)
- Validate data quality (no negatives, reasonable ranges)
- Cross-check against published state totals

#### Phase 5: Documentation (2 hours)

- Update README with STAAR function examples
- Document 2024+ limitation
- Add vignette showing STAAR analysis workflow
- Create “How it works” section

**Total estimated effort:** 14 hours

------------------------------------------------------------------------

## Test Requirements

### Raw Data Fidelity Tests Needed

For each year (2018, 2019, 2021, 2022, 2023): - **Year YYYY:**
District-level total tested count matches raw file - **Year YYYY:**
State-level Masters Grade Level % matches raw file - **Year YYYY:**
Major district (e.g., Houston ISD) counts verified

### Data Quality Checks

- No negative test counts
- No percentages \> 100% or \< 0%
- All major districts present (Houston ISD, Dallas ISD, Austin ISD)
- State totals in reasonable range (3.5-4 million students tested)
- Column consistency across years

------------------------------------------------------------------------

## Time Series Heuristics

### Expected Statewide Totals (Based on Enrollment)

| Year | Students Tested | Notes              |
|------|-----------------|--------------------|
| 2018 | ~3.6 million    | Pre-COVID baseline |
| 2019 | ~3.7 million    | Pre-COVID baseline |
| 2020 | N/A             | COVID - no STAAR   |
| 2021 | ~3.6 million    | COVID rebound year |
| 2022 | ~3.7 million    | Near-normal        |
| 2023 | ~3.8 million    | Full recovery      |

### Red Flags

- Year-over-year change \> 15% (indicates data issue)
- Missing major districts (top 10 by enrollment)
- Sum of districts ≠ state total
- Negative test counts or percentages
- Masters Grade Level \> 50% statewide (unrealistic)

------------------------------------------------------------------------

## MAJOR DISCOVERY: TAPR Contains STAAR Data!

**BREAKING:** After deep investigation, I discovered that **TAPR (Texas
Academic Performance Reports) INCLUDES STAAR assessment outcomes!**

From the official TEA documentation: \> “The TAPR must include… **State
of Texas Assessments of Academic Readiness (STAAR) outcomes by specific
student groups**”

### TAPR Data Download Endpoint

**URL:**
`https://rptsvr1.tea.texas.gov/perfreport/tapr/tapr_dd_download.html?year=2024`

**What it provides:** - **STAAR performance** by grade, subject area,
and performance level - Numerators, denominators, and rates
(percentages) - Data for all campuses, districts, and ESCs in Texas -
Available in **Excel, comma-delimited (CSV), or tab-delimited format** -
Includes student group breakdowns (demographics, special populations)

**Access method:** Interactive web form (appears to use POST requests)

**Years available:** 2012-13 through 2023-24 (latest)

### TAPR SAS Broker Pattern

The enrollment data successfully uses this SAS broker pattern:

    https://rptsvr1.tea.texas.gov/cgi/sas/broker/{DATASET}?key={KEYS}&{ID_PARAM}&prgopt={PATH}

For TAPR reports, similar SAS broker URLs exist:

    https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2024&lev=C&id={CAMPUS_ID}&prgopt=reports/tapr/paper_tapr.sas

**Question:** Is there a `getdata.sas` equivalent for TAPR/STAAR data
download (not just PDF reports)?

### Next Steps Required

1.  **Explore the TAPR data download form** to find the POST endpoint
    and parameters
2.  **Test if there’s a SAS broker `getdata.sas` equivalent** for TAPR
    data
3.  **Document the exact STAAR variables available** in TAPR vs. legacy
    STAAR aggregate files
4.  **Compare data granularity** between TAPR STAAR and legacy STAAR
    aggregate files

**This could be the solution!** If we can programmatically access TAPR
data downloads, we can get current STAAR data without the Research
Portal.

------------------------------------------------------------------------

## Alternative Data Sources to Consider

### Legacy STAAR Aggregate Files (2018-2023)

**Status:** Direct downloads available (as documented above)

**What it includes:** - Raw score distributions - All variables (100+
columns) - More granular than TAPR

**Advantage:** More detailed data **Limitation:** Only available
2018-2023

### TAPR (Texas Academic Performance Reports)

**Status:** Already partially implemented in txschooldata package via
TAPR enrollment data

**What it includes:** - **STAAR outcomes** by student groups (NEW
DISCOVERY!) - STAAR participation rates - Performance by student group -
Aggregate performance (rates, numerators, denominators)

**Advantage:** Continues to be produced, accessible via TAPR system
**Limitation:** Less granular than legacy STAAR aggregate files (likely)

### A-F Accountability Data

**Status:** Available via TEA Accountability system

**What it includes:** - Domain I: Student Achievement (STAAR-based) -
Domain II: School Progress - Domain III: Closing the Gaps

**Advantage:** Continues to be produced annually

**Limitation:** Highly processed for accountability, not raw assessment
data

------------------------------------------------------------------------

## Conclusion

**Bottom Line:** Texas STAAR assessment data may be accessible through
**TAPR (Texas Academic Performance Reports)** which includes STAAR
outcomes. This is a MAJOR discovery that changes the implementation
landscape.

### Three Potential Paths Forward:

**Path 1: TAPR Data Download (PROMISING - Requires Investigation)** -
**Years:** 2012-13 through 2023-24 (current!) - **Data:** STAAR outcomes
by student group, grade, subject, performance level - **Format:**
Excel/CSV/tab-delimited - **Status:** Interactive web form - need to
reverse-engineer the POST endpoint - **Effort:** 8-12 hours to explore
and implement - **Risk:** Medium - may not find direct programmatic
access

**Path 2: Legacy STAAR Aggregate Files (2018-2023 Only)** - **Years:**
2018, 2019, 2021, 2022, 2023 (no 2020 COVID year) - **Data:** Full raw
score distributions, all variables - **Format:** Direct CSV downloads -
**Status:** Fully automatable - **Effort:** 14 hours to implement -
**Risk:** Low - proven approach similar to enrollment data

**Path 3: Hybrid Approach** - Implement Path 1 (TAPR) for current data -
Implement Path 2 (Legacy) for historical depth - **Effort:** 20-25 hours
total - **Best of both worlds:** Current data + historical detail

### Recommendation:

**NEXT STEP:** Spend 2-3 hours exploring the TAPR data download form to
see if there’s a programmatic endpoint. If yes, pursue Path 1 or Path 3.
If no, fall back to Path 2.

The discovery that TAPR includes STAAR data is **game-changing** and may
allow us to access current assessment data without the Research Portal!

------------------------------------------------------------------------

## Sources

- [STAAR Aggregate Data - Texas Education
  Agency](https://tea.texas.gov/student-assessment/student-assessment-results/staar-aggregate-data)
- [Where can I download STAAR data? - TEA
  Knowledgebase](https://texaseducationagency.livehelpnow.net/39795/kb/article/107268/where-can-i-download-staar-data)
- [Data File Formats - TEA Student
  Assessment](https://tea.texas.gov/student-assessment/student-assessment-results/data-file-formats)
- [Texas Assessment Research
  Portal](https://txresearchportal.com/results)
- [Analytic Portal](https://txreports.emetric.net/)
