# STAAR Aggregate Data Schema Documentation

## Overview
The Texas STAAR (State of Texas Assessments of Academic Readiness) aggregate data files are comma-delimited text files containing assessment results at district and campus levels for grades 3-8 and end-of-course (EOC) exams.

## File Naming Convention
```
{level}fy{year}{grade}{subject}.dat
```
- `level`: `d` (district) or `c` (campus)
- `year`: 2-digit fiscal year (e.g., `23` = 2023)
- `grade`: `3`-`8` or EOC subject codes
- `subject`: `e` (English) or `s` (Spanish)

**Examples:**
- `dfy23e3.dat` = District, FY2023, Grade 3, English
- `cfy21e5.dat` = Campus, FY2021, Grade 5, English

## Available Years
- **2018**: Legacy format (1,989 columns)
- **2019**: Not available in legacy format
- **2020**: No assessments (COVID-19 pandemic)
- **2021**: Expanded format (2,091 columns) - added 504 indicators and LEP codes
- **2022**: Same as 2021
- **2023**: Same as 2021 but with RLA_ prefix instead of r_ (2,065 columns)

## Key Columns (First 5)
1. `DISTRICT` or `CAMPUS` - Entity ID code
2. `YEAR` - 2-digit fiscal year
3. `REGION` - Education service center region
4. `DNAME` or `CNAME` - Entity name
5. `GRADE` - Grade level (03-08)

## Schema Changes by Year

### 2018 Format
- **Column prefix**: `r_` for Reading/Language Arts, `m_` for Math
- **LEP codes**: 5 categories (`c`, `f`, `s`, `0`, `v`)
- **504 indicators**: NOT present
- **Total columns**: 1,989

### 2021-2022 Format (Expanded)
- **Column prefix**: `RLA_` for Reading/Language Arts, `m_` for Math
- **LEP codes**: 8 categories (`c`, `f`, `s`, `t`, `r`, `e`, `0`, `v`)
  - Added: `t` (Transitional Bilingual/ESL), `r` (Refugee), `e` (English as a Second Language)
- **504 indicators**: Added (`y504`, `n504`, `v504`)
- **Total columns**: 2,091

### 2023 Format
- **Column prefix**: `RLA_` for Reading/Language Arts, `m_` for Math
- Same structure as 2021-2022
- **Total columns**: 2,065

## Column Naming Convention
```
{subject}_{group}_{metric}
```

### Subject Prefixes
- `RLA_` or `r_`: Reading/Language Arts
- `m_`: Mathematics

### Student Groups
- `all`: All students
- `sexm`/`sexf`/`sexv`: Male/Female/Not reported
- `ethh`/`ethi`/`etha`/`ethb`/`ethp`/`ethw`/`eth2`/`ethv`: Ethnicity codes
  - `h`: Hispanic, `i`: American Indian, `a`: Asian, `b`: Black, `p`: Pacific Islander, `w`: White, `2`: Two or more races, `v`: Not reported
- `ecoy`/`econ`/`eco1`/`eco2`/`eco9`/`ecov`: Economic status
  - `y`: Economically disadvantaged, `n`: Not economically disadvantaged, `1`: Title I, `2`: Non-Title I, `9`: Title I eligible, `v`: Not reported
- `ti1y`/`ti1n`/`ti10`/`ti16`/`ti17`/`ti18`/`ti19`/`ti1v`: Title I participation
- `migy`/`mign`/`migv`: Migrant status
- `lepc`/`lepf`/`leps`/`lept`/`lepr`/`lepe`/`lep0`/`lepv`: LEP status codes
  - `c`: TSBIE exemption, `f`: Parental denial, `s`: ESL, `t`: Transitional Bilingual/ESL, `r`: Refugee, `e`: ELL, `0`: Not LEP, `v`: Not reported
- `bily`/`biln`/`bil2`/`bil3`/`bil4`/`bil5`/`bilv`: Bilingual status
- `esly`/`esln`/`esl2`/`esl3`/`eslv`: ESL status
- `esbiy`/`esbin`/`esbiv`: ESL/Bilingual status
- `spey`/`spen`/`spev`: Special education
- `y504`/`n504`/`v504`: Section 504 status (2021+ only)
- `gify`/`gifn`/`gifv`: Gifted and talented
- `atry`/`atrn`/`atrv`: At-risk status

### Metrics
- `_d`: Total tested count
- `_docs_n`: Documents served count
- `_abs_n`: Absent count
- `_oth_n`: Other exemptions count
- `_unsatgl_nm`: Did Not Meet Grade Level count
- `_approgl_nm`: Approaches Grade Level count
- `_meetsgl_nm`: Meets Grade Level count
- `_mastrgl_nm`: Masters Grade Level count
- `_unsatgl_rm`: Did Not Meet Grade Level rate
- `_approgl_rm`: Approaches Grade Level rate
- `_meetsgl_rm`: Meets Grade Level rate
- `_mastrgl_rm`: Masters Grade Level rate
- `_rs`: Raw score (average)
- `_avg_cat1` through `_avg_cat4`: Category averages (writing only)
- `_pct_cat1` through `_pct_cat4`: Category percentages (writing only)

## Performance Levels
1. **Did Not Meet Grade Level** (`unsatgl`) - Below satisfactory
2. **Approaches Grade Level** (`approgl`) - Satisfactory
3. **Meets Grade Level** (`meetsgl`) - Proficient
4. **Masters Grade Level** (`mastrgl`) - Advanced

## Data Quality Notes
- Small cell counts (<5) are suppressed and appear as blank or `*`
- Percentages are calculated from unrounded counts
- Rates may not sum to totals due to rounding and suppression

## Sample Districts for Testing
- **Houston ISD**: 101901 / 101912 (campus/district code varies by year)
- **Dallas ISD**: 057905
- **Austin ISD**: 227901

## References
- TEA STAAR Aggregate Data: https://tea.texas.gov/student-assessment/student-assessment-results/staar-aggregate-data/
- STAAR Data Glossary: Available from TEA assessment website
