# Claude Code Instructions

## Git Commits and PRs

- NEVER reference Claude, Claude Code, or AI assistance in commit
  messages
- NEVER reference Claude, Claude Code, or AI assistance in PR
  descriptions
- NEVER add Co-Authored-By lines mentioning Claude or Anthropic
- Keep commit messages focused on what changed, not how it was written

------------------------------------------------------------------------

## Git Workflow (REQUIRED)

### Feature Branch + PR + Auto-Merge Policy

**NEVER push directly to main.** All changes must go through PRs with
auto-merge:

``` bash
# 1. Create feature branch
git checkout -b fix/description-of-change

# 2. Make changes, commit
git add -A
git commit -m "Fix: description of change"

# 3. Push and create PR with auto-merge
git push -u origin fix/description-of-change
gh pr create --title "Fix: description" --body "Description of changes"
gh pr merge --auto --squash

# 4. Clean up stale branches after PR merges
git checkout main && git pull && git fetch --prune origin
```

### Branch Cleanup (REQUIRED)

**Clean up stale branches every time you touch this package:**

``` bash
# Delete local branches merged to main
git branch --merged main | grep -v main | xargs -r git branch -d

# Prune remote tracking branches
git fetch --prune origin
```

### Auto-Merge Requirements

PRs auto-merge when ALL CI checks pass: - R-CMD-check (0 errors, 0
warnings) - Python tests (if py{st}schooldata exists) - pkgdown build
(vignettes must render)

If CI fails, fix the issue and push - auto-merge triggers when checks
pass.

------------------------------------------------------------------------

## Local Testing Before PRs (REQUIRED)

**PRs will not be merged until CI passes.** Run these checks locally
BEFORE opening a PR:

### CI Checks That Must Pass

| Check        | Local Command                                                                  | What It Tests                                  |
|--------------|--------------------------------------------------------------------------------|------------------------------------------------|
| R-CMD-check  | `devtools::check()`                                                            | Package builds, tests pass, no errors/warnings |
| Python tests | `pytest tests/test_pytxschooldata.py -v`                                       | Python wrapper works correctly                 |
| pkgdown      | [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html) | Documentation and vignettes render             |

### Quick Commands

``` r
# R package check (required)
devtools::check()

# Python tests (required)
system("pip install -e ./pytxschooldata && pytest tests/test_pytxschooldata.py -v")

# pkgdown build (required)
pkgdown::build_site()
```

### Pre-PR Checklist

Before opening a PR, verify: - \[ \] `devtools::check()` — 0 errors, 0
warnings - \[ \] `pytest tests/test_pytxschooldata.py` — all tests
pass - \[ \]
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
— builds without errors - \[ \] Vignettes render (no `eval=FALSE`
hacks) - \[ \] All new exported functions added to `_pkgdown.yml`
reference index

------------------------------------------------------------------------

## pkgdown Configuration (CRITICAL)

**Always update `_pkgdown.yml` when adding exported functions, or CI
will fail.**

### The Problem

When you add functions with `@export` roxygen tags, pkgdown requires
them to be listed in `_pkgdown.yml` under the `reference:` section. If
they’re missing, pkgdown CI fails with:

    In _pkgdown.yml, N topics missing from index: "func1", "func2", "func3"
    Either add to the reference index, or use `@keywords internal` to drop from the index.

### The Fix

Add new functions to `_pkgdown.yml` in logical groupings:

``` yaml
reference:
- title: Fetch Data
  desc: Download data from TEA
  contents:
  - fetch_enr
  - fetch_grad      # Add new functions here!
  - fetch_staar

- title: Process & Tidy
  desc: Transform data into analysis-ready formats
  contents:
  - tidy_enr
  - tidy_grad       # And here!
  - tidy_staar
```

### Alternative: Mark as Internal

If a function is internal/helper (not user-facing), add
`@keywords internal` to exclude it from documentation:

``` r
#' @keywords internal
#' @export
internal_helper_function <- function() {
  # This won't appear in pkgdown reference
}
```

### Workflow Checklist

When adding new exported functions: 1. Write function with `@export` tag
2. Run `devtools::document()` to regenerate `.Rd` files 3. Add function
to appropriate section in `_pkgdown.yml` 4. Run
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
to verify build works 5. Commit both code changes and `_pkgdown.yml`
updates together

**Lessons learned:** - Graduation rate PR (#10) failed CI for this
reason - STAAR PR (#11) initially failed for the same reason - Local
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
testing catches this before pushing - This is a systematic workflow
issue, not a one-time mistake

------------------------------------------------------------------------

## README Images from Vignettes (REQUIRED)

**NEVER use `man/figures/` or `generate_readme_figs.R` for README
images.**

README images MUST come from pkgdown-generated vignette output so they
auto-update on merge:

``` markdown
![Chart name](https://almartin82.github.io/{package}/articles/{vignette}_files/figure-html/{chunk-name}-1.png)
```

**Why:** Vignette figures regenerate automatically when pkgdown builds.
Manual `man/figures/` requires running a separate script and is easy to
forget, causing stale/broken images.
