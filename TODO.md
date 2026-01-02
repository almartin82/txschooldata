# TODO - txschooldata

## pkgdown Build Issues

### Network Timeout During Vignette Build (2026-01-01)

The pkgdown build fails when rendering `vignettes/data-quality-qa.Rmd` due to network timeout connecting to TEA servers.

**Error:**
```
Failed to render 'vignettes/data-quality-qa.Rmd'.
Quitting from data-quality-qa.Rmd:39-51 [download-multi-year]
Failed to download CSTUD data for year 2020
Error: Timeout was reached [rptsvr1.tea.texas.gov]:
Connection timed out after 10002 milliseconds
```

**Root Cause:**
The vignette downloads real enrollment data from the Texas Education Agency during the build process. When the TEA server is unreachable or network connectivity is poor, the build fails.

**Potential Solutions:**
1. **Pre-cache data**: Run vignettes locally with caching enabled, then commit the cache files so the build doesn't need network access
2. **Use `eval=FALSE`**: Mark data-fetching chunks as `eval=FALSE` with pre-computed output shown
3. **Add retry logic**: Implement retry with exponential backoff in `fetch_enr_multi()`
4. **Mock data for vignettes**: Create sample datasets for documentation purposes that don't require network access

**Status:** Blocked by network connectivity issues. Retry when network is available.
