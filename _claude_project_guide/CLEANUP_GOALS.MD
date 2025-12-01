# Cleanup Goals

## Current State Problems

1. **Code scattered across multiple locations**: R code exists in packages, loose R files, and embedded in Rmd files
2. **Inconsistent documentation**: Mix of old manual docs, partial roxygen2, and undocumented functions
3. **No systematic testing**: testthat files exist but are mostly empty
4. **Dead code and experiments**: ~10 years of exploratory work mixed with production code
5. **Multiple paper versions**: 9 different paper drafts with associated analysis scripts
6. **Massive unmanaged data**: Thousands of simulation outputs with no clear organization
7. **Package interdependencies unclear**: Not sure which packages depend on which
8. **No version control**: Code exists only in local directories, not in git/GitHub

## End State Vision

### GitHub Repositories (New Structure)

**Repo 1: `conserv-plan-core`**
- Core algorithms for conservation planning under uncertainty
- Fully documented with roxygen2
- Comprehensive testthat coverage (>80% of key functions)
- Clean DESCRIPTION, NAMESPACE, proper R package structure
- GitHub Actions CI for automated testing
- Vignettes showing basic usage

**Repo 2: `conserv-plan-uncertainty`**  
- Uncertainty quantification tools
- Depends on: conserv-plan-core
- Same documentation/testing standards as core

**Repo 3: `conserv-benchmark-problems`**
- Benchmark problem dataset for evaluating planning software
- Data package with documentation
- Includes problem definitions, solutions, evaluation metrics
- Separate from algorithms (for independent use by other researchers)

**Repo 4: `conserv-analysis-paper`** (optional - may stay private)
- Analysis scripts and Rmd files for published papers
- Uses the above packages as dependencies
- Reproducible workflow from raw data to figures
- Clear separation: data inputs → analysis → outputs

### Code Quality Standards

**Documentation:**
- All exported functions have complete roxygen2 docs
- @param, @return, @examples for each function
- Package-level documentation (?packagename)
- README.md with installation and quick start

**Testing:**
- testthat tests for all exported functions
- Tests cover: normal cases, edge cases, error handling
- At least 80% coverage of key algorithmic functions
- Tests run automatically on GitHub Actions

**Code Style:**
- Consistent tidyverse-style formatting
- Functions do one thing well (single responsibility)
- Magic numbers replaced with named constants or parameters
- Clear variable names (no single letters except loop indices)

**Dependencies:**
- Explicit DESCRIPTION imports (no hidden library() calls)
- Minimize dependencies where possible
- Document why each dependency is needed

## What Gets Removed/Archived

**Delete Entirely:**
- Empty or stub directories from failed experiments
- Old .Rhistory, .RData files
- Duplicate copies of code ("_backup", "_old" directories)
- Paper drafts v1-v8 (keep only v9 final)

**Archive (Move to `_archive/` subdirectory):**
- Early experiment code (for historical reference if needed)
- Old paper versions (in case reviewer comments are useful)
- Original manual documentation (before roxygen2)
- Keep one exemplar of old analysis, delete rest

**Migrate to New Structure:**
- Extract reusable functions from Rmd → package functions
- Consolidate scattered R files into appropriate packages
- Move working analysis scripts to conserv-analysis-paper repo

## New Functionality to Add

**Priority 1 (Essential for publication):**
- Parallel processing wrapper for cluster runs (currently ad-hoc)
- Standardized result validation function
- Export functions for common output formats (CSV, GeoJSON)

**Priority 2 (Would be nice):**
- Progress bars for long-running optimizations
- Better error messages with suggested fixes
- Plotting functions for common visualizations

**Priority 3 (Future work):**
- Integration with other conservation planning tools
- Web API for remote job submission
- Interactive visualization dashboard

## Migration Strategy

**Phase 1: Core Package** (Start here)
- Clean pkg_core first (most foundational)
- Add docs and tests
- Create GitHub repo
- Tag v1.0.0 release

**Phase 2: Dependent Packages**
- Update packages that depend on core
- Ensure they use cleaned core package
- Same quality standards

**Phase 3: Benchmark Dataset**
- Extract and document benchmark problems
- Create data package
- Ensure reproducibility

**Phase 4: Analysis Code**
- Update Rmd files to use cleaned packages
- Verify all paper figures can be reproduced
- Document data processing workflow

## Success Criteria

We're done when:
- [ ] All packages pass `R CMD check` with no errors/warnings
- [ ] Test coverage >80% on key functions
- [ ] Every exported function has complete roxygen2 docs
- [ ] All 4 GitHub repos created with README and CI
- [ ] At least one vignette per package showing usage
- [ ] Published paper analysis is reproducible from cleaned code
- [ ] No more "TODO" or "FIXME" comments in production code
- [ ] Can install and use packages without cryptic errors

## Out of Scope (Explicitly NOT doing)

- Not rewriting algorithms (unless bugs found)
- Not adding major new features (except Priority 1 list above)
- Not supporting legacy paper versions
- Not preserving backward compatibility with old undocumented functions
- Not creating a GUI (command-line/R only)

## Questions to Resolve Before Starting

1. Should conserv-plan-uncertainty be merged into core, or stay separate?
2. Keep cluster management code in package, or as standalone scripts?
3. What to do with the symlinked Data directory?
4. Are there any functions currently used by collaborators that must stay?