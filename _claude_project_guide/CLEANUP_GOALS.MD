# Cleanup Goals

## Current State Problems

A. Plotting
1. **Some plots do not have SA above SA_SS in right hand column**: Prediction plots do correctly have SA over SA_SS
2. **Need way to convey point density in heavily overplotted regions of 4 panel plots**: Might need histograms on axes or contours on plots or ???

B. Predictive model fitting
3. **No coherent strategy for test/train split in final paper**: Have some code for separating problems into independent groups but not for aggregating split tests
4. **Overly complicated code for model fitting**: Functions so deeply nested that it's hard to understand and modify
5. **Need a way to compute & show prediction intervals, not confidence intervals on predictions**: Conventional prediction intervals require assumptions that we violate, so maybe there is another way using bootstrapping, or???

C. Benchmark problem repository management
6. **Massive unmanaged data needs code for extracting data subsets & applying future actions to problems**: Thousands of simulation outputs with no functions to walk through data and apply actions

D. Poor documentation and testing
8. **Poor documentation of meaning of network variables used**: Many variables from bipartite package whose meaning is poorly understood and documented
9. **Poor documentation of sequence of actions to build input data**: 
2. **Poor documentation of functions**: Mix of some inline comments, partial roxygen2, and undocumented functions
12. **Poor documentation of file input file and transformed file contents**: Project starts with over 200 columns and progressively strips away many of them
3. **No systematic testing**: testthat files do not exist for functions in R files

E. Streamlining
4. **Dead code and unused data columns**: ~10 years of exploratory work shows in form of many unused columns & much old, unused code

## End State Vision

### Cleaned and streamlined R code

**R directory**
- Removal of unused functions
- Functions fully documented with roxygen2

**Rmd file(s)`**  
- Paper 9 manuscript
- Paper 9 data building

### Code Quality Standards

**Documentation:**
- All functions in R directory have complete roxygen2 docs
- @param, @return, @examples for each function

**Testing:**
- testthat tests for functions in R directory
- Tests cover: normal cases, edge cases, error handling

**Code Style:**
- Functions do one thing well (single responsibility)
- Magic numbers replaced with named constants or parameters
- Clear variable names (no single letters except loop indices)

## New Functionality to Add

**Priority 1 (Essential for publication):**
- Fix variable order in plots so that SA is always above SA_SS
- Develop test/train method to have independent problems within sets
- Develop method to aggregate results across these independent test/train sets

**Priority 2 (Would be nice):**
- Develop method for computing meaningful prediction intervals, possibly using bootstrap
- Cleaner, more transparent calling of learning algorithms for prediction of error
- Clean up and document building of input files from raw data

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