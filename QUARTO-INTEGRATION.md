# Quarto Integration for Gluey

This document outlines the approaches explored for integrating gluey syntax with Quarto documents, and provides a roadmap for future development.

## Current State

### R Markdown Integration (Working)

The current implementation with R Markdown works well:
- Users add `knit: gluey::gluey_knit` to their YAML header
- The `gluey_knit` function processes gluey syntax before knitting
- The result is then passed to rmarkdown for full rendering

### Quarto Challenges

Quarto's architecture differs from R Markdown in important ways:
- Quarto has its own rendering pipeline separate from knitr
- Document-level knit functions like `gluey_knit` aren't supported
- Quarto supports pre-render scripts, but only at the project level
- Filters run after knitr processing, too late for our syntax transformation

## Approaches Explored

### 1. Project-Level Pre-render Script

**Implementation:**
- Create a pre-render script that processes gluey syntax
- Add to `_quarto.yml`:
  ```yaml
  project:
    pre-render: Rscript path/to/gluey-render.R
  ```

**Pros:**
- Leverages Quarto's supported extension mechanisms
- Can reliably process documents before rendering
- Access to environment variables containing file lists

**Cons:**
- Requires project-level configuration, not document-level
- Users need to set up a script in their project or reference one from the package

### 2. Custom Format Extension

**Implementation:**
- Create a Quarto format extension that incorporates gluey processing
- Add to document YAML:
  ```yaml
  format:
    gluey-html: default
  ```

**Pros:**
- Document-level configuration
- Integrates with Quarto's extension system

**Cons:**
- Requires creating and maintaining format extensions
- Limited to specific output formats (html, pdf, etc.)
- More complex implementation

### 3. In-Document Processing Hook

**Implementation:**
- Try to override knitr's input during the setup chunk:
  ```r
  knitr::opts_knit$set(input = gluey::process_document(knitr::current_input()))
  ```

**Pros:**
- Would allow document-level activation
- No additional files or configuration needed

**Cons:**
- May not be reliable across environments
- Depends on implementation details of knitr that could change
- Challenging to implement correctly

### 4. Helper Function for Project Setup

**Implementation:**
- Create a function that sets up the pre-render script:
  ```r
  gluey::setup_quarto_project()
  ```

**Pros:**
- Simplifies configuration for users
- Leverages the reliable pre-render approach
- One-time setup for multiple documents

**Cons:**
- Still requires project-level configuration
- Not as seamless as R Markdown integration

## Recommended Implementation

The most practical approach appears to be a combination of:

1. **Project pre-render script with setup helper**
   - Create a helper function that sets up the pre-render configuration
   - Include a pre-render script in the package that handles gluey syntax
   - Provide documentation for manual setup if preferred

2. **Future exploration of custom format extensions**
   - As a longer-term solution for document-level configuration

## Implementation Plan

### Phase 1: Project Pre-render Solution

1. Create `gluey-render.R` script in `inst/quarto/` directory
2. Implement `run_gluey_quarto()` function that sources this script
3. Implement `setup_quarto_project()` helper function that:
   - Creates/updates `_quarto.yml` with pre-render setting
   - Creates a wrapper script if needed
   - Explains what was set up to the user
4. Document the approach in vignettes/articles

### Phase 2: Investigate Format Extensions

1. Research Quarto format extension capabilities
2. Prototype a gluey-html format that processes syntax
3. Test across different environments and use cases

## Example API (Phase 1)

```r
# Setup a Quarto project to use gluey
gluey::setup_quarto_project(
  project_dir = ".",          # Project directory
  create_wrapper = TRUE,      # Create a local wrapper script
  update_quarto_yml = TRUE    # Update _quarto.yml automatically
)

# For users who prefer manual setup
gluey::gluey_quarto_command()  # Prints command to include in _quarto.yml
```

## Future Directions

- **Quarto Filter**: If future Quarto versions allow earlier filter execution, create a filter
- **RStudio Integration**: Develop an addin for one-click gluey-enabled rendering
- **Preprocessing API**: Create a more flexible API for document preprocessing
- **Lua Filter**: Explore if a lua filter could partially support some gluey features

## Contributing

If you have ideas or contributions for improving Quarto integration with gluey, please open an issue or pull request on the GitHub repository.
