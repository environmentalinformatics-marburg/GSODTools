# GSODTools 1.0.3.9005 (2022-05-14)

#### ✨ features and improvements

  * Initializes {tinytest} unit testing

#### 🐛 bug fixes

#### 💬 documentation etc

#### 🍬 miscellaneous

  * Resolves `if() conditions comparing class() to string`
  * Removes './data/moshi/' subfolder and code references
  * Fixes broken examples
  * Resolves hitherto undocumented arguments in documentation
  * Exports `as.ki.data()`, thus resolving missing links in documentation
  * Resolves non-declared `library()` or `require()` calls in package code
  * Eliminates numerous dependencies
  * Captures global variables with no visible binding


# GSODTools 1.0.3 (2022-05-09)

#### 🍬 miscellaneous

  * `gsodstations` built-in data:
    - update
    - resolves non-ASCII characters (#2)
    - adds proper documentation
  * Renames built-in data `data_nairobi_kilimanjaro` to `eastafrica`


# GSODTools 1.0.2 (2022-05-02)

  * Implements `wkt` to bypass `sp::CRS()` related issues (#5)
