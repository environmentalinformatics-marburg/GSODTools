# GSODTools 1.0.4.9002 (2022-06-03)

#### ✨ features and improvements

#### 🐛 bug fixes

#### 💬 documentation etc

#### 🍬 miscellaneous

  * Eliminates {sp} and {raster} dependencies --> uses {sf} instead (#10)
  * Disables interactive selection of area of interest in `stationFromExtent()`
  * Removes obsolete arguments in `gsodReformat()` and deprecates function


# GSODTools 1.0.4 (2022-05-18)

#### ✨ features and improvements

  * Initializes {tinytest} unit testing

#### 🍬 miscellaneous

  * Resolves `if() conditions comparing class() to string`
  * Removes './data/moshi/' subfolder and code references
  * Fixes broken examples
  * Resolves hitherto undocumented arguments in documentation
  * Exports `as.ki.data()`, thus resolving missing links in documentation
  * Resolves non-declared `library()` or `require()` calls in package code
  * Eliminates numerous dependencies
  * Captures global variables with no visible binding
  * Documents hitherto undocumented object or prevents generation of help pages


# GSODTools 1.0.3 (2022-05-09)

#### 🍬 miscellaneous

  * `gsodstations` built-in data:
    - update
    - resolves non-ASCII characters (#2)
    - adds proper documentation
  * Renames built-in data `data_nairobi_kilimanjaro` to `eastafrica`


# GSODTools 1.0.2 (2022-05-02)

  * Implements `wkt` to bypass `sp::CRS()` related issues (#5)
