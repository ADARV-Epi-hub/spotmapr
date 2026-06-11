# spotmapr 0.1.11

## New features
- **Cases-only mode** for datasets without a case/control variable
  (e.g. outbreak surveillance, where every row is a case). Pass
  `all_cases = TRUE` to `spot_map()` to skip outcome detection and
  treat every row as a case. The interactive wizard now offers
  "(No outcome column - treat all rows as cases)" as the first
  option in Step 4.

## Robustness fixes (pre-CRAN edge-case audit)
- Rows with missing or out-of-range latitude / longitude no longer
  crash the build; they are dropped with a one-line warning telling the
  user how many rows were skipped.
- Empty input data frames now produce a clear "Input data is empty"
  error instead of a confusing "could not auto-detect lat/lon" message.
- A warning is emitted when most points fall outside India's bounding
  box (lat 6-38, lon 67-98), which usually means the lat / lon columns
  were swapped.
- Scientific-notation coordinates (e.g. `1.1e1`) are now recognised by
  the column-type detector.
- Internal helper columns renamed from leading-underscore names
  (`_outcome_norm`, `_auto_lat`, `_auto_lon`) to namespaced
  `.spotmapr_*` names, so users who happen to have a column literally
  called `_outcome_norm` no longer have their data silently overwritten.

## New features
- Interactive wizard (`run_interactive()` and `spot_map()` with no args)
  now prompts for the output HTML path (Step 6), matching the Python
  version's behaviour.
- Live cluster colour picker actually recolors clusters via
  `refreshClusters()` — the icon function now reads
  `window.clusterBaseColor` dynamically.
- Pin colour & pin size sliders now work because pins are rendered as
  `L.Marker` with DivIcons that JS can mutate via `setIcon()`.
- "Download PNG" button is wired up via
  `leaflet-simple-map-screenshoter` (CDN).
- A red error banner now surfaces sidebar JS failures instead of
  swallowing them in the browser console.

## Bug fixes
- `detect_lat_lon()` no longer secretly mutates the caller's data frame
  via `parent.frame()`; it returns the (possibly augmented) data frame
  in the result list under `$df`.
- `detect_outcome()` now errors loudly with the list of available values
  when a user-supplied `case_value` isn't present in the column.
- Dropped a redundant `setView()` call that was immediately overridden
  by `fitBounds()`.
- Removed unused internal `sf_to_geojson()`.

## Internal
- Replaced brittle `eachLayer` + `instanceof` JS layer lookup with R
  leaflet's own `layerManager.getLayerGroup()` — the same mechanism
  `hideGroup`/`showGroup` use.
- Added README.md and NEWS.md.
- Added regression tests for `detect_outcome` validation and
  `detect_lat_lon`'s new return shape.

# spotmapr 0.1.10

- Initial release.
