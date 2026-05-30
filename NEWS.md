# spotmapr 0.1.11

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
