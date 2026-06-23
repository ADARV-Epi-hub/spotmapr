#' Build an interactive epidemiological spot map for India
#'
#' @param data Path to a CSV/Excel/TSV file, or a data.frame.
#' @param state_shp Optional custom state boundary file path.
#' @param district_shp Optional custom district boundary file path.
#' @param lat_col Latitude column name. Auto-detected when NULL.
#' @param lon_col Longitude column name. Auto-detected when NULL.
#' @param outcome_col Outcome column name. Auto-detected when NULL.
#' @param case_value Value representing a case. Auto-detected when NULL.
#' @param all_cases If `TRUE`, skip outcome detection and treat every row
#'   as a case (no controls on the map). Useful for outbreak surveillance
#'   or case-only datasets that don't have a control group. Defaults to `FALSE`.
#' @param count_cutoff District count threshold for zoom level (default 2).
#' @param margin_deg Padding in degrees around bounding box (default 1.0).
#' @param cluster_color Hex colour for dot-density clusters.
#' @param case_color Hex colour for case pins.
#' @param control_color Hex colour for control pins.
#' @param output Path to save the HTML file. If NULL, returns the leaflet widget.
#' @return A leaflet widget (invisibly if saved to file).
#' @export
#' @examples
#' \dontrun{
#' spot_map("cases.csv", output = "map.html")
#'
#' # Or with a data.frame
#' spot_map(my_df, lat_col = "lat", lon_col = "lon",
#'          outcome_col = "status", case_value = "case")
#' }
spot_map <- function(data,
                     state_shp = NULL,
                     district_shp = NULL,
                     lat_col = NULL,
                     lon_col = NULL,
                     outcome_col = NULL,
                     case_value = NULL,
                     all_cases = FALSE,
                     count_cutoff = 2L,
                     margin_deg = 1.0,
                     cluster_color = "#E85252",
                     case_color = "#D55757",
                     control_color = "#7676E7",
                     output = NULL) {

  # If called with no arguments at all, drop into the interactive wizard
  # (Python equivalent: `python -m spotmap` runs the prompt-based flow.)
  if (missing(data)) {
    out <- if (is.null(output)) "spotmap.html" else output
    return(invisible(run_interactive(output_path = out)))
  }

  # 1. Load data
  if (is.character(data)) {
    df <- load_data(data)
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    stop("data must be a file path (character) or a data.frame.", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("Input data is empty (0 rows). Cannot build a map.", call. = FALSE)
  }

  # 2. Detect columns
  ll <- detect_lat_lon(df, lat_col, lon_col)
  lat_col <- ll$lat
  lon_col <- ll$lon
  df <- ll$df  # may have new _auto_lat / _auto_lon columns

  # 2a. Coerce coordinates to numeric and drop rows with missing / invalid coords
  lat_num <- suppressWarnings(as.numeric(as.character(df[[lat_col]])))
  lon_num <- suppressWarnings(as.numeric(as.character(df[[lon_col]])))
  bad_coord <- is.na(lat_num) | is.na(lon_num) |
    lat_num < -90 | lat_num > 90 |
    lon_num < -180 | lon_num > 180
  n_bad <- sum(bad_coord)
  if (n_bad > 0) {
    warning(n_bad, " row(s) with missing or out-of-range coordinates were ",
            "dropped before mapping.", call. = FALSE)
    df <- df[!bad_coord, , drop = FALSE]
    if (nrow(df) == 0) {
      stop("All rows had missing or invalid coordinates. Nothing to map.",
           call. = FALSE)
    }
  }
  df[[lat_col]] <- lat_num[!bad_coord]
  df[[lon_col]] <- lon_num[!bad_coord]

  # 2b. India-bounds sanity check (warn if most points are clearly elsewhere)
  in_india <- lat_num[!bad_coord] >= 6  & lat_num[!bad_coord] <= 38 &
              lon_num[!bad_coord] >= 67 & lon_num[!bad_coord] <= 98
  if (sum(in_india) / nrow(df) < 0.5) {
    warning("Most points (",
            round(100 * (1 - sum(in_india) / nrow(df))),
            "%) fall outside India's bounding box (lat 6-38, lon 67-98). ",
            "Are your lat/lon columns swapped?",
            call. = FALSE)
  }

  # Cases-only mode: skip outcome detection, treat every row as a case.
  # Triggered when the user passes all_cases = TRUE, OR when the user
  # didn't pass an outcome_col and no plausible one can be auto-detected.
  if (isTRUE(all_cases)) {
    outcome_col <- ".spotmapr_outcome_norm"
    case_value <- "case"
    df[[".spotmapr_outcome_norm"]] <- "case"
    message("Cases-only mode: treating all ", nrow(df), " rows as cases.")
  } else {
    oc <- detect_outcome(df, outcome_col, case_value)
    outcome_col <- oc$outcome_col
    case_value <- oc$case_value

    df[[".spotmapr_outcome_norm"]] <- tolower(trimws(as.character(df[[outcome_col]])))
    df[[".spotmapr_outcome_norm"]][df[[".spotmapr_outcome_norm"]] %in% c("na", "nan")] <- NA
  }

  # 3. Load boundaries
  bnd <- load_boundaries(state_shp, district_shp)
  states <- bnd$states
  districts <- bnd$districts
  state_name_col <- bnd$state_name_col
  district_name_col <- bnd$district_name_col

  # 4. Spatial join
  points_joined <- spatial_join(df, lat_col, lon_col, states, districts,
                                 state_name_col, district_name_col)

  # 5. Split cases / controls
  is_case <- points_joined[[".spotmapr_outcome_norm"]] == case_value
  is_case[is.na(is_case)] <- FALSE
  points_cases <- points_joined[is_case, ]
  points_controls <- points_joined[!is_case, ]

  if (nrow(points_cases) == 0)
    stop("No case points found with outcome value '", case_value, "'.",
         call. = FALSE)

  # 6. Determine mode + crop
  mode_info <- determine_mode(points_cases, district_name_col, state_name_col,
                               count_cutoff)
  mode <- mode_info$mode
  bounds <- mode_info$bounds

  india_outline <- build_india_outline(states)
  affected_states <- states[states[[state_name_col]] %in% mode_info$unique_states, ]
  affected_districts <- districts[districts[[district_name_col]] %in%
                                    mode_info$affected_districts, ]

  india_sub <- crop_geodataframe(india_outline, bounds, margin_deg)
  states_sub <- crop_geodataframe(affected_states, bounds, margin_deg)
  districts_sub <- crop_geodataframe(affected_districts, bounds, margin_deg)

  # 7. Build leaflet map
  # (setView is omitted because fitBounds below overrides it anyway)
  m <- leaflet::leaflet(
    width = "100%", height = "100%",
    options = leaflet::leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1)
  ) |>
    # No-labels base so OpenStreetMap place labels can be toggled on/off.
    leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    # Toggleable place-name labels (rendered by CARTO from OpenStreetMap).
    leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Place Labels")

  # 8. Boundary layers (using addPolygons with sf objects directly)
  if (!is.null(india_sub) && nrow(india_sub) > 0) {
    m <- m |> leaflet::addPolygons(
      data = india_sub,
      weight = 1, color = "#000000", fillOpacity = 0.0, opacity = 0.5,
      group = "India Border"
    )
  }
  if (!is.null(states_sub) && nrow(states_sub) > 0) {
    m <- m |> leaflet::addPolygons(
      data = states_sub,
      weight = 1.5, color = "#4B0082", fillOpacity = 0.05, opacity = 0.7,
      group = "Affected States"
    )
  }
  if (!is.null(districts_sub) && nrow(districts_sub) > 0) {
    m <- m |> leaflet::addPolygons(
      data = districts_sub,
      weight = 1, color = "#000000", fillOpacity = 0.01, opacity = 1.0,
      group = "Affected Districts"
    )
  }

  # 9. Auto-zoom using data bounds
  tb <- as.numeric(bounds)  # xmin, ymin, xmax, ymax -- strip names
  if (length(tb) == 4 && all(is.finite(tb)) && tb[3] > tb[1] && tb[4] > tb[2]) {
    buf_x <- (tb[3] - tb[1]) * 0.1
    buf_y <- (tb[4] - tb[2]) * 0.1
    # Ensure minimum buffer
    if (buf_x < 0.01) buf_x <- 0.5
    if (buf_y < 0.01) buf_y <- 0.5
    m <- m |> leaflet::fitBounds(
      lng1 = tb[1] - buf_x,
      lat1 = tb[2] - buf_y,
      lng2 = tb[3] + buf_x,
      lat2 = tb[4] + buf_y
    )
  }

  # 10. Marker layers -- dot density (clustered)
  case_coords <- sf::st_coordinates(points_cases)
  case_popups <- paste0(
    "<b>Type:</b> Case<br>",
    "<b>State:</b> ", points_cases[[state_name_col]], "<br>",
    "<b>District:</b> ", points_cases[[district_name_col]]
  )

  # Custom cluster icon function matching Python version
  # Reads from window.clusterBaseColor so live color changes work via refreshClusters
  total_cases <- nrow(points_cases)
  cluster_icon_js <- sprintf("
    function(cluster) {
      var count = cluster.getChildCount();
      var total = %d;
      var frac = count / total;
      if (!isFinite(frac) || frac < 0) frac = 0;
      if (frac > 1) frac = 1;
      var baseHex = (typeof window !== 'undefined' && window.clusterBaseColor) ? window.clusterBaseColor : '%s';
      function hexToRgb(hex) {
        if (!hex) return {r:255,g:0,b:0};
        var c = hex.replace('#','');
        if (c.length===3) c=c[0]+c[0]+c[1]+c[1]+c[2]+c[2];
        var num = parseInt(c,16);
        return {r:(num>>16)&255, g:(num>>8)&255, b:num&255};
      }
      function mixWithWhite(rgb,t) {
        t = Math.max(0,Math.min(1,t));
        return {r:Math.round(255*(1-t)+rgb.r*t), g:Math.round(255*(1-t)+rgb.g*t), b:Math.round(255*(1-t)+rgb.b*t)};
      }
      var baseRgb = hexToRgb(baseHex);
      var t = 0.4 + 0.6*Math.sqrt(frac);
      if (!isFinite(t)||t<0.4) t=0.4; if (t>1) t=1;
      var mixed = mixWithWhite(baseRgb, t);
      var color = 'rgba('+mixed.r+','+mixed.g+','+mixed.b+',0.95)';
      var size = 30 + Math.min(25, Math.sqrt(count)*3);
      return new L.DivIcon({
        html: '<div style=\"background:'+color+';width:'+size+'px;height:'+size+'px;border-radius:50%%;display:flex;align-items:center;justify-content:center;box-shadow:0 0 6px rgba(0,0,0,0.4);font-weight:bold;color:#333;\">'+count+'</div>',
        className: 'cluster-icon',
        iconSize: new L.Point(size, size)
      });
    }
  ", total_cases, cluster_color)

  m <- m |>
    leaflet::addMarkers(
      lng = case_coords[, 1], lat = case_coords[, 2],
      popup = case_popups,
      group = "Dot Density Layer",
      clusterOptions = leaflet::markerClusterOptions(
        disableClusteringAtZoom = 15,
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE,
        maxClusterRadius = 60,
        singleMarkerMode = TRUE,
        iconCreateFunction = htmlwidgets::JS(cluster_icon_js)
      )
    )

  # Use addMarkers (L.Marker) for pins so JS can call setIcon() to update
  # color + size live, exactly like the Python version. We pass a 1px
  # transparent placeholder icon; the real pin-shaped DivIcon is applied
  # in JS at init via redrawPins().
  blank_icon <- leaflet::makeIcon(
    iconUrl = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==",
    iconWidth = 1, iconHeight = 1,
    iconAnchorX = 0, iconAnchorY = 0
  )

  # Case pins
  m <- m |>
    leaflet::addMarkers(
      lng = case_coords[, 1], lat = case_coords[, 2],
      popup = case_popups,
      icon = blank_icon,
      group = "Spot Map - Cases"
    )

  # Control pins
  if (nrow(points_controls) > 0) {
    ctrl_coords <- sf::st_coordinates(points_controls)
    ctrl_popups <- paste0(
      "<b>Type:</b> Control<br>",
      "<b>State:</b> ", points_controls[[state_name_col]], "<br>",
      "<b>District:</b> ", points_controls[[district_name_col]]
    )
    m <- m |>
      leaflet::addMarkers(
        lng = ctrl_coords[, 1], lat = ctrl_coords[, 2],
        popup = ctrl_popups,
        icon = blank_icon,
        group = "Spot Map - Controls"
      )
  } else {
    # Add empty group so JS doesn't break
    m <- m |> leaflet::addMarkers(
      lng = numeric(0), lat = numeric(0),
      icon = blank_icon,
      group = "Spot Map - Controls"
    )
  }

  # 11. Hide pin layers by default (dots mode is default).
  #     Place labels also start hidden (toggled from the sidebar).
  m <- m |>
    leaflet::hideGroup("Spot Map - Cases") |>
    leaflet::hideGroup("Spot Map - Controls") |>
    leaflet::hideGroup("Place Labels")

  # 12. Sidebar HTML + JS
  sidebar <- build_sidebar_html(
    n_cases = nrow(points_cases),
    n_controls = nrow(points_controls),
    mode = mode,
    cluster_color = cluster_color,
    case_color = case_color,
    control_color = control_color
  )

  sidebar_js <- build_sidebar_js(cluster_color, case_color, control_color)

  # Full-page styling so the map fills the browser window
  fullpage_css <- htmltools::tags$style(htmltools::HTML("
    html, body { margin: 0; padding: 0; width: 100vw; height: 100vh; overflow: hidden; }
    #htmlwidget_container { width: 100vw; height: 100vh; }
    .leaflet.html-widget { width: 100vw !important; height: 100vh !important; }
  "))

  m <- m |>
    htmlwidgets::prependContent(fullpage_css) |>
    htmlwidgets::prependContent(sidebar) |>
    htmlwidgets::onRender(sidebar_js)

  # Override sizing policy to fill browser
  m$sizingPolicy <- htmlwidgets::sizingPolicy(
    defaultWidth = "100%",
    defaultHeight = "100%",
    browser.fill = TRUE,
    viewer.fill = TRUE,
    padding = 0
  )

  return(.spot_map_finish(m, output))
}


#' @rdname spot_map
#' @export
spotmap <- function(...) spot_map(...)


#' Internal: save the widget or return it
#' @keywords internal
.spot_map_finish <- function(m, output) {
  # 13. Save or return
  if (!is.null(output)) {
    out_path <- normalizePath(output, mustWork = FALSE)
    # Prefer a single self-contained HTML file -- much friendlier for
    # non-coders who just want to email or share one file. This needs
    # pandoc; if it isn't available, fall back to the multi-file form
    # (an HTML file plus a "<name>_files" folder) and say so.
    saved_selfcontained <- tryCatch({
      htmlwidgets::saveWidget(m, file = out_path, selfcontained = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (!saved_selfcontained) {
      htmlwidgets::saveWidget(m, file = out_path, selfcontained = FALSE)
      message("Note: saved as '", basename(output), "' plus a companion ",
              "'_files' folder. Install pandoc for a single self-contained file.")
    }
    message("Map saved to: ", output)
    invisible(m)
  } else {
    m
  }
}
