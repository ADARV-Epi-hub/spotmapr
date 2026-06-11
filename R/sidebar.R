#' Build the sidebar HTML/CSS/JS for the SpotMap
#' @keywords internal
build_sidebar_html <- function(n_cases, n_controls, mode,
                                cluster_color, case_color, control_color) {
  css <- '
<style>
:root {
    --sm-primary: #0c4a6e;
    --sm-primary-2: #0e7490;
    --sm-accent: #0891b2;
    --sm-accent-hover: #0e7490;
    --sm-ink: #0f172a;
    --sm-muted: #64748b;
    --sm-border: #e2e8f0;
    --sm-surface: #ffffff;
    --sm-tint: #f8fafc;
}
#sidebar-toggle-btn {
    position: fixed; top: 16px; right: 16px; z-index: 10000;
    width: 44px; height: 44px; background: var(--sm-primary); border-radius: 10px;
    box-shadow: 0 4px 14px rgba(12,74,110,0.35); display: flex;
    align-items: center; justify-content: center; cursor: pointer;
    user-select: none; transition: all 0.18s ease;
}
#sidebar-toggle-btn:hover { background: var(--sm-primary-2); transform: translateY(-1px); box-shadow: 0 6px 18px rgba(12,74,110,0.45); }
#sidebar-toggle-btn span {
    display: block; width: 20px; height: 2.5px;
    background: #ffffff; margin: 2.5px 0; border-radius: 2px;
}
#map-sidebar {
    position: fixed; top: 16px; right: 16px; bottom: 16px; width: 318px;
    z-index: 9999; background: var(--sm-surface); border-radius: 16px;
    box-shadow: 0 10px 40px rgba(15,23,42,0.18);
    font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
    font-size: 13px; color: var(--sm-ink); overflow-y: auto;
    transform: translateX(118%);
    transition: transform 0.32s cubic-bezier(0.4, 0, 0.2, 1);
    border: 1px solid rgba(15,23,42,0.05);
}
#map-sidebar.open { transform: translateX(0); }
#map-sidebar::-webkit-scrollbar { width: 7px; }
#map-sidebar::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 4px; }
#map-sidebar::-webkit-scrollbar-thumb:hover { background: #94a3b8; }
.sidebar-header {
    padding: 20px 20px 18px; position: relative; overflow: hidden;
    background: linear-gradient(135deg, var(--sm-primary) 0%, var(--sm-primary-2) 100%);
    border-radius: 16px 16px 0 0; color: #fff;
}
.sidebar-header::after {
    content: ""; position: absolute; top: -40px; right: -40px;
    width: 130px; height: 130px; border-radius: 50%;
    background: rgba(255,255,255,0.08);
}
.sidebar-header .eyebrow {
    font-size: 10px; font-weight: 700; letter-spacing: 1.4px;
    text-transform: uppercase; opacity: 0.8; margin: 0 0 3px;
}
.sidebar-header h2 { margin: 0; font-size: 18px; font-weight: 700; letter-spacing: 0.2px; }
.stat-row {
    margin-top: 16px; display: flex; gap: 8px; position: relative; z-index: 1;
}
.stat-card {
    flex: 1; background: rgba(255,255,255,0.14); border-radius: 10px;
    padding: 9px 8px; text-align: center; backdrop-filter: blur(2px);
    border: 1px solid rgba(255,255,255,0.18);
}
.stat-card b { font-size: 18px; font-weight: 700; display: block; line-height: 1.1; }
.stat-card span { font-size: 9.5px; font-weight: 600; letter-spacing: 0.5px;
    text-transform: uppercase; opacity: 0.85; display: block; margin-top: 3px; }
.sidebar-section { padding: 16px 20px; border-bottom: 1px solid var(--sm-border); }
.sidebar-section:last-child { border-bottom: none; }
.sidebar-section h4 {
    margin: 0 0 11px 0; font-size: 10.5px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.9px; color: var(--sm-muted);
    display: flex; align-items: center; gap: 7px;
}
.sidebar-section h4::before {
    content: ""; width: 3px; height: 12px; border-radius: 2px;
    background: var(--sm-accent); display: inline-block;
}
.seg-control {
    display: flex; background: var(--sm-tint); border-radius: 10px; padding: 4px; gap: 4px;
    border: 1px solid var(--sm-border);
}
.seg-control label {
    flex: 1; text-align: center; border-radius: 7px;
    cursor: pointer; font-size: 12px; font-weight: 500; transition: all 0.15s ease;
}
.seg-control input { display: none; }
.seg-control input:checked + span {
    background: var(--sm-primary); box-shadow: 0 2px 6px rgba(12,74,110,0.28);
    color: #fff; font-weight: 600;
}
.seg-control span {
    display: block; padding: 8px 8px; border-radius: 7px;
    color: var(--sm-muted); transition: all 0.15s ease;
}
.seg-control label:hover span { color: var(--sm-ink); }
.swatch-row {
    display: flex; gap: 10px; align-items: center; margin: 8px 0;
    padding: 8px 10px; background: var(--sm-tint); border-radius: 9px;
    border: 1px solid var(--sm-border);
}
.swatch-row label { flex: 1; font-size: 12.5px; font-weight: 500; color: #334155; }
.swatch-row input[type="color"] {
    width: 42px; height: 30px; border: 1px solid var(--sm-border); border-radius: 7px;
    cursor: pointer; padding: 2px; background: #fff;
}
input[type="range"] {
    width: 100%; accent-color: var(--sm-accent); height: 5px;
}
.slider-row {
    display: flex; align-items: center; gap: 12px;
    padding: 6px 10px; background: var(--sm-tint); border-radius: 9px;
    border: 1px solid var(--sm-border);
}
.slider-value {
    min-width: 38px; font-size: 12.5px; font-weight: 700; color: var(--sm-accent);
    text-align: right;
}
.btn {
    display: block; width: 100%; padding: 11px 12px; margin: 8px 0 0;
    background: var(--sm-primary); color: #fff; border: none; border-radius: 9px;
    cursor: pointer; font-size: 12.5px; font-weight: 600; text-align: center;
    text-decoration: none; transition: all 0.16s ease; letter-spacing: 0.2px;
}
.btn:hover { background: var(--sm-primary-2); transform: translateY(-1px); box-shadow: 0 4px 12px rgba(12,74,110,0.25); }
.btn.secondary { background: #fff; color: var(--sm-primary); border: 1.5px solid var(--sm-border); }
.btn.secondary:hover { background: var(--sm-tint); border-color: var(--sm-accent); color: var(--sm-accent); }
.sidebar-foot {
    padding: 13px 20px; text-align: center; font-size: 10px; color: #94a3b8;
    letter-spacing: 0.3px; background: var(--sm-tint); border-radius: 0 0 16px 16px;
}
.spot-filter { display: none; margin-top: 12px; }
.spot-filter.show { display: block; }
#map-legend {
    position: fixed; top: 16px; left: 60px; z-index: 1000; background: #fff;
    padding: 12px 16px; border-radius: 12px; box-shadow: 0 6px 24px rgba(15,23,42,0.15);
    font-size: 12px; font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, sans-serif;
    min-width: 120px; display: none; border: 1px solid var(--sm-border);
}
#map-legend h4 {
    margin: 0 0 9px 0; font-size: 10px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.7px; color: var(--sm-muted); text-align: center;
}
.legend-item { display: flex; align-items: center; margin-bottom: 6px; font-size: 12.5px; font-weight: 500; color: var(--sm-ink); }
.legend-icon {
    width: 15px; height: 15px; border-radius: 50%;
    margin-right: 10px; border: 1px solid rgba(0,0,0,0.12);
}
@media print {
    /* Force browser to actually print background colours so cluster
       icons, legend swatches, and pins keep their colour in the PDF. */
    * {
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
        color-adjust: exact !important;
    }
    html, body { background: #fff !important; }
    #map-sidebar, #sidebar-toggle-btn { display: none !important; }
    #map-legend {
        display: block !important;
        position: absolute; top: 10px; left: 10px;
        background: #fff !important;
        border: 1px solid #ccc;
    }
    .legend-icon { border: 1px solid #333 !important; }
    .leaflet-control-zoom, .leaflet-control-attribution { display: none !important; }
    /* Cluster icon divs */
    .cluster-icon > div, .marker-cluster div {
        box-shadow: none !important;
    }
}
</style>'

  html <- paste0('
<div id="sidebar-toggle-btn" title="Map options">
  <div><span></span><span></span><span></span></div>
</div>

<div id="map-legend">
    <h4>Legend</h4>
    <div class="legend-item" id="legend-case-item">
        <span class="legend-icon" id="legend-icon-case" style="background-color:', case_color, ';"></span>
        <span>Case</span>
    </div>
    <div class="legend-item" id="legend-control-item" style="display:none;">
        <span class="legend-icon" id="legend-icon-control" style="background-color:', control_color, ';"></span>
        <span>Control</span>
    </div>
</div>

<div id="map-sidebar">
  <div class="sidebar-header">
    <h2>SpotMap</h2>
    <div class="stat-row">
      <div class="stat-card"><b>', n_cases, '</b><span>Cases</span></div>
      <div class="stat-card"><b>', n_controls, '</b><span>Controls</span></div>
      <div class="stat-card"><b>', mode, '</b><span>Zoom</span></div>
    </div>
  </div>

  <div class="sidebar-section">
    <h4>Display Mode</h4>
    <div class="seg-control">
      <label>
        <input type="radio" name="markerMode" value="dots" checked>
        <span>Dot Density</span>
      </label>
      <label>
        <input type="radio" name="markerMode" value="pins">
        <span>Spot Pins</span>
      </label>
    </div>
    <div class="spot-filter" id="spotFilterBox">
      <h4 style="margin-top:12px;">Show</h4>
      <div class="seg-control">
        <label>
          <input type="radio" name="spotFilterMode" value="cases" checked>
          <span>Cases Only</span>
        </label>
        <label>
          <input type="radio" name="spotFilterMode" value="both">
          <span>Cases &amp; Controls</span>
        </label>
      </div>
    </div>
  </div>

  <div class="sidebar-section" id="clusterColorSection">
    <h4>Cluster Colour</h4>
    <div class="swatch-row">
      <label>Pick a colour</label>
      <input type="color" id="clusterCustomColor" value="', cluster_color, '">
    </div>
  </div>

  <div class="sidebar-section" id="pinColorSection" style="display:none;">
    <h4>Pin Colours</h4>
    <div class="swatch-row">
      <label>Cases</label>
      <input type="color" id="caseColorPicker" value="', case_color, '">
    </div>
    <div class="swatch-row">
      <label>Controls</label>
      <input type="color" id="controlColorPicker" value="', control_color, '">
    </div>
    <h4 style="margin-top:14px;">Pin Size</h4>
    <div class="slider-row">
      <input type="range" id="pinSizeSlider" min="0.5" max="2.0" step="0.1" value="1.0">
      <span class="slider-value" id="pinSizeValue">1.0x</span>
    </div>
  </div>

  <div class="sidebar-section">
    <h4>Export Map</h4>
    <a class="btn" id="downloadPngLink">Download PNG</a>
    <a class="btn secondary" id="downloadPrintLink">Print / Save PDF</a>
  </div>

  <div class="sidebar-foot">Generated with SpotMap &middot; spotmapr</div>
</div>

<link rel="stylesheet" href="https://unpkg.com/leaflet-simple-map-screenshoter/dist/leaflet-simple-map-screenshoter.css" />
<script src="https://unpkg.com/leaflet-simple-map-screenshoter"></script>
')

  htmltools::HTML(paste0(css, html))
}


#' Build the sidebar JavaScript for layer toggling
#' @keywords internal
build_sidebar_js <- function(cluster_color, case_color, control_color) {
  paste0('
function(el, x) {
  try {
    var mapObj = this;

    var sidebar = document.getElementById("map-sidebar");
    var toggleBtn = document.getElementById("sidebar-toggle-btn");
    if (!sidebar || !toggleBtn) return;

    var sidebarOpen = true;
    sidebar.classList.add("open");

    toggleBtn.addEventListener("click", function() {
      sidebarOpen = !sidebarOpen;
      sidebar.classList.toggle("open", sidebarOpen);
    });

    window.caseColor = "', case_color, '";
    window.controlColor = "', control_color, '";
    window.clusterBaseColor = "', cluster_color, '";
    window.pinScale = 1.0;

    function updateLegend() {
      var legendBox = document.getElementById("map-legend");
      if (!legendBox) return;
      var modeEl = document.querySelector("input[name=\\"markerMode\\"]:checked");
      if (!modeEl) return;
      var isPins = modeEl.value === "pins";
      legendBox.style.display = isPins ? "block" : "none";
      if (!isPins) return;
      document.getElementById("legend-icon-case").style.backgroundColor = window.caseColor;
      document.getElementById("legend-icon-control").style.backgroundColor = window.controlColor;
      var filterEl = document.querySelector("input[name=\\"spotFilterMode\\"]:checked");
      var isBoth = filterEl && filterEl.value === "both";
      document.getElementById("legend-control-item").style.display = isBoth ? "flex" : "none";
    }

    function makePinIcon(colorHex) {
      var scale = window.pinScale || 1.0;
      var baseW = 18, baseH = 24;
      var html =
        \'<div style="position:relative;width:\'+baseW+\'px;height:\'+baseH+\'px;transform:scale(\'+scale+\');transform-origin:50% 100%;">\' +
          \'<div style="position:absolute;left:3px;top:6px;width:12px;height:12px;border-radius:50% 50% 50% 0;background:\'+colorHex+\';transform:rotate(-45deg);box-shadow:0 0 2px rgba(0,0,0,0.5);"></div>\' +
          \'<div style="position:absolute;left:6.5px;top:9.5px;width:5px;height:5px;border-radius:50%;background:white;opacity:0.9;"></div>\' +
        \'</div>\';
      return new L.DivIcon({ html: html, className: "", iconSize: [baseW, baseH], iconAnchor: [baseW/2, baseH] });
    }

    // Use R leaflet layerManager for reliable group show/hide
    // This is the same mechanism hideGroup/showGroup use internally
    function showGroup(name) {
      if (!mapObj.layerManager) return;
      try {
        var g = mapObj.layerManager.getLayerGroup(name, false);
        if (g && !mapObj.hasLayer(g)) mapObj.addLayer(g);
      } catch(e) { console.log("showGroup error:", name, e); }
    }
    function hideGroup(name) {
      if (!mapObj.layerManager) return;
      try {
        var g = mapObj.layerManager.getLayerGroup(name, false);
        if (g && mapObj.hasLayer(g)) mapObj.removeLayer(g);
      } catch(e) { console.log("hideGroup error:", name, e); }
    }

    function redrawPins() {
      if (!mapObj.layerManager) return;
      try {
        var caseGroup = mapObj.layerManager.getLayerGroup("Spot Map - Cases", false);
        if (caseGroup) {
          caseGroup.eachLayer(function(marker) {
            if (marker.setIcon) marker.setIcon(makePinIcon(window.caseColor));
          });
        }
        var ctrlGroup = mapObj.layerManager.getLayerGroup("Spot Map - Controls", false);
        if (ctrlGroup) {
          ctrlGroup.eachLayer(function(marker) {
            if (marker.setIcon) marker.setIcon(makePinIcon(window.controlColor));
          });
        }
      } catch(e) { console.log("redrawPins error:", e); }
      updateLegend();
    }

    function applyLayerLogic() {
      var modeEl = document.querySelector("input[name=\\"markerMode\\"]:checked");
      var filterEl = document.querySelector("input[name=\\"spotFilterMode\\"]:checked");
      if (!modeEl || !filterEl) return;
      var mode = modeEl.value;
      var filter = filterEl.value;

      document.getElementById("spotFilterBox").classList.toggle("show", mode === "pins");
      document.getElementById("clusterColorSection").style.display = mode === "dots" ? "block" : "none";
      document.getElementById("pinColorSection").style.display = mode === "pins" ? "block" : "none";

      if (mode === "dots") {
        showGroup("Dot Density Layer");
        hideGroup("Spot Map - Cases");
        hideGroup("Spot Map - Controls");
      } else {
        hideGroup("Dot Density Layer");
        showGroup("Spot Map - Cases");
        if (filter === "both") {
          showGroup("Spot Map - Controls");
        } else {
          hideGroup("Spot Map - Controls");
        }
      }
      updateLegend();
    }

    // Refresh cluster icons (re-runs iconCreateFunction with new clusterBaseColor)
    function refreshClusters() {
      if (!mapObj.layerManager) return;
      try {
        var g = mapObj.layerManager.getLayerGroup("Dot Density Layer", false);
        if (!g) return;
        // MarkerClusterGroup wraps markers; find it inside the LayerGroup
        g.eachLayer(function(layer) {
          if (typeof layer.refreshClusters === "function") {
            layer.refreshClusters();
          }
        });
        // Also handle case where g itself is a MarkerClusterGroup
        if (typeof g.refreshClusters === "function") g.refreshClusters();
      } catch(e) { console.log("refreshClusters error:", e); }
    }

    document.querySelectorAll("input[type=radio]").forEach(function(r) {
      r.addEventListener("change", applyLayerLogic);
    });

    // Cluster colour picker -- live update
    var clusterP = document.getElementById("clusterCustomColor");
    if (clusterP) {
      clusterP.addEventListener("input", function(e) {
        window.clusterBaseColor = e.target.value;
        refreshClusters();
      });
    }

    // Colour pickers
    var caseP = document.getElementById("caseColorPicker");
    var ctrlP = document.getElementById("controlColorPicker");
    if (caseP) caseP.addEventListener("input", function(e) { window.caseColor = e.target.value; redrawPins(); });
    if (ctrlP) ctrlP.addEventListener("input", function(e) { window.controlColor = e.target.value; redrawPins(); });

    // Pin size slider
    var sizeSlider = document.getElementById("pinSizeSlider");
    var sizeValue = document.getElementById("pinSizeValue");
    if (sizeSlider) {
      sizeSlider.addEventListener("input", function(e) {
        window.pinScale = parseFloat(e.target.value);
        sizeValue.textContent = window.pinScale.toFixed(1) + "x";
        redrawPins();
      });
    }

    // Move legend inside map container so screenshoter captures it
    try {
      var legendDiv = document.getElementById("map-legend");
      if (legendDiv && mapObj.getContainer) {
        mapObj.getContainer().appendChild(legendDiv);
      }
    } catch(e) {}

    // Screenshoter for PNG export
    var simpleMapScreenshoter = null;
    try {
      if (typeof L.simpleMapScreenshoter === "function") {
        simpleMapScreenshoter = L.simpleMapScreenshoter({
          hidden: true,
          mimeType: "image/png"
        }).addTo(mapObj);
      }
    } catch(e) { console.log("screenshoter init error:", e); }

    // Print
    var printBtn = document.getElementById("downloadPrintLink");
    if (printBtn) printBtn.addEventListener("click", function() { window.print(); });

    // PNG download
    var pngBtn = document.getElementById("downloadPngLink");
    if (pngBtn) {
      pngBtn.addEventListener("click", function() {
        if (!simpleMapScreenshoter) {
          alert("Screenshot library failed to load. Try Print / Save PDF instead.");
          return;
        }
        sidebar.classList.remove("open");
        sidebarOpen = false;
        setTimeout(function() {
          simpleMapScreenshoter.takeScreen("blob", { caption: function() { return ""; } })
            .then(function(blob) {
              var link = document.createElement("a");
              link.download = "spotmap.png";
              link.href = URL.createObjectURL(blob);
              link.click();
            })
            .catch(function(e) { alert(e); })
            .finally(function() {
              sidebar.classList.add("open");
              sidebarOpen = true;
            });
        }, 500);
      });
    }

    // Apply initial pin icons + layer logic. The map starts in dots mode,
    // so pins are hidden, but we still need to assign DivIcons so that
    // when the user switches to pins mode they show up correctly.
    redrawPins();
    applyLayerLogic();
    updateLegend();
  } catch(err) {
    console.error("SpotMap sidebar error:", err);
    try {
      var banner = document.createElement("div");
      banner.style.cssText = "position:fixed;top:10px;left:50%;transform:translateX(-50%);background:#fee;color:#900;padding:8px 14px;border:1px solid #c66;border-radius:6px;z-index:99999;font-family:sans-serif;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,0.15);";
      banner.textContent = "SpotMap UI error: " + (err && err.message ? err.message : err) + " (see browser console for details)";
      document.body.appendChild(banner);
      setTimeout(function() { banner.style.display = "none"; }, 8000);
    } catch(_) {}
  }
}
')
}
