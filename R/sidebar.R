#' Build the top toolbar HTML/CSS for the SpotMap (ADARV attached-bar layout)
#' @keywords internal
build_sidebar_html <- function(n_cases, n_controls, mode,
                                cluster_color, case_color, control_color) {
  css <- '
<style>
:root {
    --sm-primary: #1b2a5e;
    --sm-primary-2: #2a3d7a;
    --sm-accent: #2563eb;
    --sm-ink: #0f172a;
    --sm-muted: #64748b;
    --sm-border: #e2e8f0;
    --sm-surface: #ffffff;
    --sm-tint: #f8fafc;
}
#sm-toolbar {
    position: fixed; top: 0; left: 0; right: 0; z-index: 9999;
    display: flex; align-items: center; justify-content: flex-end; flex-wrap: wrap;
    gap: 10px; padding: 10px 16px;
    background: var(--sm-surface); border-bottom: 1px solid var(--sm-border);
    box-shadow: 0 2px 10px rgba(15,23,42,0.06);
    font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
}
.sm-group { display: flex; align-items: center; gap: 8px; }
.sm-chip-label {
    display: inline-flex; align-items: center; gap: 6px; font-size: 12px; font-weight: 600;
    color: var(--sm-primary); background: #eef2ff; padding: 6px 11px; border-radius: 999px; white-space: nowrap;
}
.sm-chip-label .dot { width: 7px; height: 7px; border-radius: 50%; background: var(--sm-accent); }
.sm-dropdown { position: relative; }
.sm-pill {
    display: inline-flex; align-items: center; gap: 7px; font-size: 12.5px; font-weight: 600;
    color: var(--sm-ink); background: #fff; border: 1px solid var(--sm-border);
    padding: 7px 13px; border-radius: 999px; cursor: pointer; white-space: nowrap; transition: all .15s ease;
}
.sm-pill:hover { border-color: var(--sm-accent); color: var(--sm-accent); }
.sm-pill .caret { font-size: 9px; opacity: .55; }
.sm-pill.primary { background: var(--sm-primary); color: #fff; border-color: var(--sm-primary); }
.sm-pill.primary:hover { background: var(--sm-primary-2); color: #fff; }
.sm-pill.disabled { opacity: .45; pointer-events: none; }
.sm-menu {
    position: absolute; top: calc(100% + 8px); left: 0; min-width: 215px; background: #fff;
    border: 1px solid var(--sm-border); border-radius: 12px; box-shadow: 0 10px 30px rgba(15,23,42,0.18);
    padding: 8px; display: none; z-index: 10001;
}
.sm-menu.open { display: block; }
.sm-dropdown.right .sm-menu { left: auto; right: 0; }
.sm-menu-title {
    font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .7px;
    color: var(--sm-muted); padding: 8px 10px 5px;
}
.sm-menu-item {
    display: flex; align-items: center; justify-content: space-between; gap: 12px;
    padding: 9px 10px; border-radius: 8px; font-size: 12.5px; font-weight: 500;
    color: var(--sm-ink); cursor: pointer;
}
.sm-menu-item:hover { background: var(--sm-tint); }
.sm-menu-item.active { background: #eef2ff; color: var(--sm-primary); font-weight: 600; }
.sm-menu-item.disabled { opacity: .4; pointer-events: none; cursor: not-allowed; }
.sm-menu-item .check { color: var(--sm-accent); font-weight: 700; opacity: 0; }
.sm-menu-item.active .check { opacity: 1; }
.sm-color-row {
    display: flex; align-items: center; justify-content: space-between;
    padding: 7px 10px; font-size: 12.5px; font-weight: 500; color: #334155;
}
.sm-color-row input[type="color"] {
    width: 34px; height: 24px; border: 1px solid var(--sm-border); border-radius: 6px;
    padding: 1px; cursor: pointer; background: #fff;
}
.switch { position: relative; display: inline-block; width: 42px; height: 22px; flex: 0 0 auto; }
.switch input { opacity: 0; width: 0; height: 0; margin: 0; }
.slider-toggle {
    position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0;
    background: #cbd5e1; border-radius: 22px; transition: 0.2s;
}
.slider-toggle::before {
    content: ""; position: absolute; height: 16px; width: 16px; left: 3px; bottom: 3px;
    background: #fff; border-radius: 50%; transition: 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.3);
}
.switch input:checked + .slider-toggle { background: var(--sm-primary); }
.switch input:checked + .slider-toggle::before { transform: translateX(20px); }
.toggle-state {
    font-size: 11px; font-weight: 700; color: var(--sm-muted);
    text-transform: uppercase; letter-spacing: 0.5px; min-width: 22px;
}
.leaflet-top { top: 58px; }
#map-legend {
    position: absolute; top: 72px; left: 12px; z-index: 1000; background: #fff;
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
#map-credit {
    position: absolute; bottom: 12px; left: 50%; transform: translateX(-50%);
    z-index: 1000; background: rgba(255,255,255,0.92); color: var(--sm-primary);
    font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
    font-size: 11px; font-weight: 600; letter-spacing: 0.2px;
    padding: 5px 13px; border-radius: 8px; border: 1px solid var(--sm-border);
    box-shadow: 0 2px 8px rgba(15,23,42,0.12); pointer-events: none; white-space: nowrap;
}
@media print {
    * {
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
        color-adjust: exact !important;
    }
    html, body { background: #fff !important; }
    #sm-toolbar { display: none !important; }
    .leaflet-top { top: 0 !important; }
    #map-legend {
        display: block !important; position: absolute; top: 10px; left: 10px;
        background: #fff !important; border: 1px solid #ccc;
    }
    .legend-icon { border: 1px solid #333 !important; }
    .leaflet-control-zoom, .leaflet-control-attribution { display: none !important; }
    .cluster-icon > div, .marker-cluster div { box-shadow: none !important; }
}
</style>'

  html <- paste0('
<div id="map-credit">Interactive spot maps for India &middot; created by ADARV</div>

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

<div id="sm-toolbar">
  <div class="sm-group">
    <span class="sm-chip-label"><span class="dot"></span>Pin Size</span>
    <div class="sm-dropdown" id="dd-pinsize">
      <button class="sm-pill" id="pinsizeBtn"><span id="pinsizeLabel">75% (Recommended)</span> <span class="caret">&#9660;</span></button>
      <div class="sm-menu" id="pinsizeMenu">
        <div class="sm-menu-title">Pin Size</div>
        <div class="sm-menu-item" data-pct="50"><span>50%</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-pct="75"><span>75% (Recommended)</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-pct="100"><span>100%</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-pct="150"><span>150%</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-pct="200"><span>200%</span><span class="check">&#10003;</span></div>
      </div>
    </div>
  </div>

  <div class="sm-group">
    <span class="sm-chip-label"><span class="dot"></span>Category</span>
    <div class="sm-dropdown" id="dd-category">
      <button class="sm-pill" id="categoryBtn"><span id="categoryLabel">Only Cases</span> <span class="caret">&#9660;</span></button>
      <div class="sm-menu" id="categoryMenu">
        <div class="sm-menu-title">Spot Map Filter</div>
        <div class="sm-menu-item" data-filter="cases"><span>Only Cases</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-filter="both"><span>Case &amp; Control</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-title">Colours</div>
        <div class="sm-color-row" id="row-case"><span>Case</span><input type="color" id="caseColorPicker" value="', case_color, '"></div>
        <div class="sm-color-row" id="row-control"><span>Control</span><input type="color" id="controlColorPicker" value="', control_color, '"></div>
        <div class="sm-color-row" id="row-cluster"><span>Cluster</span><input type="color" id="clusterCustomColor" value="', cluster_color, '"></div>
      </div>
    </div>
  </div>

  <div class="sm-group">
    <span class="sm-chip-label"><span class="dot"></span>Map Type</span>
    <div class="sm-dropdown" id="dd-maptype">
      <button class="sm-pill" id="maptypeBtn"><span id="maptypeLabel">Dot Density</span> <span class="caret">&#9660;</span></button>
      <div class="sm-menu" id="maptypeMenu">
        <div class="sm-menu-title">Map Type</div>
        <div class="sm-menu-item" data-mode="pins"><span>Spot Map (Pins)</span><span class="check">&#10003;</span></div>
        <div class="sm-menu-item" data-mode="dots"><span>Dot Density (Cluster)</span><span class="check">&#10003;</span></div>
      </div>
    </div>
  </div>

  <div class="sm-group">
    <span class="sm-chip-label"><span class="dot"></span>Labels</span>
    <label class="switch"><input type="checkbox" id="toggleLabels"><span class="slider-toggle"></span></label>
    <span class="toggle-state" id="labelsState">Off</span>
  </div>

  <button class="sm-pill" id="recenterBtn">&#8635; Recenter</button>

  <div class="sm-dropdown right" id="dd-download">
    <button class="sm-pill primary" id="downloadBtn">Download <span class="caret">&#9660;</span></button>
    <div class="sm-menu" id="downloadMenu">
      <div class="sm-menu-item" id="downloadPngLink"><span>PNG image</span></div>
      <div class="sm-menu-item" id="downloadPrintLink"><span>Print / Save PDF</span></div>
    </div>
  </div>
</div>

<link rel="stylesheet" href="https://unpkg.com/leaflet-simple-map-screenshoter/dist/leaflet-simple-map-screenshoter.css" />
<script src="https://unpkg.com/leaflet-simple-map-screenshoter"></script>
')

  htmltools::HTML(paste0(css, html))
}


#' Build the toolbar JavaScript for layer toggling
#' @keywords internal
build_sidebar_js <- function(cluster_color, case_color, control_color) {
  paste0('
function(el, x) {
  try {
    var mapObj = this;
    function byId(id) { return document.getElementById(id); }

    window.caseColor = "', case_color, '";
    window.controlColor = "', control_color, '";
    window.clusterBaseColor = "', cluster_color, '";
    window.pinScale = 0.75;

    var state = { mode: "dots", filter: "cases", pinPct: 75 };
    function pinLabel(pct) { return pct === 75 ? "75% (Recommended)" : (pct + "%"); }

    var initCenter = mapObj.getCenter();
    var initZoom = mapObj.getZoom();

    function updateLegend() {
      var box = byId("map-legend");
      if (!box) return;
      var isPins = state.mode === "pins";
      box.style.display = isPins ? "block" : "none";
      if (!isPins) return;
      byId("legend-icon-case").style.backgroundColor = window.caseColor;
      byId("legend-icon-control").style.backgroundColor = window.controlColor;
      byId("legend-control-item").style.display = state.filter === "both" ? "flex" : "none";
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

    function showGroup(name) {
      if (!mapObj.layerManager) return;
      try { var g = mapObj.layerManager.getLayerGroup(name, false); if (g && !mapObj.hasLayer(g)) mapObj.addLayer(g); } catch(e) {}
    }
    function hideGroup(name) {
      if (!mapObj.layerManager) return;
      try { var g = mapObj.layerManager.getLayerGroup(name, false); if (g && mapObj.hasLayer(g)) mapObj.removeLayer(g); } catch(e) {}
    }

    function redrawPins() {
      if (!mapObj.layerManager) return;
      try {
        var caseGroup = mapObj.layerManager.getLayerGroup("Spot Map - Cases", false);
        if (caseGroup) caseGroup.eachLayer(function(m) { if (m.setIcon) m.setIcon(makePinIcon(window.caseColor)); });
        var ctrlGroup = mapObj.layerManager.getLayerGroup("Spot Map - Controls", false);
        if (ctrlGroup) ctrlGroup.eachLayer(function(m) { if (m.setIcon) m.setIcon(makePinIcon(window.controlColor)); });
      } catch(e) {}
      updateLegend();
    }

    function refreshClusters() {
      if (!mapObj.layerManager) return;
      try {
        var g = mapObj.layerManager.getLayerGroup("Dot Density Layer", false);
        if (!g) return;
        g.eachLayer(function(layer) { if (typeof layer.refreshClusters === "function") layer.refreshClusters(); });
        if (typeof g.refreshClusters === "function") g.refreshClusters();
      } catch(e) {}
    }

    function syncMenus() {
      document.querySelectorAll("#maptypeMenu .sm-menu-item").forEach(function(it) {
        it.classList.toggle("active", it.getAttribute("data-mode") === state.mode);
      });
      byId("maptypeLabel").textContent = state.mode === "pins" ? "Spot Map (Pins)" : "Dot Density";

      document.querySelectorAll("#categoryMenu .sm-menu-item").forEach(function(it) {
        it.classList.toggle("active", it.getAttribute("data-filter") === state.filter);
      });
      byId("categoryLabel").textContent = state.filter === "both" ? "Case & Control" : "Only Cases";

      var dotsItem = document.querySelector("#maptypeMenu .sm-menu-item[data-mode=dots]");
      if (dotsItem) dotsItem.classList.toggle("disabled", state.filter === "both");

      document.querySelectorAll("#pinsizeMenu .sm-menu-item").forEach(function(it) {
        it.classList.toggle("active", parseInt(it.getAttribute("data-pct"), 10) === state.pinPct);
      });
      byId("pinsizeLabel").textContent = pinLabel(state.pinPct);
      byId("pinsizeBtn").classList.toggle("disabled", state.mode !== "pins");

      byId("row-case").style.display = state.mode === "pins" ? "flex" : "none";
      byId("row-control").style.display = (state.mode === "pins" && state.filter === "both") ? "flex" : "none";
      byId("row-cluster").style.display = state.mode === "dots" ? "flex" : "none";
    }

    function applyLayerLogic() {
      if (state.mode === "dots") {
        showGroup("Dot Density Layer");
        hideGroup("Spot Map - Cases");
        hideGroup("Spot Map - Controls");
      } else {
        hideGroup("Dot Density Layer");
        showGroup("Spot Map - Cases");
        if (state.filter === "both") { showGroup("Spot Map - Controls"); } else { hideGroup("Spot Map - Controls"); }
        redrawPins();
      }
      syncMenus();
      updateLegend();
    }

    function closeAllMenus(except) {
      document.querySelectorAll(".sm-menu").forEach(function(m) { if (m !== except) m.classList.remove("open"); });
    }
    function wireDropdown(btnId, menuId) {
      var b = byId(btnId), m = byId(menuId);
      if (!b || !m) return;
      b.addEventListener("click", function(e) {
        e.stopPropagation();
        if (b.classList.contains("disabled")) return;
        var isOpen = m.classList.contains("open");
        closeAllMenus();
        if (!isOpen) m.classList.add("open");
      });
      m.addEventListener("click", function(e) { e.stopPropagation(); });
    }
    document.addEventListener("click", function() { closeAllMenus(); });
    wireDropdown("pinsizeBtn", "pinsizeMenu");
    wireDropdown("categoryBtn", "categoryMenu");
    wireDropdown("maptypeBtn", "maptypeMenu");
    wireDropdown("downloadBtn", "downloadMenu");

    document.querySelectorAll("#maptypeMenu .sm-menu-item").forEach(function(it) {
      it.addEventListener("click", function() {
        if (it.classList.contains("disabled")) return;
        state.mode = it.getAttribute("data-mode");
        closeAllMenus();
        applyLayerLogic();
      });
    });
    document.querySelectorAll("#categoryMenu .sm-menu-item").forEach(function(it) {
      it.addEventListener("click", function() {
        state.filter = it.getAttribute("data-filter");
        if (state.filter === "both" && state.mode === "dots") state.mode = "pins";
        applyLayerLogic();
      });
    });
    document.querySelectorAll("#pinsizeMenu .sm-menu-item").forEach(function(it) {
      it.addEventListener("click", function() {
        state.pinPct = parseInt(it.getAttribute("data-pct"), 10);
        window.pinScale = state.pinPct / 100;
        closeAllMenus();
        redrawPins();
        syncMenus();
      });
    });

    var clusterP = byId("clusterCustomColor");
    if (clusterP) clusterP.addEventListener("input", function(e) { window.clusterBaseColor = e.target.value; refreshClusters(); });
    var caseP = byId("caseColorPicker");
    if (caseP) caseP.addEventListener("input", function(e) { window.caseColor = e.target.value; redrawPins(); });
    var ctrlP = byId("controlColorPicker");
    if (ctrlP) ctrlP.addEventListener("input", function(e) { window.controlColor = e.target.value; redrawPins(); });

    function applyLabelLogic() {
      var cb = byId("toggleLabels");
      if (!cb) return;
      if (cb.checked) { showGroup("Place Labels"); } else { hideGroup("Place Labels"); }
      var st = byId("labelsState");
      if (st) st.textContent = cb.checked ? "On" : "Off";
    }
    var labelsCb = byId("toggleLabels");
    if (labelsCb) labelsCb.addEventListener("change", applyLabelLogic);

    var recenterBtn = byId("recenterBtn");
    if (recenterBtn) recenterBtn.addEventListener("click", function() { mapObj.setView(initCenter, initZoom); });

    try {
      var legendDiv = byId("map-legend");
      if (legendDiv && mapObj.getContainer) mapObj.getContainer().appendChild(legendDiv);
      var creditDiv = byId("map-credit");
      if (creditDiv && mapObj.getContainer) mapObj.getContainer().appendChild(creditDiv);
    } catch(e) {}

    var simpleMapScreenshoter = null;
    try {
      if (typeof L.simpleMapScreenshoter === "function") {
        simpleMapScreenshoter = L.simpleMapScreenshoter({ hidden: true, mimeType: "image/png" }).addTo(mapObj);
      }
    } catch(e) {}

    var printBtn = byId("downloadPrintLink");
    if (printBtn) printBtn.addEventListener("click", function() { closeAllMenus(); window.print(); });

    var pngBtn = byId("downloadPngLink");
    if (pngBtn) pngBtn.addEventListener("click", function() {
      closeAllMenus();
      if (!simpleMapScreenshoter) { alert("Screenshot library failed to load. Try Print / Save PDF instead."); return; }
      var tb = byId("sm-toolbar");
      if (tb) tb.style.display = "none";
      setTimeout(function() {
        simpleMapScreenshoter.takeScreen("blob", { caption: function() { return ""; } })
          .then(function(blob) {
            var link = document.createElement("a");
            link.download = "spotmap.png";
            link.href = URL.createObjectURL(blob);
            link.click();
          })
          .catch(function(e) { alert(e); })
          .finally(function() { if (tb) tb.style.display = "flex"; });
      }, 400);
    });

    applyLabelLogic();
    applyLayerLogic();
    redrawPins();
  } catch(err) {
    console.error("SpotMap toolbar error:", err);
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
