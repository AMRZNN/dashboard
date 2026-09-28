// ============================================================
//  AMR Dashboard — scripts.js
// ============================================================

// Sidebar hoogte: van sidebar.top tot row2.bottom - 30px
function alignSidebar() {
  var row2    = document.querySelector('.amr-row2');
  var sidebar = document.querySelector('.amr-sidebar');
  if (!row2 || !sidebar) return;
  var h = row2.getBoundingClientRect().bottom - sidebar.getBoundingClientRect().top - 20;
  if (h > 100) sidebar.style.height = h + 'px';
}

// Observeer DOM-wijzigingen zodat we weten wanneer Shiny klaar is
var _observer = new MutationObserver(function() { alignSidebar(); });
document.addEventListener('DOMContentLoaded', function() {
  alignSidebar();
  _observer.observe(document.body, { childList: true, subtree: true });
  // Stop observeren na 10 seconden (grafieken zijn dan zeker geladen)
  setTimeout(function() { _observer.disconnect(); alignSidebar(); }, 10000);
});
window.addEventListener('resize', alignSidebar);

// ------------------------------------------------------------
//  Dataset toggle (brmo / respiratoir)
// ------------------------------------------------------------
function setDataset(val) {
  var input = document.getElementById("dataset");
  if (input) {
    input.value = val;
    Shiny.setInputValue("dataset", val, { priority: "event" });
  }
  document.querySelectorAll(".amr-dataset-btn").forEach(function(btn) {
    btn.classList.remove("active");
  });
  var activeBtn = document.getElementById("btn-dataset-" + val);
  if (activeBtn) activeBtn.classList.add("active");

  // Wissel filterblok in sidebar
  var brmoFilter = document.getElementById("sidebar-brmo-filter");
  var respFilter = document.getElementById("sidebar-resp-filter");
  if (brmoFilter) brmoFilter.style.display = (val === "brmo")        ? "" : "none";
  if (respFilter) respFilter.style.display = (val === "respiratoir") ? "" : "none";

  onPathogenChange();
}

// ------------------------------------------------------------
//  Weergave toggle (absoluut / per100k)
// ------------------------------------------------------------
function setWeergave(val) {
  var input = document.getElementById("weergave");
  if (input) {
    input.value = val;
    Shiny.setInputValue("weergave", val, { priority: "event" });
  }
  document.querySelectorAll(".amr-weergave-btn").forEach(function(btn) {
    btn.classList.remove("active");
  });
  var activeBtn = document.getElementById(val === "absoluut" ? "btn-absoluut" : "btn-per100k");
  if (activeBtn) activeBtn.classList.add("active");
}

// ------------------------------------------------------------
//  Alle selecteren per groep
// ------------------------------------------------------------
function setSelectAll(group, checked) {
  document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']")
    .forEach(function(cb) { cb.checked = checked; });
  onPathogenChange();
}

// ------------------------------------------------------------
//  Top 4 selecteren — op basis van R-output top4_brmo / top4_resp
// ------------------------------------------------------------

// Cache voor top-4 waarden die vanuit R binnenkomen via sendCustomMessage
var _top4Cache = { brmo: null, resp: null };

// R stuurt top-4 actief zodra data geladen is
Shiny.addCustomMessageHandler("top4_brmo", function(top4) {
  _top4Cache.brmo = top4;
});
Shiny.addCustomMessageHandler("top4_resp", function(top4) {
  _top4Cache.resp = top4;
});

function applyTop4(group, top4) {
  // Deselecteer alles in de groep eerst
  document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']")
    .forEach(function(cb) { cb.checked = false; });

  top4.forEach(function(val) {
    var cb = document.querySelector(
      ".amr-pathogeen-check[data-group='" + group + "'][data-value='" + val + "']"
    );
    if (cb) cb.checked = true;
  });

  onPathogenChange();
}

function setTop4(group) {
  var cached = group === "resp" ? _top4Cache.resp : _top4Cache.brmo;

  if (cached && cached.length > 0) {
    // R-waarden beschikbaar: gebruik ze
    applyTop4(group, cached);
  } else {
    // R nog niet klaar: vraag R om top-4 en wacht op antwoord
    Shiny.setInputValue("top4_request", group, { priority: "event" });

    // Wacht max 2 seconden op de cache, daarna fallback op tekstcontent
    var attempts = 0;
    var poll = setInterval(function() {
      var c = group === "resp" ? _top4Cache.resp : _top4Cache.brmo;
      if (c && c.length > 0) {
        clearInterval(poll);
        applyTop4(group, c);
      } else if (++attempts >= 20) {
        clearInterval(poll);
        // Laatste poging: lees tekstcontent van hidden element
        var outputId = group === "resp" ? "top4_resp" : "top4_brmo";
        var el = document.getElementById(outputId);
        if (el && el.textContent.trim()) {
          try {
            applyTop4(group, JSON.parse(el.textContent.trim()));
            return;
          } catch(err) {}
        }
        // Absolute fallback: eerste 4 checkboxes
        var all = document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']");
        applyTop4(group, Array.from(all).slice(0, 4).map(function(cb) { return cb.dataset.value; }));
      }
    }, 100);
  }
}

// ------------------------------------------------------------
//  Checkboxes → Shiny inputs
//  Stuurt ook alle_geselecteerd mee als boolean
// ------------------------------------------------------------
function onPathogenChange() {
  var dataset = (document.getElementById("dataset") || {}).value || "brmo";
  var group   = dataset === "respiratoir" ? "resp" : "brmo";
  var inputId = dataset === "respiratoir" ? "pathogenen_resp" : "pathogenen_brmo";

  var alle  = document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']");
  var aangevinkt = [];
  alle.forEach(function(cb) { if (cb.checked) aangevinkt.push(cb.dataset.value); });

  var val = aangevinkt.join(",");
  var input = document.getElementById(inputId);
  if (input) input.value = val;
  Shiny.setInputValue(inputId, val, { priority: "event" });
}

// ------------------------------------------------------------
//  Init na pagina-load
// ------------------------------------------------------------
document.addEventListener("DOMContentLoaded", function() {
  setTimeout(onPathogenChange, 300);
});