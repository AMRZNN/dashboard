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
//  Alle/Geen selecteren per groep
// ------------------------------------------------------------
function setSelectAll(group, checked) {
  document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']")
    .forEach(function(cb) { cb.checked = checked; });
  onPathogenChange();
}

// ------------------------------------------------------------
//  Checkboxes → Shiny inputs
// ------------------------------------------------------------
function onPathogenChange() {
  var dataset = (document.getElementById("dataset") || {}).value || "brmo";
  var group   = dataset === "respiratoir" ? "resp" : "brmo";
  var inputId = dataset === "respiratoir" ? "pathogenen_resp" : "pathogenen_brmo";

  var selected = [];
  document.querySelectorAll(".amr-pathogeen-check[data-group='" + group + "']:checked")
    .forEach(function(cb) { selected.push(cb.dataset.value); });

  var val = selected.join(",");
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