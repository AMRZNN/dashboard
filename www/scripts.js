/* =========================================================
   AMR Dashboard — scripts.js (clean, mockup-aligned)
   - Header branding card injection (logo clickable + right link)
   - Auto-scale to viewport (width + height)
   ========================================================= */

/* ---------- Edit these if needed ---------- */
const AMR_WEBSITE = "https://www.amrznn.nl/";
const AMR_TITLE = "AMR Surveillance Noord-Nederland";
const AMR_SUBTITLE = "Geaggregeerde trends, geen patiëntniveau-data";

/* Optional right-side link in header (mockup shows a chevron) */
const RIGHT_LINK_TEXT = "Geaggregeerde trends, geen patiëntniveau-data";
const RIGHT_LINK_URL  = "https://www.amrznn.nl/";

/* Scaling baseline (matches your layout composition) */
const BASE_W = 1400;
const BASE_H = 980;

(function () {
  function scaleDashboard(){
    const root = document.getElementById("amr-scale-root");
    if(!root) return;

    const header = document.querySelector(".main-header");
    const headerH = header ? header.offsetHeight : 0;

    const availW = window.innerWidth;
    const SAFE_BOTTOM = 28; // taakbalk/browsers UI buffer (pas aan indien nodig)
    const availH = window.innerHeight - headerH - SAFE_BOTTOM;


    const sW = availW / BASE_W;
    const sH = availH / BASE_H;
    const scale = Math.min(sW, sH, 1);

    root.style.transformOrigin = "top left";
    root.style.transform = "scale(" + scale.toFixed(4) + ")";
    root.style.width = (100 / scale) + "%";
    root.style.height = (100 / scale) + "%";
  }

  function injectHeader(){
    const nav = document.querySelector(".main-header .navbar");
    if(!nav) return;
    if(nav.querySelector(".amr-header-brand")) return;

    const brand = document.createElement("div");
    brand.className = "amr-header-brand";

    brand.innerHTML =
      '<a class="amr-logo-link" href="' + AMR_WEBSITE + '" target="_blank" rel="noopener">' +
        '<img src="logo_amr.png" class="amr-logo" alt="AMR Zorgnetwerk" />' +
      "</a>" +
      '<div class="amr-header-text">' +
        '<div class="amr-header-title">' + AMR_TITLE + "</div>" +
        '<a class="amr-header-link" href="' + RIGHT_LINK_URL + '" target="_blank" rel="noopener">' +
          RIGHT_LINK_TEXT +
        "</a>" /* +
        
      "</div>" +
      '<div class="amr-header-right">' +
        '<a class="amr-header-link" href="' + RIGHT_LINK_URL + '" target="_blank" rel="noopener">' +
          RIGHT_LINK_TEXT +
        "</a>" +
        '<span class="amr-chevron">›</span>' +
      "</div>";
      */

    nav.prepend(brand);
  }

  function boot(){
    injectHeader();
    // scale after injection to prevent clipping
    setTimeout(scaleDashboard, 20);
    setTimeout(scaleDashboard, 120);
  }

  window.addEventListener("resize", scaleDashboard);
  window.addEventListener("load", boot);

  // Shiny reconnect hook
  document.addEventListener("shiny:connected", boot);

  // fallback
  setTimeout(boot, 60);
})();
