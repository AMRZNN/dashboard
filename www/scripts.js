/* =========================================================
   AMR Dashboard — scripts.js (clean, mockup-aligned)
   - Auto-scale to viewport (width + height)
   ========================================================= */

/* Scaling baseline (matches your layout composition) */
const BASE_W = 1400;
const BASE_H = 1080;

(function () {
  function scaleDashboard(){
    const root = document.getElementById("amr-scale-root");
    if(!root) return;

    const header = document.querySelector(".main-header");
    const headerH = header ? header.offsetHeight : 0;

    const availW = window.innerWidth;
    const SAFE_BOTTOM = 60; // taakbalk/browsers UI buffer (pas aan indien nodig)
    const availH = window.innerHeight - headerH - SAFE_BOTTOM;


    const sW = availW / BASE_W;
    const sH = availH / BASE_H;
    const scale = Math.min(sW, sH, 1);

    root.style.transformOrigin = "top left";
    root.style.transform = "scale(" + scale.toFixed(4) + ")";
    root.style.width = (100 / scale) + "%";
    root.style.height = (100 / scale) + "%";
  }

  function boot(){
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