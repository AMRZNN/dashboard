// Weergave toggle
function setWeergave(val) {
  // Update hidden input
  var input = document.getElementById("weergave");
  if (input) {
    input.value = val;
    // Trigger Shiny input event
    Shiny.setInputValue("weergave", val, {priority: "event"});
  }
  // Update knop stijlen
  document.querySelectorAll(".amr-weergave-btn").forEach(function(btn) {
    btn.classList.remove("active");
  });
  var activeBtn = document.getElementById("btn-" + val);
  if (activeBtn) activeBtn.classList.add("active");
}

(function() {

  function fixKpiHeight() {
    var kpiBox    = document.querySelector('.amr-kpi-box');
    var kpiOutput = document.getElementById('ggd-kpi-kpi_grid');
    var kpiGrid   = kpiOutput ? kpiOutput.querySelector('.amr-kpi-grid') : null;

    if (kpiBox && kpiOutput) {
      var available = kpiBox.offsetHeight - 20;
      kpiOutput.style.height = available + 'px';
      kpiOutput.style.display = 'block';
      if (kpiGrid) kpiGrid.style.height = available + 'px';
    }
  }

  document.addEventListener('shiny:connected', fixKpiHeight);
  document.addEventListener('shiny:idle',      fixKpiHeight);
  document.addEventListener('shiny:value',     function() { setTimeout(fixKpiHeight, 50); });
  window.addEventListener('resize',            fixKpiHeight);
  setTimeout(fixKpiHeight, 300);
  setTimeout(fixKpiHeight, 1000);

})();