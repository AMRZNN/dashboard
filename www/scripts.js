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