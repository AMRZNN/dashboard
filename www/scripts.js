(function() {

  function fixHeights() {

    // --- KPI grid ---
    var kpiBox    = document.querySelector('.amr-kpi-box');
    var kpiOutput = document.getElementById('ggd-kpi-kpi_grid');
    var kpiGrid   = kpiOutput ? kpiOutput.querySelector('.amr-kpi-grid') : null;

    if (kpiBox && kpiOutput) {
      var kpiAvail = kpiBox.offsetHeight - 20;
      kpiOutput.style.height = kpiAvail + 'px';
      kpiOutput.style.display = 'block';
      if (kpiGrid) kpiGrid.style.height = kpiAvail + 'px';
    }

    // --- Micro staafgrafiek ---
    var microBox     = document.querySelector('.amr-micro-box');
    var microWrapper = microBox ? microBox.querySelector('.amr-micro-plot-wrapper') : null;
    var microOutput  = microWrapper ? microWrapper.querySelector('.shiny-html-output') : null;

    if (microBox && microWrapper) {
      var boxH      = microBox.offsetHeight;
      var headerH   = microBox.querySelector('.box-header') ? microBox.querySelector('.box-header').offsetHeight : 0;
      var subtitleH = microBox.querySelector('.amr-subtitle') ? microBox.querySelector('.amr-subtitle').offsetHeight : 0;
      var padding   = 36;
      var microAvail = boxH - headerH - subtitleH - padding;

      microWrapper.style.height = microAvail + 'px';
      if (microOutput) microOutput.style.height = microAvail + 'px';

      // Shiny girafe output heeft ook een inner div
      var girafe = microWrapper.querySelector('.girafe');
      if (girafe) girafe.style.height = microAvail + 'px';
    }
  }

  document.addEventListener('shiny:connected', fixHeights);
  document.addEventListener('shiny:idle',      fixHeights);
  document.addEventListener('shiny:value',     function() { setTimeout(fixHeights, 50); });
  window.addEventListener('resize',            fixHeights);
  setTimeout(fixHeights, 300);
  setTimeout(fixHeights, 1000);

})();
