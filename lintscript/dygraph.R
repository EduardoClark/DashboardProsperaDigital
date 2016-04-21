<style>.dygraph-legend {background-color:#E7E3E3 !important;}</style>
    <script>
    $(document).ready(function(){
      $("#categorias > li:nth-child(2) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(1) > a:nth-child(1)").click(function(){
        $("#grafico").before($("#mapa"));
      });
      $("#categorias > li:nth-child(3) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(4) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(5) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(6) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(7) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(8) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(9) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(10) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      $("#categorias > li:nth-child(11) > a:nth-child(1)").click(function(){
        $("#mapa").before($("#grafico"));
      });
      
    });
  </script>
    
    
    
    series <- xts(TimeData, order.by = datetimes, tz="CST")
    TimeSeries <- dygraph(series) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", drawGrid = FALSE,label = "Porcentaje de clínicas ya incorporado", 
             axisLabelFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}',
             valueFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}') %>%  dyRangeSelector()
    
    
    <div id="grafico" class="jumbotron" style="background-color:#E7E3E3;margin-top:30px;margin-bottom:-20px">
      <div class="container" >
      {{ timeseries }}
    </div>
      </div>