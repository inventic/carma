<!-- Import VINs screen -->
<script type="text/template"
        id="vin-screen-template"
        class="screen-template">
  <div id="vin-form" />
</script>

<script type="text/template"
        id="vin-form-template"
        class="view-template">
  <div style="text-align:center;">
  <fieldset>
    <legend>Импорт VIN</legend>
    <form id="vin-import-form" onsubmit="doVin(); return false;">
      <p>
        <select name="program">
          <option value="ford">Ford</option>
          <option value="fordPlus">Ford+</option>
          <option value="vwMotor">VW Легковые</option>
          <option value="vwCommercial">VW Коммерческие</option>
          <option value="opel">Opel</option>
          <option value="hummer">Hummer</option>
          <option value="chevroletNAO">Chevrolet NAO</option>
          <option value="chevroletKorea">Chevrolet Korea</option>
          <option value="cadillac">Cadillac</option>
          <option value="vwRuslan">VW Рус-Лан</option>
          <option value="chartis">Chartis</option>
          <option value="vwAvilon">VW Avilon</option>
          <option value="atlantM">Атлант-М</option>
          <option value="autocraft">AutoCraft</option>
          <option value="b2c">B2C</option>
        </select>
        <input type="file"
               name="file"
               accept="text/csv|application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" />
      </p>
      <button class="btn btn-success" type="submit">
        Отправить
      </button>
    </form>
  </fieldset>
  </div>
  <div id="vin-alert-container" />
</script>

<script type="text/template"
        id="vin-alert-template">
  <!-- TODO Should be row-fluid when fluid containers are
            fixed in Bootstrap upstream. -->
  <div class="container">
    <div class="row">
      <div class="span6 offset3">
        {{# alerts}}
          <div class="alert alert-{{alertType}}" style="margin-bottom: 2px;">
            <button class="close"
                        data-dismiss="alert"
                        onclick="removeVinAlert('{{alertId}}'); return false;">×</button>
            {{alertVinFile}}: {{ alertMessage }}
            {{# alertErrorFile }}
              <a href="{{alertErrorFile}}">Файл</a> с необработанными записями.
            {{/ alertErrorFile }}
            {{# alertErrorLogFile }}
              <a href="{{alertErrorLogFile}}">Файл</a> с описанием ошибок.
            {{/ alertErrorLogFile }}
          </div>
        {{/ alerts}}
      </div>
    </div>
  </div>
</script>
