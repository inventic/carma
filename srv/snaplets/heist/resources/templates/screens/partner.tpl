<!-- Partner screen template -->
<script type="text/template"
        class="screen-template"
        id="partner-screen-template">

  <div id="partner-left" class="nice-scrollbar pane">
    <form class="form-vertical">
      <button class="btn btn-action" type="button"
        onclick="location.hash='partner';location.reload(true);">
        <i class="icon icon-plus"></i>Добавить партнёра
      </button>
      <br/><br/>
      <table id="partner-table" class="table table-striped table-bordered">
        <thead>
          <tr>
            <th>#</th>
            <th>Название</th>
            <th>Город</th>
            <th>Комментарии</th>
          </tr>
        </thead>
        <tbody/>
      </table>
    </form>
  </div>

  <div id="partner-center" class="nice-scrollbar pane">
    <form class="form-vertical">
      <div id="partner-form" />
      <div class="control-group">
        <div class="control-label">
          <label>Услуги</label>
        </div>
        <div class="controls">
          <span class="accordion" id="partner-service-references" />
          <span id="partner-service-picker-container" />
          <button class="dropdown-toggle btn btn-action"
                  onclick="addNewServiceToPartner();"
                  type="button"
                  data-toggle="dropdown">
            <i class="icon icon-plus"></i>Добавить услугу
          </button>
        </div>
      </div>
      <div id="partner-permissions" />
    </form>
  </div>
</script>

