<!-- Search screen -->
<script type="text/template"
        id="search-screen-template"
        class="screen-template">
  <!-- Can't use offsetN class here due to fluid layout. -->
  <div id="tableView" />
</script>

<script type="text/template"
        id="search-table-template"
        class="view-template">
  <div style="text-align:center;">
  <fieldset style="width:50%; margin-left:25%;">
    <legend>Поиск</legend>
    <form onsubmit="doSearch(); return false;">
      <div data-date-format="dd.mm.yyyy" 
           id="search-datepicker"
           data-provide="datepicker"
           data-date-weekstart="1"
           class="input-append date">
	    <input type="text"
               style="width: 90%;"
               id="table-query"
               size="16" class="span2" id="acpro_inp2">
	    <span class="add-on"><i class="icon-calendar"></i></span>
      </div>
      <button class="btn btn-success" type="submit">
        Поиск
      </button>
    </form>
  </fieldset>
  </div>
  <table id="searchtable" class="table table-striped table-bordered">
    <thead>
      <tr>
        <th>ID</th>
        <th>ФИО</th>
        <th>Дата звонка</th>
        <th>Телефон</th>
        <th>Номер машины</th>
        <th>Программа</th>
      </tr>
    </thead>
    <tbody/>
  </table>
</script>
