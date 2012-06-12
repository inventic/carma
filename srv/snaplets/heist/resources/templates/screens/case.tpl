<!-- Case screen layout -->
<script type="text/template"
        class="screen-template"
        id="case-screen-template">
  <!-- Main case form -->
  <div id="left" class="nice-scrollbar pane">
    <form class="form-vertical">
      <div class="control-group">
        <div class="control-label">
          <label>Номер</label>
        </div>
        <div class="controls">
          <input type="text" disabled id="case-number"
                 data-bind="value: maybeId"/>
        </div>
      </div>
      <div id="case-form" />
      
      <div class="control-group">
        <div class="control-label">
          <label>Услуги</label>
        </div>
        <div class="controls">
          <span class="accordion" id="case-services-references" />
          
          <span id="service-picker-container" />
        </div>
      </div>
      
      <div id="case-permissions" />
    </form>
  </div>

  <!-- Central pane with subform -->
  <!--
  TODO Should be spanN when fluid containers are fixed in
       Bootstrap upstream. -->
  <div id="center" class="nice-scrollbar pane">
  </div>

  <!-- Rightmost pane with list of empty fields and action notes
  -->
  <div id="right" class="nice-scrollbar pane">
    <form class="form-vertical">
      <div class="control-group">
        <div class="controls">
          <span class="accordion" id="case-actions-references" />
        </div>
      </div>
    </form>
    <div id="empty-fields-placeholder" />
  </div>
</script>
