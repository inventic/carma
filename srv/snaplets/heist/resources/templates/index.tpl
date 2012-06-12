<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>CaRMa</title>
    <link rel="stylesheet" href="/s/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/s/css/datepicker.css" />
    <link rel="stylesheet" href="/s/css/jquery.dataTables.css" />

    <!-- Additional set of icons -->
    <link rel="stylesheet" href="/s/css/stolen-icons.css" />

    <link rel="stylesheet" href="/s/css/local.css" />
    <!-- DOM manipulation -->
    <script src="/s/js/3p/jquery-1.7.1.min.js" />

    <!-- Rich UI -->
    <script src="/s/js/3p/bootstrap.min.js" />
    <script src="/s/js/3p/bootstrap-datepicker.js" />
    <script src="/s/js/3p/bootstrap-typeahead.js" />
    <script src="/s/js/3p/bootstrap-popover.js" />

    <!-- Tabular display -->
    <script src="/s/js/3p/jquery.dataTables.min.js" />

    <!-- Responsive UI javascript library -->
    <script src="/s/js/3p/knockout-2.0.0.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/s/js/3p/underscore-1.3.1.min.js" />

    <!-- Loose MVC -->
    <script src="/s/js/3p/backbone-0.9.1.min.js" />

    <!-- Knockback is a Knockout + Backbone glue -->
    <script src="/s/js/3p/knockback-0.13.min.js" />

    <!-- Simple templates -->
    <script src="/s/js/3p/mustache.js" />

    <!-- OpenLayers library allows map rendering -->
    <script src="http://www.openlayers.org/api/OpenLayers.js" />

    <!-- 25Kb of date parsing and formatting -->
    <script src="/s/js/3p/date-ru-RU.js" />


    <!-- Model processing -->
    <script src="/s/js/metamodel.js" />
    <script src="/s/js/search.js" />
    <script src="/s/js/main.js" />
    <script src="/s/js/local.js" />
    <script src="/s/js/viewsware.js" />
  </head>
  <body>

    <apply template="nav-bar" />

    <!-- Main container for dynamically rendered layouts -->
    <div class="container-fluid">
      <div class="row-fluid" id="layout" />
    </div>

    <applyDir template-dir="screens" />

    <!--
         Form field templates.

         Field template must have id in form of <type>-field-template,
         where <type> is field type to be rendered using this
         template, or <name>-<type>-field-template, where <name> is
         the name of field of given type which will be rendered with
         this template. Client code must prefer named templates to
         only-typed ones.

      -->

    <script type="text/template"
            class="field-template"
            id="textarea-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">             
          <textarea class="pane-span focusable"
                    name="{{ name }}"
                    {{# readonly }}disabled{{/ readonly }}
                    rows="7"
                    data-bind="value: {{ name }},
                               valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="statictext-field-template">
      <div class="control-group">
          <span data-bind="text: {{ name }}" />
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="text-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <input type="text"
                 class="pane-span focusable"
                 name="{{ name }}"
                 {{# meta.transform }}
                    style="text-transform:{{meta.transform}};"
                 {{/ meta.transform }}
                 {{# readonly }}readonly{{/ readonly }}
                 data-bind="value: {{ name }},
                            valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="datetime-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <input type="text"
                 class="pane-span focusable"
                 name="{{ name }}"
                 {{# readonly }}readonly{{/ readonly }}
                 data-bind="value: {{ name }}" />
        </div>
      </div>
    </script>

    <!-- Like text-field-template, but with datepicker -->
    <script type="text/template"
            class="field-template"
            id="date-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append date"
               data-provide="datepicker"
               data-autoshow-datepicker="true"
               data-date-format="dd.mm.yyyy"
               data-date-weekstart="1">
            <input type="text"
                   class="pane-span focusable"
                   name="{{ name }}"
                   {{# readonly }}readonly{{/ readonly }}
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'" />
            <span class="add-on"><i class="icon icon-calendar" /></span>
          </div>
        </div>
      </div>
    </script>

    <!-- Like text-field-template but with call button -->
    <!-- FIXME: this template differs from the picker-field-template
         only in icon class. Seems that it is reasonable to parametrize it.
    -->
    <script type="text/template"
            class="field-template"
            id="phone-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}</label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   class="pane-span focusable"
                   name="{{ name }}"
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'"/>
            <span class="add-on">
              <i class="icon stolen-icon-phone"
                 onclick="doPick('{{ meta.picker }}', '{{ name }}');"/>
            </span>
          </div>
        </div>
      </div>
    </script>


    <script type="text/template"
            class="field-template"
            id="dictionary-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <!-- 

            Note the difference between readonly attribute and
            disabled class from Bootstrap.

            -->
            
            <input type="text"
                   class="pane-span
                          focusable
                          {{# meta.addClass }}{{meta.addClass}}{{/ meta.addClass }}
                          {{# readonly }}disabled{{/ readonly }}"
                   {{# readonly }}readonly{{/ readonly }}
                   name="{{ name }}"
                   data-source="global.dictionaries['{{meta.dictionaryName}}']"
                   data-bind="value: {{ name }}Local,
                              valueUpdate: 'afterkeydown'
                              {{# meta.dictionaryParent }},
                              attr: { 'data-parent': {{ meta.dictionaryParent }} }
                              {{/ meta.dictionaryParent }}"
                   data-provide="typeahead" />
            <span class="add-on">
              <i class="icon icon-chevron-down"
                 data-provide="typeahead-toggle" />
            </span>
          </div>
          {{# meta.targetCategory }}
          <ul data-depends="{{ name }}"
              data-source="{{ meta.targetCategory }}"
              data-provide="checklist" />
          {{/ meta.targetCategory }}
        </div>
      </div>
    </script>

    <!-- Picker which fills fields with stored data -->
    <script type="text/template"
            class="field-template"
            id="picker-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { error: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   class="pane-span focusable {{# readonly }}disabled{{/ readonly }}"
                   {{# readonly }}readonly{{/ readonly }}
                   {{# meta.transform }}
                      style="text-transform:{{meta.transform}};"
                   {{/ meta.transform }}
                   name="{{ name }}"
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'"/>
            <span class="add-on"><i class="icon icon-search"
                                    onclick="doPick('{{ meta.picker }}');"/></span>
          </div>
        </div>
      </div>
    </script>

    <!-- radio widget for flat dictionary fields -->
    <script type="text/template"
            class="field-template"
            id="radio-dictionary-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          {{# dictionary.entries }}
            <label class="radio">
              <!-- Mustache.js contexts support bubbling -->
              <input type="radio"
                     name="{{ name }}"
                     value="{{ value }}"
                     data-bind="checked: {{ name }}"></input>
              {{ label }}
            </label>
          {{/ dictionary.entries }}
        </div>
      </div>
    </script>

    <!-- May be used for plain rendering of flat dictionaries as well -->
    <script type="text/template"
            class="field-template"
            id="select-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <select name="{{ name }}"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }},
                             valueUpdate: 'change'">
            {{# dictionary.entries }}
            <option value="{{value}}">{{meta.label}}</option>
            {{/ dictionary.entries }}
          </select>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="checkbox-field-template">
      <div class="control-group">
        <div class="controls">
          <label class="checkbox inline">
            <input type="checkbox"
                   name="{{ name }}"
                   {{# readonly }}disabled{{/ readonly }}
                   data-bind="checked: {{ name }},
                              valueUpdate: 'change'" />
          {{ meta.label }}
          {{# meta.infoText }}
            <i class="icon icon-question-sign"
               data-provide="popover"
               data-content="{{ meta.infoText }}" />
          {{/ meta.infoText }}
          </label>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="map-field-template">
      <div class="control-group">
        <div class="controls">
          <div style="height:600px;" id="{{ name }}" class="osMap"></div>
        </div>
      </div>
    </script>
 
    <script type="text/template"
            class="field-template"
            id="table-field-template">
      <div class="control-group">
        <div class="controls">
          <table id="{{ name }}" class="dataTable table table-striped table-bordered">
            <thead>
              <tr>
                {{# meta.columns }}<th>{{ label }}</th>{{/ meta.columns }}
              </tr>
            </thead>
            <tbody/>
          </table>
        </div>
      </div>
    </script>

    <!-- NOP here — references are rendered after model has loaded -->
    <script type="text/template" 
            class="field-template"
            id="reference-field-template" />

    <!-- 

         Special template used to render first field of group in
         parent view.
    -->
    <script type="text/template" 
            class="field-template"
            id="group-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   class="pane-span"
                   {{# meta.transform }}
                      style="text-transform:{{meta.transform}};"
                   {{/ meta.transform }}
                   onfocus="showComplex('{{ viewName }}', '{{ name }}');"
                   {{# readonly }}readonly{{/ readonly }}
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'" />
            <span class="add-on">
              <i onclick="showComplex('{{ viewName }}', '{{ name }}');"
                 class="icon icon-share" />
            </span>
          </div>
        </div>
      </div>
    </script>

    <!-- 
    
         Template for one of references.
    
         Must generate id="{{ refView }}" element which
         will hold contents of referenced model. Its class must be is
         {{ refClass }}.

         "{{ refView }}-perms" will be used for instance permissions.

         May setup on-demand loading function.
    -->
    <script type="text/template"
            class="reference-template"
            id="services-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <a class="accordion-toggle"
             id="{{ refView }}-link"
             data-bind="text: modelTitle"
             data-target="#{{ refView }}-head"
             data-toggle="collapse">Услуга…</a>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}" 
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="reference-template"
            id="actions-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <a class="accordion-toggle"
             id="{{ refView }}-link"
             data-bind="text: nameLocal"
             data-target="#{{ refView }}-head"
             data-toggle="collapse">Действие…</a>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}" 
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
        </div>
      </div>
    </script>

    <!-- Group view container -->
    <script type="text/template"
            class="group-template"
            id="-group-template">
      <fieldset class="complex-field"
                id="{{ refView }}"
                style="display: none;">
          <i class="icon icon-remove complex-field-close"
             onclick="hideComplex()"/>
          <form class="content form-vertical"/>
      </fieldset>
    </script>

    <!-- Template for fields with unknown type -->
    <script type="text/template"
            class="field-template"
            id="unknown-field-template">
      <div class="control-group">
        <div class="controls">
          <span class="label label-important">
            (Ошибка — поле {{ name }} неизвестного типа)
          </span>
        </div>
      </div>
    </script>

    <!-- Form controls wrt user permissions -->
    <script type="text/template"
            id="permission-template">
        {{# readonly }}
        <button class="btn disabled" type="button">
          <i class="icon-ban-circle" /> Только для чтения</button>
        {{/ readonly }}
        {{^ readonly }}
        <button class="btn btn-success" type="button"
                onClick="saveInstance('{{ viewName }}');successfulSave.call(this);">
          <i class="icon-pencil icon-white" /> Сохранить</button>
          <span class="save-result"/>
        {{/ readonly }}
    </script>

    <!-- List of empty required fields -->
    <script type="text/template"
            id="empty-fields-template">
      <ul id="empty-fields">
      {{# fields }}
      <li onclick="focusField('{{name}}'); return false;"
          data-bind="css: { lierror: {{name}}Not }, visible: {{name}}Not">{{meta.label}}</li>
      {{/ fields }}
      </ul>
    </script>

    <!-- Render service picker with services dictionary -->
    <script type="text/template"
            id="service-picker-template">
      <ul class="nav nav-pills">
        <li class="dropdown">
          <button class="dropdown-toggle btn btn-action"
                  type="button"
                  data-toggle="dropdown">
            <i class="icon icon-plus" />Добавить услугу
          </button>
          <ul class="dropdown-menu">
            {{# dictionary.entries }}
            <li>
              <a href="#" onclick="addService('{{value}}');">
                <i class="icon-{{icon}} icon-black" />
                {{ label }}
              </a>
            </li>
            {{/ dictionary.entries }}
          </ul>
        </li>
      </ul>
    </script>

    <script type="text/template"
            id="check-list-item-template">
      <li><input type="checkbox" /> {{ label }}</li>
    </script>

    <!-- Fallback template for pickTemplate failures -->
    <script type="text/template"
            id="unknown-template">
      <span class="label label-important">
        Не удалось найти ни один из шаблонов:
        {{#names}}{{.}}&nbsp;{{/names}}
      </span>
    </script>

    <!-- Default case group view template -->
    <script type="text/template"
            class="group-template"
            id="default-case-group-template">
      <fieldset class="complex-field default-complex-field"
                id="default-case-complex-field">
          <div data-bind="foreach: servicesDescs">
            <span data-bind="text: $data"></span>
          </div>
      </fieldset>
    </script>

  </body>
</html>
