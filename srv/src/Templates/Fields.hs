{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- Form field templates.

-- Field template must have id in form of <type>-field-template,
-- where <type> is field type to be rendered using this
-- template, or <name>-<type>-field-template, where <name> is
-- the name of field of given type which will be rendered with
-- this template. Client code must prefer named templates to
-- only-typed ones.

module Templates.Fields (allFields) where

import Text.Hamlet
import Data.Text (Text)

allFields =
    [hamlet|
     ^{textarea}
     ^{statictext}
     ^{text}
     ^{datetime}
     ^{date}
     ^{phone}
     ^{dictionary}
     ^{picker}
     ^{radioDictionary}
     ^{select}
     ^{checkbox}
     ^{mapTpl}
     ^{table}
     ^{reference}
     ^{group}
     ^{serviceReference}
     ^{actionReference}
     ^{groupView}
     ^{unknownField}
     ^{emptyFields}
     ^{servicePicker}
     ^{checkListItem}
     ^{unknown}
     ^{defaultCaseGroup}
     |]

withMeta :: Text -> (t -> Html) -> t -> Html
withMeta id c =
    [hamlet|
     <script type="text/template"
             class="field-template"
             id=#{id}>
       <div class="control-group"
            {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
            {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
           ^{c}
     |]

regField :: Text -> (t -> Html) -> t -> Html
regField id c =
    [hamlet|
     <script type="text/template"
             class="field-template"
             id=#{id}>
       <div class="control-group">
     |]

infoText =
    [hamlet|
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText }}" />
            {{/ meta.infoText }}
     |]

withMetaInfo :: Text -> (t -> Html) -> t -> Html
withMetaInfo id c = withMeta id
    [hamlet|
     ^{infoText}
     ^{c}
     |]

textarea = withMetaInfo "textarea-field-template"
    [hamlet|
     <div class="controls">
       <textarea class="pane-span focusable"
                 name="{{ name }}"
                 {{# readonly }}disabled{{/ readonly }}
                 rows="7"
                 data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
       |]

statictext = regField "statictext-field-template"
    [hamlet|
     <span data-bind="text: {{ name }}">
     |]

text = withMetaInfo "text-field-template"
       [hamlet|
        <div class="controls">
          <input type="text"
                 class="pane-span focusable"
                 autocomplete="off"
                 name="{{ name }}"
                 {{# meta.transform }}
                    style="text-transform:{{meta.transform}};"
                 {{/ meta.transform }}
                 {{# readonly }}readonly{{/ readonly }}
                 data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
        |]

datetime = withMetaInfo "datetime-field-template"
           [hamlet|
            <div class="controls">
              <input type="text"
                     class="pane-span focusable"
                     autocomplete="off"
                     name="{{ name }}"
                     {{# readonly }}readonly{{/ readonly }}
                     data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
            |]

-- Like text-field-template, but with datepicker
date = withMetaInfo "date-field-template"
       [hamlet|
        <div class="controls">
          <div class="input-append date"
               data-provide="datepicker"
               data-autoshow-datepicker="true"
               data-date-format="dd.mm.yyyy"
               data-date-weekstart="1">
            <input type="text"
                   class="pane-span focusable"
                   autocomplete="off"
                   name="{{ name }}"
                   {{# readonly }}readonly{{/ readonly }}
                   data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
            <span class="add-on">
              <i class="icon icon-calendar">
        |]

-- Like text-field-template but with call button -->
-- FIXME: this template differs from the picker-field-template
--        only in icon class. Seems that it is reasonable to parametrize it.

phone = withMetaInfo "phone-field-template"
        [hamlet|
         <div class="control-label">
           <label>{{ meta.label }}
         <div class="controls">
           <div class="input-append">
             <input type="text"
                    class="pane-span focusable"
                    autocomplete="off"
                    name="{{ name }}"
                    data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
             <span class="add-on">
               <i class="icon stolen-icon-phone"
                  onclick="doPick('{{ meta.picker }}', '{{ name }}');">
         |]

dictionary = withMetaInfo "dictionary-field-template"
             [hamlet|
              <div class="controls">
                <div class="input-append">
                  -- Note the difference between readonly attribute and
                  -- disabled class from Bootstrap.
                  <input type="text"
                         class="pane-span focusable {{# meta.addClass }}{{meta.addClass}}{{/ meta.addClass }} {{# readonly }}disabled{{/ readonly }}"
                         {{# readonly }}readonly{{/ readonly }}
                         autocomplete="off"
                         name="{{ name }}"
                         data-source="global.dictionaries['{{meta.dictionaryName}}']"
                         data-bind="value: {{ name }}Local, valueUpdate: 'afterkeydown' {{# meta.dictionaryParent }}, attr: { 'data-parent': {{ meta.dictionaryParent }} } {{/ meta.dictionaryParent }}"
                         data-provide="typeahead">
                  <span class="add-on">
                    <i class="icon icon-chevron-down"
                      {{^readonly}}data-provide="typeahead-toggle"{{/readonly}}
                    >
                {{# meta.targetCategory }}
                <ul data-depends="{{ name }}"
                    data-source="{{ meta.targetCategory }}"
                    data-provide="checklist" >
                {{/ meta.targetCategory }}
                |]

-- Picker which fills fields with stored data
picker = withMetaInfo "picker-field-template"
         [hamlet|
          <div class="controls">
            <div class="input-append">
              <input type="text"
                     class="pane-span focusable {{# readonly }}disabled{{/ readonly }}"
                     autocomplete="off"
                     {{# readonly }}readonly{{/ readonly }}
                     {{# meta.transform }}
                        style="text-transform:{{meta.transform}};"
                     {{/ meta.transform }}
                     name="{{ name }}"
                     data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
              <span class="add-on"><i class="icon icon-search"
                 onclick="doPick('{{ meta.picker }}', '{{ name }}', event.srcElement);"/>
              |]

-- radio widget for flat dictionary fields
radioDictionary = regField "radio-dictionary-field-template"
        [hamlet|
         ^{infoText}
         <div class="controls">
           {{# dictionary.entries }}
             <label class="radio">
               <!-- Mustache.js contexts support bubbling -->
               <input type="radio"
                      name="{{ name }}"
                      value="{{ value }}"
                      data-bind="checked: {{ name }}">
               {{ label }}
           {{/ dictionary.entries }}
         |]

-- May be used for plain rendering of flat dictionaries as well
select = regField "select-field-template"
         [hamlet|
          ^{infoText}
          <select name="{{ name }}"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }}, valueUpdate: 'change'">
            {{# dictionary.entries }}
            <option value="{{value}}">{{meta.label}}
            {{/ dictionary.entries }}
          |]

checkbox = regField "checkbox-field-template"
           [hamlet|
            <div class="controls">
              <label class="checkbox inline">
                <input type="checkbox"
                       name="{{ name }}"
                       {{# readonly }}disabled{{/ readonly }}
                       data-bind="checked: {{ name }}, valueUpdate: 'change'">
              {{ meta.label }}
          {{# meta.infoText }}
            <i class="icon icon-question-sign"
               data-provide="popover"
               data-content="{{ meta.infoText }}" />
          {{/ meta.infoText }}
            |]

mapTpl = regField "map-field-template"
      [hamlet|
       <div class="controls">
         <div style="height:600px;" id="{{ name }}" class="osMap">
       |]

table = regField "table-field-template"
        [hamlet|
         <div class="controls">
           <table id="{{ name }}"
                  class="dataTable table table-striped table-bordered">
             <thead>
               <tr>
                 {{# meta.columns }}
                 <th>{{ label }}
                 {{/ meta.columns }}
             <tbody>
         |]

-- NOP here — references are rendered after model has loaded
reference =
    [hamlet|
     <script type="text/template"
             class="field-template"
             id="reference-field-template">
     |]

-- Special template used to render first field of group in parent view.
group = regField "group-field-template"
        [hamlet|
         ^{infoText}
         <div class="controls">
           <div class="input-append">
             <input type="text"
                    class="pane-span"
                    autocomplete="off"
                    {{# meta.transform }}
                       style="text-transform:{{meta.transform}};"
                    {{/ meta.transform }}
                    onfocus="showComplex('{{ viewName }}', '{{ name }}');"
                    {{# readonly }}readonly{{/ readonly }}
                    data-bind="value: {{ name }}, valueUpdate: 'afterkeydown'">
             <span class="add-on">
               <i onclick="showComplex('{{ viewName }}', '{{ name }}');"
                  class="icon icon-share">
           |]


-- Template for one of references.

-- Must generate id="{{ refView }}" element which
-- will hold contents of referenced model. Its class must be is
-- {{ refClass }}.

-- "{{ refView }}-perms" will be used for instance permissions.

-- May setup on-demand loading function.
referenceTpl :: Text -> Text -> Text -> t -> Html
referenceTpl id bind name =
    let dataTarget :: Text
        dataTarget = "#{{ refView }}-head"
    in [hamlet|
        <script type="text/template"
                class="reference-template"
                id="services-reference-template">
          <div class="accordion-group">
            <div class="accordion-heading">
              <a class="accordion-toggle"
                 id="{{ refView }}-link"
                 data-bind="#{bind}"
                 data-target="#{dataTarget}"
                 data-toggle="collapse">
               #{name}

            <div id="{{ refView }}-head"
                 class="accordion-body collapse in">
              <div class="accordion-inner {{ refClass }}"
                   id="{{ refView }}">
                <!-- Instance contents are rendered here -->
            |]

serviceReference =
    referenceTpl "services-reference-template" "text: modelTitle" "Услуга…"

actionReference =
    referenceTpl "actions-reference-template" "text: nameLocal" "Действие…"

-- Group view container
groupView =
    [hamlet|
     <script type="text/template"
             class="group-template"
             id="-group-template">
       <fieldset class="complex-field"
                 id="{{ refView }}"
                 style="display: none;">
           <i class="icon icon-remove complex-field-close"
              onclick="hideComplex()"/>
           <form class="content form-vertical"/>
     |]

-- Template for fields with unknown type
unknownField = regField "unknown-field-template"
    [hamlet|
     <div class="controls">
       <span class="label label-important">
         (Ошибка — поле {{ name }} неизвестного типа)
     |]

-- List of empty required fields
emptyFields =
    [hamlet|
     <script type="text/template"
             id="empty-fields-template">
       <ul id="empty-fields">
       {{# fields }}
       <li onclick="focusField('{{name}}'); return false;"
           data-bind="css: { lierror: {{name}}Not }, visible: {{name}}Not">
         {{meta.label}}
       {{/ fields }}
     |]

-- Render service picker with services dictionary
servicePicker =
    [hamlet|
     <script type="text/template"
             id="service-picker-template">
       <ul class="nav nav-pills">
         <li class="dropdown">
           <button class="dropdown-toggle btn btn-action"
                   type="button"
                   data-toggle="dropdown">
             <i class="icon icon-plus">
             Добавить услугу
           <ul class="dropdown-menu">
             {{# dictionary.entries }}
             <li>
               <a href="#" onclick="addService('{{value}}');">
                 <i class="icon-{{icon}} icon-black">
                 {{ label }}
             {{/ dictionary.entries }}
      |]

checkListItem =
    [hamlet|
     <script type="text/template"
            id="check-list-item-template">
      <li>
        <input type="checkbox">
        {{ label }}
     |]

-- Fallback template for pickTemplate failures
unknown =
    [hamlet|
     <script type="text/template"
             id="unknown-template">
       <span class="label label-important">
         Не удалось найти ни один из шаблонов:
         {{#names}}{{.}}&nbsp;{{/names}}
     |]

-- Default case group view template
defaultCaseGroup =
    [hamlet|
     <script type="text/template"
             class="group-template"
             id="default-case-group-template">
       <fieldset class="complex-field default-complex-field"
                 id="default-case-complex-field">
          <div class="program">
            <h1 data-bind="text: programLocal">
            <div data-bind="html:programDesc">
            <br>
            <br>
           <div data-bind="foreach: servicesDescs">
             <dt data-bind="text: title">
             <dd data-bind="html: description">
     |]