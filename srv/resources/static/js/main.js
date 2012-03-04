/// Screen layout rendering, loading models.

$(function(){
    // Screens have top-level template and a number of views.
    //
    // Each view has setup function which accepts DOM element, screen
    // arguments and renders HTML to element and returns viewsWare
    // value for the view element.
    //
    // Screen rendering is called through router.
    var Screens = {
        "case":
            {
                "template": "case-screen-template",
                "views":
                    {
                        "form": setupCaseMain
                    }
            },
        "search":
            {
                "template": "search-screen-template",
                "views":
                    {
                        "tableView": setupSearchTable
                    }
            }
    };

    // Setup routing
    var MenuRouter = Backbone.Router.extend({
        routes: {
            "case/:id/": "loadCase",
            "case/": "newCase",
            "search/": "search"
        },

        loadCase: function (id) {
            renderScreen("case", {"id": id});
        },

        newCase: function () {
            renderScreen("case", {"id": null});
        },

        search: function () {
            renderScreen("search");
        }
    });

    window.global = {
        // «Screen» element which holds all views
        topElement: $el("layout"),
        screens: Screens,
        router: new MenuRouter,

        activeScreen: null,
        // viewWare is for bookkeeping of views in current screen.
        //
        // Hash keys are DOM tree element IDs associated with the
        // model (view names). Values are hashes which contain the
        // following keys:
        //
        // - model (model definition);
        // - modelName;
        // - mkBackboneModel (Backbone constructor);
        // - knockVM (Knockout ViewModel bound to view).
        //
        // When screen is loaded, viewsWare should generally contain
        // only keys which correspond to that screen views.
        viewsWare: {}
    };
    renderScreen("search");

    Backbone.history.start({pushState: false});
});

function el(id) {
    return document.getElementById(id);
}

function $el(id) {
    return $(el(id));
}

// Render top-level screen template (static)
//
// args object is passed further to all view setup functions.
function renderScreen(screenName, args) {
    var screen = global.screens[screenName];
    global.activeScreen = screen;

    // Highlight the new item in navbar
    $("li.active").removeClass("active");
    $el(screenName + "-screen-nav").addClass("active");

    var tpl = $el(screen.template).html();
    global.topElement.html(tpl);
    for (viewName in screen.views) {
        global.viewsWare[viewName] =
            screen.views[viewName]($el(viewName), args);
    }
}

// Remove all content of view and clean up wares.
function forgetView(viewName) {
    var vW = global.viewsWare[viewName];
    kb.vmRelease(vW.knockVM);
    vW = {};
    $el(viewName).empty();
}

// Clean up all views on screen and everything.
function forgetScreen() {
    for (viewName in global.viewsWare) {
        forgetView(viewName);
    };
    global.topElement.empty();
    global.viewsWare = {};
    global.activeScreen = null;
}

function setupCaseMain(el, args) {
    return modelSetup("case")(el, args.id);
}

function setupSearchTable(el, args) {
    el.html($el("search-table-template").html());
    $el("searchtable").dataTable({
        aoColumnDefs: [{
            // Render case id as link to case page
            fnRender: function (o, val) {
                return "<a href='#' onclick=\"renderScreen('case', {id:" + 
                    val + "});\">" + val + "<a/>";
            },
            aTargets: [0]
        }], 
        oLanguage: {
            sSearch: "Фильтр",
            oPaginate: {
                sNext: "Вперёд",
                sPrevious: "Назад"
            },
            sInfo: "Показаны записи с _START_ по _END_ (всего _TOTAL_)",
            sInfoEmpty: "",
            sZeroRecords: "Ничего не найдено"
        }});
    return {};
}

// Manually load JSON data from server and add it to table
//
// TODO Allow adjusting of search fields etc.
function doSearch() {
    var t = $el("searchtable").dataTable();
    var term = $el("table-query").val();
    var method = "search/?";
    var fields = ["id", "ownerSurname", "callDate", "phone", "plateNum", "program", "service"];
    for (f in fields) {
        method += (fields[f] + "=" + term + "&");
    };
    method += "_matchType=s&_searchType=or&_limit=100";
    $.getJSON(modelMethod("case", method),
              function(results) {
                  var data = []
                  for (i = 0; i < results.length; i++) {
                      var o = results[i];
                      var row = [];
                      // Extract fields data from JSON, show dash in
                      // case of undefined field
                      for (j = 0; j < fields.length; j++) {
                          if (_.isUndefined(o[fields[j]]))
                              row[j] = "—";
                          else
                              row[j] = o[fields[j]];
                      }
                      data[i] = row;
                  }
                  t.fnClearTable();
                  t.fnAddData(data);
              });
}

// Return function which will setup views for that model given its
// view element and instance id.
function modelSetup(modelName) {
    return function(el, id) {
        $.getJSON(modelMethod(modelName, "model"),
            function(model) {
                mkBackboneModel = backbonizeModel(model, modelName);
                var idHash = {};
                if (id)
                    idHash = {id: String(id)}
                instance = new mkBackboneModel(idHash);
                knockVM = new kb.ViewModel(instance);

                el.html(renderFormView(model));
                ko.applyBindings(knockVM);

                // Wait a bit to populate model fields and bind form
                // elements without PUT-backs to server
                window.setTimeout(function () {
                    knockVM._kb_vm.model.setupServerSync();
                }, 1000);

                // Return wares produced by view
                return {
                    "model": model,
                    "modelName": modelName,
                    "mkBackboneModel": mkBackboneModel,
                    "knockVM": knockVM
                };
            });
    }
}

function initOSM() {
      window.osmap = new OpenLayers.Map("basicMap");
      var mapnik = new OpenLayers.Layer.OSM();
      osmap.addLayer(mapnik);
      osmap.setCenter(new OpenLayers.LonLat(37.617874,55.757549) // Center of the map
        .transform(
          new OpenLayers.Projection("EPSG:4326"), // transform from WGS 1984
          new OpenLayers.Projection("EPSG:900913") // to Spherical Mercator Projection
        ), 16 // Zoom level
      );
}

/// Model functions.

// Model method HTTP access point wrt redson location
function modelMethod(modelName, method) {
    return "/_/" + modelName + "/" + method;
}

// Save current model instance
function save() {
    global.knockVM._kb_vm.model.save();
}

// Save current model instance and start fresh form
function proceed() {
    save();
    forgetView();
    setupView(new global.mkBackboneModel);
}

// Load existing model instance
function restore(id) {
    forgetView();
    setupView(new global.mkBackboneModel({"id": String(id)}));
}

// Remove currently loaded instance from storage and start fresh form
function remove(id) {
    global.knockVM._kb_vm.model.destroy();
    forgetView();
    setupView(new global.mkBackboneModel);
}
