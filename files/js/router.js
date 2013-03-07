var Router = Backbone.Router.extend({
  routes: {
    "login": "loginView"
  , "about": "aboutView"
  , "*actions": "defaultRoute"
  }
});
