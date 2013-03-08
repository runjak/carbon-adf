var Router = Backbone.Router.extend({
  routes: {
    "create":   "createView"
  , "login":    "loginView"
  , "profile":  "profileView"
  , "admin":    "adminView"
  , "about":    "aboutView"
  , "*actions": "defaultRoute"
  }
});
