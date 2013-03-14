Router = Backbone.Router.extend({
  routes: {
    "create":      "createView"
  , "login":       "loginView"
  , "profile":     "profileView"
  , "admin/:page": "adminView"
  , "admin":       "adminView"
  , "about":       "aboutView"
  , "*actions":    "defaultRoute"
  }
});
