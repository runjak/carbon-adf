Router = Backbone.Router.extend({
  routes: {
    "view":        "mainView"
  , "view/:inf":   "mainView"
  , "create":      "createView"
  , "login":       "loginView"
  , "profile":     "profileView"
  , "admin/:page": "adminView"
  , "admin":       "adminView"
  , "about":       "aboutView"
  , "*actions":    "defaultRoute"
  }
});
