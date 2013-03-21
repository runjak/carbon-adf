Router = Backbone.Router.extend({
  routes: {
    "view/:inf":   "mainView"
  , "view":        "mainView"
  , "create":      "createView"
  , "login":       "loginView"
  , "profile":     "profileView"
  , "admin/:page": "adminView"
  , "admin":       "adminView"
  , "about":       "aboutView"
  , "*actions":    "defaultRoute"
  }
});
