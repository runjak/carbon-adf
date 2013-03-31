Router = Backbone.Router.extend({
  routes: {
    "browse":      "browseView"
  , "view/:inf":   "mainView"
  , "view":        "mainView"
  , "create":      "createView"
  , "discussion":  "discussionView"
  , "login":       "loginView"
  , "profile":     "profileView"
  , "users/:uid":  "userView"
  , "users":       "userView"
  , "admin/:page": "adminView"
  , "admin":       "adminView"
  , "about":       "aboutView"
  , "*actions":    "defaultRoute"
  }
});
