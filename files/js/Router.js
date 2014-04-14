/**
  The central routing of the application :)
*/
Router = Backbone.Router.extend({
  /** How routes look, and what their names are: */
  routes: {
    "article/create":      "createArticleView"
  , "article/:id":         "singleArticleView"
  , "article":             "articleView"
  , "discussion/create":   "createDiscussionView"
  , "discussion/:id":      "singleDiscussionView"
  , "discussion/:id/:tab": "singleDiscussionViewTab"
  , "discussion":          "discussionView"
  , "login":               "loginView"
  , "result":              "resultView"
  , "result/:id":          "singleResultView"
  , "user":                "userView"
  , "user/:id":            "singleUserView"
  , "*actions":            "defaultRoute"
  }
/**
  Takes a jQuery object, and starts watching that for click events.
  On click, it navigates the router with the objects href attr, triggering events.
*/
, watchClick: function(target){
    var router = this;
    var href   = target.attr('href');
    target.click(function(){
      router.navigate(href, {trigger: true});
    });
  }
});
