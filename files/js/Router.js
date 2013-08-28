Router = Backbone.Router.extend({
  routes: {
    "article/create":      "createArticleView"
  , "article/:id":         "singleArticleView"
  , "article":             "articleView"
  , "discussion/create":   "createDiscussionView"
  , "discussion/:id":      "singleDiscussionView"
  , "discussion/:id/:tab": "singleDiscussionViewTab"
  , "discussion":          "discussionView"
  , "login":               "loginView"
  , "user":                "userView"
  , "user/:id":            "singleUserView"
  , "*actions":            "defaultRoute"
  }
, watchClick: function(target){
    var router = this;
    var href   = target.attr('href');
    target.click(function(){
      router.navigate(href, {trigger: true});
    });
  }
});
