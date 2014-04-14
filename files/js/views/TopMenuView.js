TopMenuView = Backbone.View.extend({
  initialize: function(){
    var t = this;
    var activations = {
      'route:createArticleView':       '.article'
    , 'route:singleArticleView':       '.article'
    , 'route:articleView':             '.article'
    , 'route:createDiscussionView':    '.discussion'
    , 'route:singleDiscussionView':    '.discussion'
    , 'route:singleDiscussionViewTab': '.discussion'
    , 'route:discussionView':          '.discussion'
    , 'route:resultView':              '.result'
    , 'route:loginView':               null
    , 'route:singleUserView':          '.user'
    , 'route:userView':                '.user'
    , 'route:defaultRoute':            null
    };
    _.each(activations, function(c, route){
      window.App.router.on(route, function(){t.activate(c);});
    });
  }
, activate: function(c){
    this.$('li.active').removeClass('active');
    if(c !== null)
      this.$(c).addClass('active');
  }
});
