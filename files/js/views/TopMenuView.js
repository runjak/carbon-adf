TopMenuView = Backbone.View.extend({
  initialize: function(){
    var t = this;
    window.App.router.on('route:createArticleView', function(){t.activate();});
    window.App.router.on('route:singleArticleView', function(){t.activate('.article');});
    window.App.router.on('route:loginView',         function(){t.activate();});
    window.App.router.on('route:singleUserView',    function(){t.activate('.user');});
    window.App.router.on('route:userView',          function(){t.activate('.user');});
    window.App.router.on('route:defaultRoute',      function(){t.activate();});
  }
, activate: function(c){
    this.$('li.active').removeClass('active');
    if(c !== null)
      this.$(c).addClass('active');
  }
});
