SingleArticleView = ArticleRender.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    var view = this;
    window.App.router.on('route:singleArticleView', function(aid){
      view.setArticleId(aid).always(function(){
        window.App.hideManager.render(view);
      });
    });
    this.setModel(this.model);
  }
, setArticleId: function(aid){
    var view = this;
    var p = $.Deferred();
    if(this.model && this.model.get('id') === aid){
      p.resolve(this.model);
    }else{
      var m = new Article({id: aid});
      console.log(m);
      m.fetch().done(function(){
        view.setModel(m);
        p.resolve(m);
      }).fail(function(f){
        m.set({
          headline: 'Article not found'
        , description: ('There was no article found by the requested id of ' + aid)
        });
        console.log('SingleArticleView:setArticleId('+aid+'):'+JSON.stringify(f));
        view.setModel(m);
        p.reject(m);
      });
    }
    return p;
  }
});
