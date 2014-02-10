SingleArticleView = ArticleRender.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    this.renderTarget = this.$('.render');
    this.actions = new SingleArticleActionView({
      el: this.$('#SingleArticleViewActions')
    , model: window.App.collectedArticles
    });
    this.history = new ArticleHistoryView({el: this.$('#SingleArticleHistoryView ul')});
    var view = this;
    this.actions.setGetArticle(function(){
      return view.model;
    }).setHistory(this.history);
    window.App.router.on('route:singleArticleView', function(aid){
      view.setArticleId(aid).always(function(){
        window.App.hideManager.render(view);
      });
    });
    this.setModel(this.model);
  }
, setArticle: function(m){
    this.model = m;
    var aid = m.get('id');
    window.App.router.navigate('#/article/'+aid);
    this.setArticleId(aid);
  }
, setArticleId: function(aid){
    var view = this;
    var p = $.Deferred();
    if(this.model && this.model.get('id') === aid){
      p.resolve(this.model);
    }else{
      var m = this.actions.checkArticleId(aid);
      if(!m) m = new Item({id: aid});
      m.fetch().done(function(){
        view.setModel(m);
        view.history.setModel(m);
        p.resolve(m);
      }).fail(function(f){
        m.set({
          description: {
            headline: 'Article not found'
          , summary: ('There was no article found by the requested id of ' + aid)}
        , article: {content: ''}
        });
        console.log('SingleArticleView:setArticleId('+aid+'):'+JSON.stringify(f));
        view.setModel(m);
        p.reject(m);
      });
    }
    return p;
  }
});
