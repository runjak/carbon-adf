SingleArticleActionView = Backbone.View.extend({
  initialize: function(){
    this.aid        = null;
    this.getArticle = function(){};
    this.history    = null;
    this.collected  = false;
    this.collect    = this.$('#SingleArticleViewActionCollect');
    this.edit       = this.$('#SingleArticleViewActionEdit');
    this.uncollect  = this.$('#SingleArticleViewActionUncollect');
    window.App.login.on('change:loggedIn', this.render, this);
    this.model.bind('reset add remove', this.checkArticleId, this);
    var view = this;
    this.collect.click(function(){
      window.App.collectedArticles.add(view.getArticle());
    });
    this.edit.click(function(){
      window.App.views.createArticleView.setModel(view.getArticle());
      window.App.router.navigate('article/create', {trigger: true});
    });
    this.uncollect.click(function(){
      window.App.collectedArticles.remove(view.getArticle());
    });
    this.$('#SingleArticleViewActionHistory').click(function(){
      var h = view.history;
      if(h === null) return;
      if(h.visible()){
        h.hide();
      }else
        h.show();
    });
    this.$('#SingleArticleViewActionCollected').click(function(){
      window.App.views.collectedArticlesView.toggle();
    });
  }
, render: function(){
    if(this.collected){
      this.uncollect.show();
      this.collect.hide();
    }else{
      this.collect.show();
      this.uncollect.hide();
    }
    if(window.App.login.get('loggedIn')){
      this.edit.show();
    }else
      this.edit.hide();
  }
, checkArticleId: function(aid){
    if(typeof(aid) === 'undefined'){
      aid = this.aid;
    }else if(aid instanceof Item){
      aid = aid.get('id');
    }else this.aid = aid;
    var match = this.model.findWhere({id: aid});
    this.collected = (typeof(match) !== 'undefined');
    this.render();
    return match;
  }
, setGetArticle: function(f){
    this.getArticle = f;
    return this;
  }
, setHistory: function(h){
    this.history = h;
    return this;
  }
});
