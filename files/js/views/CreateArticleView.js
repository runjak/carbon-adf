CreateArticleView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.preview = new ArticleRender({el: this.$('#CreateArticleViewPreview')});
    this.setModel(new Article());
    var view = this;
    window.App.router.on('route:createArticleView', function(){
      window.App.hideManager.render(view);
    });
  }
, events: {
    "click #CreateArticle":                "create"
  , "keyup #CreateArticleViewHeadline":    "syncHeadline"
  , "keyup #CreateArticleViewDescription": "syncDescription"
  , "keyup #CreateArticleViewContent":     "syncContent"
  }
, render: function(){}
, create: function(e){
    e.preventDefault();
    var promise = null;
    var view = this;
    if(typeof(this.model.get('id')) === 'undefined'){
      console.log('Calling model.create…');
      promise = this.model.create();
    }else{
      console.log('Calling model.update…');
      promise = this.model.update();
    }
    return promise.done(function(d){
      window.App.views.singleArticleView.setArticle(view.model);
      view.setModel(new Article());
    }).fail(function(f){
      console.log('Failed to create new article!');
      console.log(f);
    });
  }
, setModel: function(m){
    this.model = m;
    this.preview.setModel(m);
    this.$('#CreateArticleViewHeadline').val(m.get('headline'));
    this.$('#CreateArticleViewDescription').val(m.get('description'));
    this.$('#CreateArticleViewContent').val(m.get('content'));
  }
, syncHeadline: function(){this.model.set({headline: this.$('#CreateArticleViewHeadline').val()});}
, syncDescription: function(){this.model.set({description: this.$('#CreateArticleViewDescription').val()});}
, syncContent: function(){this.model.set({content: this.$('#CreateArticleViewContent').val()});}
});
