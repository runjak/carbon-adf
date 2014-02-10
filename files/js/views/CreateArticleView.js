CreateArticleView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.preview = new ArticleRender({el: this.$('#CreateArticleViewPreview')});
    this.setModel(new Item());
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
    if(this.model.isNew()){
      this.model.set({commitMessage: 'Created Article from CreateArticleView.'});
    }else{
      this.model.set({commitMessage: 'Edited Article from CreateArticleView.'});
    }
    console.log('CreateArticleView.create(): ' + JSON.stringify(this.model.attributes));
    var view = this
      , promise = this.model.mySave();
    return promise.done(function(d){
      view.model.set(d);
      window.App.views.singleArticleView.setArticle(view.model);
      view.setModel(new Item());
    }).fail(function(f){
      console.log('Failed to create new article!');
      console.log(f);
    });
  }
, setModel: function(m){
    this.model = m;
    this.preview.setModel(m);
    var d = {headline: '', summary: '', content: ''};
    d = $.extend(d, m.get('description'), m.get('article'));
    this.$('#CreateArticleViewHeadline').val(d.headline);
    this.$('#CreateArticleViewDescription').val(d.summary);
    this.$('#CreateArticleViewContent').val(d.content);
  }
, syncHeadline: function(){
    var d = {headline: this.$('#CreateArticleViewHeadline').val()};
    this.model.modify('description', d);
  }
, syncDescription: function(){
    var d = {summary: this.$('#CreateArticleViewDescription').val()};
    this.model.modify('description', d);
  }
, syncContent: function(){
    var a = {content: this.$('#CreateArticleViewContent').val()};
    this.model.modify('article', a);
  }
});
