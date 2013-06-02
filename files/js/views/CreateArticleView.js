CreateArticleView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.preview = new ArticleView({el: this.$('#CreateArticleViewPreview')});
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
, render: function(){
    console.log('CreateArticleView:render()');
  }
, create: function(e){
    e.preventDefault();
    console.log('CreateArticleView:create()');
    var view = this;
    this.model.create(
      this.model.get('headline')
    , this.model.get('description')
    , this.model.get('content')
    ).done(function(d){
      console.log('New article created!');
      console.log(d);
      //FIXME route to new content
      //FIXME set clean model
    }).fail(function(f){
      console.log('Failed to create new article!');
      console.log(f);
    });
  }
, setModel: function(m){
    this.model = m;
    this.preview.setModel(m);
  }
, syncHeadline: function(){this.model.set({headline: this.$('#CreateArticleViewHeadline').val()});}
, syncDescription: function(){this.model.set({description: this.$('#CreateArticleViewDescription').val()});}
, syncContent: function(){this.model.set({content: this.$('#CreateArticleViewContent').val()});}
});
