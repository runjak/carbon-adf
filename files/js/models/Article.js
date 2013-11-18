Article = Item.extend({
  urlRoot: 'article/'
, defaults: {}
, initialize: function(){
    var t = this;
    this.paperArticle = new PaperArticle({model: this});
  }
, create: function(){
    var article = this;
    return $.post(this.urlRoot, this.getQuery()).done(function(d){
      article.set(d);
    });
  }
, update: function(){
    var article = this;
    var target  = this.urlRoot + this.get('id');
    return $.put(target, this.getQuery()).done(function(d){
      article.set(d);
    });
  }
, getQuery: function(){
    var q = {
      headline:    this.get('headline')
    , description: this.get('description')
    , content:     this.get('content')
    };
    return q;
  }
, mkDummy: function(n){
    this.set({
      headline:    n
    , description: ''
    , content:     ''
    });
    return this.create();
  }
});
