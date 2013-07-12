Article = Item.extend({
  urlRoot: 'article/'
, defaults: {}
, initialize: function(){
    this.paperArticle = null;
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
, setPosition: function(p){
    var url = this.urlRoot
            + this.get('id')
            + '/collection/'
            + this.get('collectionId')
            + '/position';
    var article = this;
    return $.put(url, p).done(function(){article.set(p);});
  }
, getXY: function(){
    return {
      x: this.get('posX')
    , y: this.get('posY')
    };
  }
});
