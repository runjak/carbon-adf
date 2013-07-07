Article = Item.extend({
  urlRoot: 'article/'
, defaults: {}
, initialize: function(){}
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
    console.log('Article:getQuery: '+JSON.stringify(q));
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
});
