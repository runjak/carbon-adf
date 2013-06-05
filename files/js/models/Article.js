Article = Backbone.Model.extend({
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
, dayFromTime: function(t){
    var t = /([^ ]+) /.exec(t);
    if(t.length === 2)
      return t[1]; 
    return '';
  }
, stripFractionFromTime: function(t){
    var t = /([^\.]+)\./.exec(t);
    if(t.length === 2)
      return t[1];
    return '';
  }
});
