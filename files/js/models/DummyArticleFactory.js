DummyArticleFactory = Backbone.Model.extend({
  defaults: {i: 0} 
, nextName: function(){
    var name = '';
    var i = this.get('i');
    this.set({i: i+1});
    while(i >= 0){
      name = String.fromCharCode(97 + i % 26) + name;
      if(i >= 26){
        i /= 26;
      }else
        i -= 26;
    }
    return name;
  }
, nextDummy: function(){
    var a = new Article();
    var p = $.Deferred();
    a.mkDummy(this.nextName()).done(function(){
      p.resolve(a);
    });
    return p;
  }
/*
  Reset should set a useful i given an ArticleCollection.
  To do this, it will map over the articles,
  and find the dummy with the highest id.
  Than the headline can be converted back to an i.
*/
, reset: function(articles){
    var id = 0;
    var headline = '';
    articles.each(function(a){
      if(a.get('content')     !== '') return;
      if(a.get('description') !== '') return;
      if(a.get('id') <= id) return;
      id = a.get('id');
      headline = a.get('headline');
    });
    var i = 1;
    _.each(headline, function(c){
      c = c.charCodeAt(0) - 97;
      if(i > 1) i *= 26;
      i += c;
    });
    this.set({i: i});
  }
});
