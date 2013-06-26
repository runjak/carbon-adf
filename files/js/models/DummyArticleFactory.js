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
, reset: function(){ //FIXME maybe calculate last i from dummy Articles in Discusison
    this.set({i: 0});
  }
});
