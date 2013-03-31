CurrentInformation = Information.extend({
  initialize: function(){
    var current = this.loGetId();
    if(current) this.setId(current);
    this.on('change:id', function(ci){
      ci.loSetId(ci.get('id'));
    });
  }
, loGetId: function(){ return localStorage.currentInformation; }
, loSetId: function(id){ localStorage.currentInformation = id; }
, setId:   function(id){
    if(id === this.get('id')) return;
    var t = this;
    var i = new Information({id: id});
    i.fetch().done(function(){
      t.replaceWith(i);
    });
  }
, replaceWith: function(i){
    this.attributes = {};
    this.set(i.attributes);
    this.loSetId(this.get('id'));
  }
});
