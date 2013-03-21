CurrentInformation = Information.extend({
  initialize: function(){
    var current = this.loGetId();
    if(current) this.set({id: current});
    this.on('change:id', function(ci){
      ci.loSetId(ci.get('id'));
    });
  }
, loGetId: function(){ return localStorage.currentInformation; }
, loSetId: function(id){ localStorage.currentInformation = id; }
});
