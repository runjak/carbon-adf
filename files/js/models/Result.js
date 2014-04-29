Result = Backbone.Model.extend({
  initialize: function(){
    this.parents  = {};
    this.children = {};
    this.inSet    = {};
    this.udecSet  = {};
    this.outSet   = {};
    //Filling the sets:
    _.each(this.get('items'), function(i){
      var k = i[0], v = i[1];
      switch(k){
        case 'In':
          this.inSet[v] = true;
        break;
        case 'Udec':
          this.udecSet[v] = true;
        break;
        case 'Out':
        default:
          this.outSet[v] = true;
      }
    }, this);
  }
, parentCount: function(){
    return _.keys(this.parents).length;
  }
, childCount: function(){
    return _.keys(this.children).length;
  }
// Testing if r is a child of this with respect to set inclusion
, isChild: function(r){
    if(r.get('id') === this.get('id'))
      return false;
    var ins  = _.keys(r.inSet)
      , outs = _.keys(r.outSet);
    for(var i = 0; i < ins.length; i++)
      if(!(ins[i] in this.inSet))
        return false;
    for(var i = 0; i < outs.length; i++)
      if(!(outs[i] in this.inSet))
        return false;
    return true;
  }
// Adding r as a child to this
, addChild: function(r){
    this.children[r.get('id')] = r;
    r.parents[this.get('id')]  = this;
  }
, showSet: function(){
    var atoms = _.keys(this.inSet);
    atoms.push.apply(atoms, _.map(_.keys(this.outSet), function(o){return "¬"+o;}));
    var ret = _.reduce(atoms, function(m, x){
      if(m === "") return x;
      return m+","+x;
    }, "", this);
    if(ret === "")
      return "∅";
    return "{"+ret+"}";
  }
});
