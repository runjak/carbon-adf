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
// Testing if this is a child of p
, isChild: function(p){
    if(p.get('id') === this.get('id'))
      return false;
    var ins  = _.keys(this.inSet)
      , outs = _.keys(this.outSet);
    for(var i = 0; i < ins.length; i++)
      if(!(ins[i] in p.inSet))
        return false;
    for(var i = 0; i < outs.length; i++)
      if(!(outs[i] in p.outSet))
        return false;
    return true;
  }
// Testing if this is a parent of c
, isParent: function(c){
    return c.isChild(this);
  }
// Adding c as a child to this
, addChild: function(c){
    this.children[c.get('id')] = c;
    c.parents[this.get('id')]  = this;
  }
// Injecting c as child at deepest places:
, pushChild: function(c){
    var subChild = false;
    _.each(this.children, function(x){
      if(x.isParent(c)){
        x.pushChild(c);
        subChild = true;
      }
    }, this);
    if(!subChild){
      _.each(this.children, function(x){
        if(x.isChild(c)){
          this.removeChild(x);
          c.pushChild(x);
        }
      }, this);
      this.addChild(c);
    }
  }
// Removing r as a child on this
, removeChild: function(c){
    delete this.children[c.get('id')];
    delete c.parents[this.get('id')];
  }
/*
  @param ridLookup :: ResultId -> String
  showSet can take a function that converts resultId to a string,
  in case the resulting string shall contain something
  more entertaining than plain resultIds.
*/
, showSet: function(ridLookup){
    var lookup  = ridLookup || _.identity
      , _lookup = function(x){return "¬"+lookup(x)}
      , atoms   = _.map(_.keys(this.inSet), lookup);
    atoms.push.apply(atoms, _.map(_.keys(this.outSet), _lookup));
    var ret = atoms.join(',');
    if(ret === "")
      return "∅";
    return "{"+ret+"}";
  }
, showSetTree: function(lookup){
    var set = this.get('id')+'@'+this.showSet(lookup);
    var cSets = _.map(_.values(this.children), function(c){
      return c.showSetTree(lookup);
    }, this);
    if(cSets.length === 0)
      return set;
    return '('+set+'|'+cSets.join(', ')+')';
  }
, showChildren: function(){
    var p = this.get('id');
    var cs = _.keys(this.children).join(', ')
    console.log(p+' -> '+cs);
  }
});
