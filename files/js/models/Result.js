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
    //Guards at the entrance:
    var tId = this.get('id')
      , pId = p.get('id');
    if(pId === tId)         return false;
    if(pId in this.parents) return true;
    if(tId in p.parents)    return false;
    //The real calculations:
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
// Adding c as a child to this
, addChild: function(c){
    this.children[c.get('id')] = c;
    c.parents[this.get('id')]  = this;
  }
/*
  @param c Result
  @param [blackList] ResultId -> Bool
  Injecting c as a child at the deepest possible places.
  The blackList parameter will be used to prevent pushing in cycles.
*/
, pushChild: function(c, blackList){
    //Guards at the entrance:
    var cId = c.get('id')
      , tId = this.get('id');
    if(cId === tId)          return;
    if(cId in this.children) return;
    if(tId in c.children)    return;
    //Sanitizing the blackList:
    blackList = blackList || {};
    blackList[tId] = true;
    //The pushing:
    var subChild = false;
    _.each(this.children, function(x){
      if(x.get('id') in blackList) return;
      if(x.isParent(c)){
        x.pushChild(c, blackList);
        subChild = true;
      }
    }, this);
    //No deeper root, so direct child:
    if(!subChild){
      _.each(this.children, function(x){
        if(x.isChild(c)){
          this.removeChild(x);
          c.pushChild(x, blackList);
        }
      }, this);
      this.addChild(c);
    }
  }
// Removing c as a child on this
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
/* Mirror functions for convenience below */
// Testing if this is a parent of c
, isParent: function(c){
    return c.isChild(this);
  }
// Adding p as a parent to this
, addParent: function(p){
    p.addChild(this);
  }
// Injecting this as a child to p at deepest places:
, pushParent: function(p){
    p.pushChild(this);
  }
// Removing p as a parent from this
, removeParent: function(p){
    p.removeChild(this);
  }
});
