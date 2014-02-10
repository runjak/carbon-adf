RelationCollection = ExtendedCollection.extend({
  model: Relation
, relationFromTo: function(source, target){
    var sid = source.get('id');
    var tid = target.get('id');
    return this.elem(function(r){
      var hasSource = r.source.get('id') === sid;
      var hasTarget = r.target.get('id') === tid;
      return (hasSource && hasTarget);
    });
  }
});
