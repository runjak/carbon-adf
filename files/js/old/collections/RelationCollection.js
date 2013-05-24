RelationCollection = Backbone.Collection.extend({
  model: Relation
, filterParent:     function(){return this.where({type: 'Parent'});}
, filterCollection: function(){return this.where({type: 'Collection'});}
, filterAttack:     function(){return this.where({type: 'Attack'});}
, filterDefense:    function(){return this.where({type: 'Defense'});}
});
