Relation = Backbone.Model.extend({
  urlRoot: '/relation/'
, defaults: {}
, initialize: function(){}
//Expected getters:
, getSource:  function(){return this.get('source');}
, getTarget:  function(){return this.get('target');}
, getType:    function(){return this.get('type');}
, getComment: function(){return this.get('comment');}
, getCreated: function(){return this.get('creation');}
, getDeleted: function(){return this.get('deletion');}
//Predicates:
, isParent:     function(){return this.getType() === 'Parent';}
, isCollection: function(){return this.getType() === 'Collection';}
, isAttack:     function(){return this.getType() === 'Attack';}
, isDefense:    function(){return this.getType() === 'Defense';}
//Changes:
, create: function(options, callback){
    var defaults = { source:  null
                   , target:  null
                   , type:    null
                   , comment: null };
    options = $.extend(options, defaults);
    $.post('/action/relation/addRelation', options, function(data){
      if(callback) callback(data);
    });
  }
, delete: function(callback, refetch){
    var t = this;
    var q = {relationId: this.get('id')};
    $.post('/action/relation/deleteRelation', q, function(data){
      if(callback) callback(data);
      if(refetch)  t.fetch();
    });
  }
, setComment: function(comment, callback){
    var t = this;
    var q = {relationId: this.get('id')
            , comment: comment };
    $.post('/action/relation/updateComment', q, function(data){
      t.set({comment: comment});
      if(callback) callback(data);
    });
  }
});
