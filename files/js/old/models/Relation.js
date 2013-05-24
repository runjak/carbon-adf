Relation = Backbone.Model.extend({
  urlRoot: '/relation/'
, defaults: {}
, initialize: function(){}
//Expected getters:
, getSource:  function(){return this.get('source');}
, getSourceI: function(){return this.get('sourceI');}
, getTarget:  function(){return this.get('target');}
, getTargetI: function(){return this.get('targetI');}
, getType:    function(){return this.get('type');}
, getComment: function(){return this.get('comment');}
, getCreated: function(){return this.get('creation');}
, getDeleted: function(){return this.get('deletion');}
//Predicates:
, isParent:     function(){return this.getType() === 'Parent';}
, isCollection: function(){return this.getType() === 'Collection';}
, isAttack:     function(){return this.getType() === 'Attack';}
, isDefense:    function(){return this.getType() === 'Defense';}
, isTarget: function(i){return i.get('id') === this.get('target');}
, isSource: function(i){return i.get('id') === this.get('source');}
//Changes:
, create: function(attributes, options){
    var defaults = { source:  null
                   , target:  null
                   , type:    null
                   , comment: null };
    attributes = $.extend(attributes, defaults);
    var defaults = {wait: true, success: null, error: null};
    options = $.extend(options, defaults);
    this.save(attributes, defaults);
  }
, setComment: function(comment, options){
    var attributes = {comment: comment};
    var defaults = {wait: true, success: null, error: null};
    options = $.extend(options, defaults);
    this.save(attributes, options);
  }
//Fetching the ends:
, _fetchEnd: function(end){
    return (new Information({id: this.get(end)})).fetch();
  }
, fetchSource: function(callback){
    var t = this;
    var d = $.Deferred();
    this._fetchEnd('source').done(function(i){
      i = new Information(i);
      t.set({sourceI: i});
      d.resolve(i);
    });
    return d;
  }
, fetchTarget: function(callback){
    var t = this;
    var d = $.Deferred();
    this._fetchEnd('target').done(function(i){
      i = new Information(i);
      t.set({targetI: i});
      d.resolve(i);
    });
    return d;
  }
});
