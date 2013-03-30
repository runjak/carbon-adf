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
, _fetchEnd: function(endName, endStore, callback){
    var i = this.get(endStore);
    if(i) return i;
    i = new Information({id: this.get(endName)});
    i.fetch({success: callback});
    this.set({endStore: i});
    return i;
  }
, fetchSource: function(callback){
    return this._fetchEnd('source', 'sourceInformation', callback);
  }
, fetchTarget: function(callback){
    return this._fetchEnd('target', 'targetInformation', callback);
  }
});
