/**
  The Item, reflecting the structure of Carbon.Data.Item
*/
Item = ResultSet.extend({
  urlRoot: 'item/'
// This is mainly to have a handy list of what we expect:
, defaults: {
    id:            null
  , description:   null
  , article:       null
  , condition:     null
  , relation:      null
  , relations:     null
  , discussion:    null
  , resultSet:     null
  , creation:      null
  , deletion:      null
  , parents:       null
  , children:      null
  , commitMessage: null
  , commitAuthor:  null
  //Local only:
  , canCommit: false // Needs to be set to true, to enable saving the Item via commit.
  }
, initialize: function(){
    //Label for Result:applyTo:
    this.resultLabel = null;
    //Calling the super constructor:
    ResultSet.prototype.initialize.apply(this, arguments);
  }
, setDescription: function(headline, summary){
    return this.modify('description', {'headline': headline, 'summary': summary});
  }
, setArticle: function(content){
    return this.modify('article', {'content': content});
  }
, setProofStandard: function(ps){
    return this.modify('condition', {'proofStandard': ps, formula: null});
  }
, setFormula: function(f){
    return this.modify('condition', {'proofStandard': null, 'formula': f});
  }
, setRelation: function(source, target, rType){
    return this.modify('relation', {'source': source, 'target': target, 'relationType': rType});
  }
/*
  @param attr String
  @param obj Object
  @param [silent = false] Bool
  Inspired from the Haskell State Monad,
  this function gets an attribute,
  extends it with a given object,
  and sets it again.
*/
, modify: function(attr, obj, silent){
    silent = silent || false;
    this.set(attr, $.extend(this.get(attr), obj));
    if(silent === false){
      this.trigger('change', this.attributes);
    }
    return this;
  }
, isProofStandardCustom: function(){
    if(c = this.get('condition'))
      return (c.proofStandard === null);
    return false;
  }
/**
  The original Backbone.js save method has problems with attribute values being objects themself.
  To fix this, I've written my own save method.
*/
, mySave: function(){
    //Saving the item:
    var req = {}, url = this.urlRoot;
    //Making sure commitAuthor is set:
    if(this.get('commitAuthor') === null){
      if(window.App.login.get('loggedIn')){
        this.set({commitAuthor: window.App.login.get('id')}, {silent: true});
      }
    }
    //Serialize to req:
    _.each(this.attributes, function(v, k){
      if(k === 'canCommit') return;
      if(typeof(v) === 'object')
        v = JSON.stringify(v);
      req[k] = v;
    });
    this.set({canCommit: false}, {silent: true});
    if(this.isNew())
      return $.post(url, req);
    return $.put(url + this.get("id"), req);
  }
});
