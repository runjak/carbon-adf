/**
  The Item, reflecting the structure of Carbon.Data.Item
*/
Item = DateObject.extend({
  urlRoot: 'item/'
// This is mainly to have a handy list of what we expect:
, defaults: {
    id: null
  , description: null
  , article: null
  , condition: null
  , relation: null
  , relations: null
  , discussion: null
  , resultSet: null
  , creation: null
  , deletion: null
  , parents: null
  , children: null
  , commitMessage: null
  , commitAuthor: null
  //Local only:
  , canCommit: false // Needs to be set to true, to enable saving the Item via commit.
  }
, initialize: function(){
    this.discussion = {
      'arguments': new ItemCollection()
    , relations: new ItemCollection()
    , participants: new UserCollection()
    };
    this.discussion.arguments.on('reset add remove', this.gatherRelations, this);
    this.on('change', this.attributesToDiscussion, this);
    this.attributesToDiscussion();
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
    if(!silent)
      this.trigger('change:'+attr, this.attributes);
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
/**
  Checks, if this Item is a Discussion,
  and if so, fills the this.discussion representation of that data.
*/
, attributesToDiscussion: function(){
    var d = this.get('discussion');
    if(!d){
      //Clear all data in the rep.:
      this.discussion.arguments.reset();
      this.discussion.participants.reset();
    }else{
      /**
        Updating the arguments:
        - First all arguments are gathered,
        - than 3 sets are build from them,
        - afterwards they are fetched and put in the collection.
        - finally gatherRelations is called.
      */
      if(d.arguments){
        //Gathering update information:
        var a = this.discussion.arguments
          , t = this
          , items = {}
          , fetch = false;
        //Two cases where arguments come from:
        if(d.arguments.Left){
          _.each(d.arguments.Left, function(i){
            items[i.id] = new Item(i);
          });
        }else if(d.arguments.Right){
          _.each(d.arguments.Right, function(i){
            items[i] = new Item({id: i});
          });
          fetch = true;
        }
        //Now we'll have to build three sets:
        var add = {}, keep = {}, remove = {};
        a.each(function(i){
          var id = i.get('id');
          if(items[id]){ // Keep this, and update attributes
            i.set(items[id].attributes);
            keep[id] = i;
          }else{ // Remove this
            remove[id] = i;
          }
        });
        _.each(_.keys(items), function(id){
          if(!keep[id])
            add[id] = items[id];
        });
        //Performing the update:
        var update = function(){
          a.reset(_.flatten([_.values(keep),_.values(add)]));
          t.gatherRelations();
        };
        if(fetch){
          a.fetchAll(_.values(add)).done(function(){update();});
        } else update();
      }
      //Updating participants:
      var p  = this.discussion.participants
        , ps = _.map(d.participants, function(p){
        return new User({id: p});
      });
      p.fetchAll(ps).done(function(){p.set(ps);});
    }
    return this;
  }
, discussionToAttributes: function(o, setOptions){
    //Taking care of options:
    o = (typeof(o) === 'object') ? o : {};
    o.args = (o.args === 'Left') ? o.args : 'Right';
    o.other = (o.args === 'Left') ? 'Right' : 'Left';
    o.msg = o.msg || 'General modification of the discussion.';
    setOptions = (typeof(setOptions) === 'object') ? setOptions : {silent: true};
    //Setting custom fields to attributes:
    var discussion = {
      'arguments': {}
    , participants: this.discussion.participants.map(function(p){return p.get('id');})
    };
    //Setting the Arguments:
    discussion.arguments[o.args] = this.discussion.arguments.map(function(a){
      if(o.args === 'Left')
        return a.attributes;
      return a.get('id');
    });
    var d = $.extend(this.get('discussion'), discussion);
    delete d.arguments[o.other];
    this.set({discussion: discussion, commitMessage: o.msg}, setOptions);
    return this;
  }
, isDiscussionParticipant: function(user){
    return this.discussion.participants.findWhere({id: user.get('id')});
  }
//Saves the given item, and adds it afterwards.
, addArgument: function(item){
    var t = this
      , p = $.Deferred();
    item.mySave().done(function(d){
      item.set(d);
      t.discussion.arguments.add([item]);
      var o = {args: 'Right', msg: 'Added an Argument.'};
      t.discussionToAttributes(o).mySave().done(function(d){
        t.set(d);
        p.resolve(d, t);
      }).fail(function(msg){
        p.reject(msg);
      });
    }).fail(function(msg){
      p.reject(msg);
    });
    return p;
  }
//Removes an item from a discussion.
, removeArgument: function(item){
    var t = this;
    t.discussion.arguments.remove(item);
    var o = {args: 'Right', msg: 'Removed an Argument.'};
    return t.discussionToAttributes(o).mySave().done(function(d){
      console.log('item removed, new item is: ');
      console.log(d);
      t.set(d);
    }).fail(function(msg){
      console.log('Fail in Item:removeArgument(): ' + msg);
    });
  }
/**
  Usually called from attributesToDiscussion.
  This function gathers all relations from a discussions arguments,
  filters out duplicates by id,
  and puts them into the item.discussion.relations collection.
*/
, gatherRelations: function(){
    // Gathering all current relations:
    var rs = {};
    this.discussion.arguments.each(function(a){
      _.each(a.get('relations'), function(r){
        var relation = new Item(r);
        rs[relation.get('id')] = relation;
      });
    });
    //A set approach as in attributesTo Discussion:
    var add = {}, keep = {}, remove = {};
    this.discussion.relations.each(function(r){
      var id = r.get('id');
      if(rs[id]){ // A relation to keep:
        r.set(rs[id].attributes);
        keep[id] = r;
      }else{ // A relation to remove:
        remove[id] = r;
      }
    });
    _.each(_.keys(rs), function(id){
      if(!keep[id])
        add[id] = rs[id];
    });
    //Performing the update:
    this.discussion.relations.reset(_.flatten([_.values(add),_.values(keep)]));
  }
});
