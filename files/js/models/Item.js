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
      //Updating arguments:
      if(d.arguments){
        //Building a set of current arguments:
        var a = this.discussion.arguments, current = {};
        a.each(function(x){current[x.get('id')] = x;});
        //The evil thing about arguments is,
        //that they come in two flavors, Left and Right,
        //where the Right case needs more attention.
        var fetch = false, add = {}, keep = {};
        if(d.arguments.Left){
          _.each(d.arguments.Left, function(i){
            if(i.id in current){
              keep[i.id] = current[i.id];
            }else{
              add[i.id] = new Item(i);
            }
          });
        }else if(d.arguments.Right){
          fetch = true;
          _.each(d.arguments.Right, function(i){
            if(i in current){
              keep[i] = current[i];
            }else{
              add[i] = new Item({id: i});
            }
          });
        }
        //Now for finishing our update:
        var t = this, update = function(){
          a.reset(_.flatten([_.values(keep),_.values(add)]));
          t.gatherRelations();
        };
        if(fetch){
          a.fetchAll(_.values(add)).done(update);
        }else update();
      }
      //Updating participants:
      if(d.participants){
        // We want to keep current representations, so we do a 'clever' update.
        var p = this.discussion.participants
          , current = {};
        p.each(function(x){current[x.get('id')] = x;});
        var add = {}, keep = {};
        _.each(d.participants, function(uid){
          if(uid in current){
            keep[uid] = current[uid];
          }else{
            add[uid] = new User({id: uid});
          }
        });
        p.fetchAll(_.values(add)).done(function(){
          p.set(_.flatten([_.values(add),_.values(keep)]));
        });
      }
    }
  }
, discussionToAttributes: function(o, setOptions){
    //FIXME DEBUG
    console.log('Item.discussionToAttributes('
                +JSON.stringify(o)+','
                +JSON.stringify(setOptions)+')');
    //Taking care of options:
    o = (typeof(o) === 'object') ? o : {};
    o.args = (o.args === 'Left') ? o.args : 'Right';
    o.other = (o.args === 'Left') ? 'Right' : 'Left';
    o.msg = o.msg || 'General modification of the discussion.';
    setOptions = (typeof(setOptions) === 'object') ? setOptions : {silent: true};
    //Setting custom fields to attributes:
    var discussion = {
      'arguments': {}
    , participants: this.discussion.participants
                    .map(function(p){return p.get('id');})
                    .filter(function(x){return $.isNumeric(x);})
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
      if(id in rs){ // A relation to keep:
        r.set(rs[id].attributes);
        keep[id] = r;
      }else{ // A relation to remove:
        remove[id] = r;
      }
    });
    _.each(_.keys(rs), function(id){
      if(!(id in keep))
        add[id] = rs[id];
    });
    //Performing the update:
    this.discussion.relations.reset(_.flatten([_.values(add),_.values(keep)]));
  }
});
