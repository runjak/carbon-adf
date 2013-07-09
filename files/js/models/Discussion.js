Discussion = Item.extend({
  urlRoot: 'discussion/'
, defaults: {
    articles:   new ArticleCollection()
  , participants:  new UserCollection()
  , relations: new RelationCollection()
  }
, initialize: function(){}
, create: function(){
    var q = {
      headline: this.get('headline')
    , description: this.get('description')
    };
    if(d = this.get('deadline'))
      q.deadline = d;
    var created = $.Deferred();
    var discussion = this;
    $.post(this.urlRoot, q).done(function(d){
      //Add articles to the discussion:
      var base = 'collection/' + d.collectionId + '/';
      discussion.get('articles').map(function(a){
        $.put(base + a.get('id'));
      });
      //Set discussion data and resolve:
      created.resolve(discussion.set(d));
    }).fail(function(f){
      created.reject(f);
    });
    return created;
  }
/* Custom load method because it's easier than to overwrite fetch.
   But fetch doesn't respect articles being a Collection and makes them an array.*/
, load: function(){
    var discussion = this;
    var ret = $.Deferred();
    this.fetch().done(function(d){
      //Handling Articles:
      var as = discussion.get('articles');
      var ac = new ArticleCollection(_.map(as, function(a){
        a = new Article(a);
        return a.set({collectionId: discussion.get('collectionId')});
      }));
      discussion.set({articles: ac});
      //Handling Relations:
      var rs = discussion.get('relations');
      var rc = new RelationCollection(_.reduce(rs, function(stack, r){
        var relation = new Relation(r);
        if(relation.setDiscussion(discussion))
          stack.push(relation);
        return stack;
      }, []));
      discussion.set({relations: rc});
      //Handling Participants:
      var us = discussion.get('participants');
      var uc = new UserCollection();
      uc.fetchAndReset(_.map(us, function(u){
        return new User({id: u});
      })).always(function(){
        discussion.set({participants: uc});
        ret.resolve(d);
      });
    }).fail(function(f){
      ret.reject(f);
    });
    return ret;
  }
, addArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.put(this.getCAUrl(a)).done(function(){
      a.set({collectionId: discussion.get('collectionId')});
      discussion.get('articles').add(a);
    });
  }
, removeArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.delete(this.getCAUrl(a)).done(function(){
      discussion.get('articles').remove(a);
      var aid = a.get('id');
      var rs = discussion.get('relations').elems(function(r){
        var hasSource = r.source.get('id') === aid;
        var hasTarget = r.target.get('id') === aid;
        return (hasSource || hasTarget);
      });
      discussion.get('relations').remove(rs);
    });
  }
/* Produces the url necessary to add/remove an article to/from a collection. */
, getCAUrl: function(a){
    var cid = this.get('collectionId');
    var aid = a.get('id');
    return 'collection/'+cid+'/'+aid;
  }
, setParticipant: function(isP){
    if(!window.App.login.get('loggedIn')) return;
    var url = this.urlRoot + this.get('id') + '/participate/' + window.App.login.get('id');
    var ps  = this.get('participants');
    var def = isP ? $.post(url) : $.delete(url);
    def.done(function(){
      ps.toggleElem(window.App.login);
    });
  }
, setNewRelationStart: function(source){
    this.set({'newRelationStart': source});
  }
, setNewRelationEnd: function(target){
    if(!this.get('newRelationStart')) return false;
    this.set({'newRelationEnd': target});
    return true;
  }
});
