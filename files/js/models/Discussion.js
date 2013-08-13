Discussion = Item.extend({
  urlRoot: 'discussion/'
, defaults: {}
, initialize: function(){
    this.articles     = new  ArticleCollection();
    this.participants = new     UserCollection();
    this.relations    = new RelationCollection();
    this.results      = new  DiscussionResults();
    this.on('change:articles',     this.updateArticles,     this);
    this.on('change:participants', this.updateParticipants, this);
    this.on('change:relations',    this.updateRelations,    this);
    this.on('change:results',      this.updateResults,      this);
  }
, updateArticles: function(){
    var cid = {collectionId : this.get('collectionId')};
    var as  = _.map(this.get('articles'), function(a){
      a = new Article(a);
      return a.set(cid);
    });
    this.articles.set(as);
    /*
      Sadly I've got to call updateRelations here,
      because the existence of Articles determines the
      outcome of Relation.setDiscussion,
      which is used to filter produced Relations.
    */
    this.updateRelations();
  }
, updateParticipants: function(){
    var ps = _.map(this.get('participants'), function(p){
      return new User({id: p[0], voted: p[1]});
    });
    var t = this;
    this.participants.fetchAll(ps).done(function(){
      t.participants.set(ps);
    });
  }
, updateRelations: function(){
    var discussion = this;
    var rs = this.get('relations');
    rs = _.reduce(rs, function(stack, r){
      var relation = new Relation(r);
      if(relation.setDiscussion(discussion))
        stack.push(relation);
      return stack;
    }, []);
    this.relations.set(rs);
  }
, updateResults: function(){
    this.results.setResults(this.get('results'));
  }
, create: function(){
    var q = {
      headline:    this.get('headline')
    , description: this.get('description')
    };
    if(d = this.get('deadline'))
      q.deadline = d;
    var created = $.Deferred();
    var discussion = this;
    var cid = {collectionId: discussion.collectionId};
    $.post(this.urlRoot, q).done(function(d){
      //Add articles to the discussion:
      discussion.articles.map(function(a){
        a.set(cid);
        $.put(discussion.getCAUrl(a));
      });
      //Set discussion data and resolve:
      created.resolve(discussion.set(d));
    }).fail(function(f){
      created.reject(f);
    });
    return created;
  }
, addArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.put(this.getCAUrl(a)).done(function(){
      a.set({collectionId: discussion.get('collectionId')});
      discussion.articles.add(a);
    });
  }
, removeArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.delete(this.getCAUrl(a)).done(function(){
      discussion.articles.remove(a);
      var aid = a.get('id');
      var rs  = discussion.relations.elems(function(r){
        var hasSource = r.source.get('id') === aid;
        var hasTarget = r.target.get('id') === aid;
        return (hasSource || hasTarget);
      });
      discussion.relations.remove(rs);
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
    var ps  = this.participants;
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
