Relation = Item.extend({
  urlRoot: 'relation/'
, defaults: {}
, initialize: function(){}
, setDiscussion: function(d){
    this.discussion = d;
    var sid = this.get('source');
    var tid = this.get('target');
    this.source = null;
    this.target = null;
    var relation = this;
    d.get('articles').each(function(a){
      var aid = a.get('id');
      if(aid === sid) relation.source = a;
      if(aid === tid) relation.target = a;
    });
    return (this.source !== null && this.target !== null);
  }
, create: function(discussion, source, target, headline, description){
    this.discussion = discussion;
    this.source = source;
    this.target = target;
    var q = {
      'headline':    headline
    , 'description': description
    , 'discussion':  discussion.get('id')
    , 'source':      source.get('id')
    , 'target':      target.get('id')
    };
    var relation = this;
    return $.post(this.urlRoot, q).done(function(d){
      relation.set(d).setDiscussion(discussion);
      discussion.get('relations').add(relation);
    });
  }
});
