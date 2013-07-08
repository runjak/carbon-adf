Relation = Item.extend({
  urlRoot: 'relation'
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
      if(aid === rid) relation.target = a;
    });
  }
});
