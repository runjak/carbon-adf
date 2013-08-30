DiscussionResults = Backbone.Model.extend({
  initialize: function(){
    this.ResultTypes = ["ConflictFree","TwoValued","Stable","Grounded","Complete","Admissible"];
    this.MaxVotes    = 1;
    var t = this;
    _.each(this.ResultTypes, function(rType){
      t[rType] = new ResultCollection();
    });
  }
, setResults: function(rs){
    var buckets  = {}
      , MaxVotes = 1
      , results  = this;
    _.each(this.ResultTypes, function(rType){
      buckets[rType] = [];
    });
    _.each(rs, function(r){
      buckets[r.type].push(r);
      if(r.votes > MaxVotes)
        MaxVotes = r.votes;
    });
    this.MaxVotes = MaxVotes;
    _.each(this.ResultTypes, function(rType){
      results[rType].set(buckets[rType]);
    });
    return this;
  }
, hasResults: function(){
    var results = this;
    return _.some(this.ResultTypes, function(rType){
      return results[rType].length > 0;
    });
  }
, each: function(f, c){
    var t = this;
    _.each(this.ResultTypes, function(rType){
      t[rType].each(f, c);
    });
    return this;
  }
});
