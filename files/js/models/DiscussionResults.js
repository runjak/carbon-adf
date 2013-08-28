DiscussionResults = Backbone.Model.extend({
  initialize: function(){
    this.ResultTypes = ["ConflictFree","TwoValued","Stable","Grounded","Complete","Admissible"];
    var t = this;
    _.each(this.ResultTypes, function(rType){
      t[rType] = new ResultCollection();
    });
  }
, setResults: function(rs){
    var buckets = {};
    _.each(this.ResultTypes, function(rType){
      buckets[rType] = [];
    });
    _.each(rs, function(r){
      buckets[r.type].push(r);
    });
    var results = this;
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
      _.each(t[rtype], f, c);
    });
    return this;
  }
});
