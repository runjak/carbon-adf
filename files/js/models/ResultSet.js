ResultSet = Discussion.extend({
  //Set inclusion for result types: admissible >= complete >= (grounded, pref >= stable >= two-valued)
  resultTypes: { TwoValued:  4
               , Stable:     3
               , Preferred:  2
               , Grounded:   2
               , Complete:   1
               , Admissible: 0}
, resultSubsets: {} // Maps from most specific ResultType -> [Result]
, levels: [] // [[Result]], sorted top -> bottom
  // Returns the most specific resultType.
, topResultType: function(result){
    var rTypes = result.get('resultType')
      , ret    = 'Admissible';
    _.each(rTypes, function(rType){
      if(!(rType in this.resultTypes))
        return;
      if(this.resultTypes[rType] > this.resultTypes[ret]){
        ret = rType;
      }
    }, this);
    return ret;
  }
, initialize: function(){
    //Calling the super constructor:
    Discussion.prototype.initialize.apply(this, arguments);
    //Own initialization:
    this.results = new ResultCollection(); 
    this.on('change:resultSet', this.updateResults, this);
    this.updateResults();
  }
, updateResults: function(){
    var rSet = this.get('resultSet');
    if(!rSet){
      this.results.reset();
      return;
    }
    var results = _.map(rSet.results, function(r){
      return new Result(r);
    });
    //Filling resultSubsets:
    _.each(_.keys(this.resultTypes), function(r){
      this.resultSubsets[r] = [];
    }, this);
    _.each(results, function(r){
      var rType = this.topResultType(r);
      this.resultSubsets[rType].push(r);
    }, this);
    //Building the levels:
    var rS = this.resultSubsets;
    this.levels = [
        rS['TwoValued']
      , rS['Stable']
      , rS['Preferred']
      , rS['Grounded'].concat(rS['Complete'])
      , rS['Admissible']]
    //Checking parent/child relations for layered resultTypes:
    var pcPairs = _.initial(_.zip(this.levels, _.tail(this.levels)));
    _.each(pcPairs, function(pair){
      var ps = pair[0], cs = pair[1];
      _.each(ps, function(p){
        _.each(cs, function(c){
          if(p.isChild(c))
            p.addChild(c);
        }, this);
      }, this);
    }, this);
    //Saving results to the collection:
    this.results.reset(results);
  }
});
