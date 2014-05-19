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
, maxResultVotes: 0
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
    this.voteMap = {}; // UserId -> Voted(Bool)
    this.voters  = new UserCollection();
    this.on('change:resultSet', this.updateVoters, this);
    this.updateVoters();
  }
, updateResults: function(){
    var rSet = this.get('resultSet');
    if(!rSet){
      this.results.reset();
      return;
    }
    this.maxResultVotes = 0;
    var results = _.map(rSet.results, function(r){
      if(r.votes > this.maxResultVotes){
        this.maxResultVotes = r.votes;
      }
      return new Result(r);
    }, this);
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
, updateVoters: function(){
    this.voteMap = {};
    var rSet  = this.get('resultSet'), t = this;
    if(!rSet){
      this.voters.reset();
      return;
    }
    var users = _.map(rSet.voters, function(xs){
      var uid = xs[0], vtd = xs[1];
      this.voteMap[uid] = vtd;
      return new User({id: uid});
    }, this);
    this.voters.fetchAll(users).done(function(){
      t.voters.set(users);
    }).fail(function(){
      console.log("Failed to fetch users in ResultSet:updateVoters.");
      console.log("… ResultSet was: " + JSON.stringify(rSet));
    });
  }
});
