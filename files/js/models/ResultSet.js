ResultSet = Discussion.extend({
  initialize: function(){
    //Calling the super constructor:
    Discussion.prototype.initialize.apply(this, arguments);
    this.results = new ResultCollection(); 
    this.on('change:resultSet', this.updateResults, this);
    this.updateResults();
  }
, updateResults: function(){
    var rSet = this.get('resultSet');
    if(!rSet) return;
    var results = _.map(rSet.results, function(r){
      return new Result(r);
    });
    //Finding parents for results:
    _.each(results, function(x){
      _.each(results, function(y){
        if(y.isChild(x))
          y.addChild(x);
      }, this);
    }, this);
    this.results.reset(results);
  }
});
