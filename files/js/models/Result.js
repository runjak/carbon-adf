Result = Backbone.Model.extend({
  defaults: {like: false}
, filterArticles: function(label){
    var as  = this.get('articles');
    var ret = [];
    _.each(this.get('articles'), function(a){
      if(a[0] === label)
        ret.push(a[1]);
    });
    return ret;
  }
, getIn:   function(){return this.filterArticles("In");}
, getUdec: function(){return this.filterArticles("Udec");}
, getOut:  function(){return this.filterArticles("Out");}
, stateFor: function(aid){
    var state = _.find(this.get('articles'), function(a){
      return a[1] == aid;
    });
    if(typeof(state) === 'undefined')
      return 'Udec';
    return state[0];
  }
, getSubsets: function(articles){
    var subsets = {
      ins:   this.getIn()
    , udecs: this.getUdec()
    , outs:  this.getOut()
    };
    _.each(subsets, function(subset, key){
      subsets[key] = articles.elems(function(a){
        return _.some(subset, function(id){
          return id === a.get('id');
        });
      });
    });
    return subsets;
  }
});
