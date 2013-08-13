Result = Backbone.Model.extend({
  filterArticles: function(label){
    var as  = this.get('articles');
    var ret = [];
    _.each(this.get('articles'), function(a){
      if(a[0] === label)
        ret.push(a[1]);
    });
    return ret;
  }
, getIn:   function(){return filterArticles("In");}
, getUdec: function(){return filterArticles("Udec");}
, getOut:  function(){return filterArticles("Out");}
});
