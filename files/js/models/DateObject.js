/**
  This object aims to supplement different methods
  to handle dates.
*/
DateObject = Backbone.Model.extend({
  dayFromTime: function(t){
    t = /([^ ]+) /.exec(t);
    if(t.length === 2)
      return t[1]; 
    return '';
  }
, stripFractionFromTime: function(t){
    t = /([^\.]+)\./.exec(t);
    if(t !== null && t.length === 2)
      return t[1];
    return '';
  }
});
