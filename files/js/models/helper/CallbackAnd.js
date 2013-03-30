/**
  The logical and on callback basis.
  the callback that it takes as a parameter
  will get the parameters of all calls to CallbackAnd.call
  in an array.
  When call has been called count times,
  the count will will be reset to origCount.
*/
CallbackAnd = Backbone.Model.extend({
  defaults: { count:     0
            , origCount: 0
            , data:      []}
, initialize: function(){
    var oc = this.get('origCount');
    if(!oc) this.set({origCount: this.get('count')});
  }
, call: function(d){
    var c = this.get('count') - 1;
    var x = {count: c, data: this.get('data')};
    x.data.push(d);
    this.set(x);
    if(c === 0) this.done();
  }
, getCall: function(){
    var t = this;
    return function(d){t.call(d);};
  }
, done: function(){
    var f = this.get('callback');
    if(f) f(this.get('data'));
    this.set({count: this.get('origCount'), data: []});
  }
});
