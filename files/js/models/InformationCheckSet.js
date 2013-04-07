InformationCheckSet = Backbone.Model.extend({
  defaults: { watchCollection: null //Must be set
            , checkedSet: {}}
, initialize: function(){
    var t = this;
    this.get('watchCollection').on("change reset add remove", function(){t.trigger("change");});
  }
, check: function(i){
    var set = this.get('checkedSet');
    set[i.get('id')] = true;
    this.set({checkedSet: set});
  }
, uncheck: function(i){
    var set = this.get('checkedSet');
    set[i.get('id')] = false;
    this.set({checkedSet: set});
  }
, setChecked: function(i, c){
    if(c) this.check(i);
    else  this.uncheck(i);
  }
, isChecked: function(i){
    var set = this.get('checkedSet');
    return (set[i.get('id')] === true);
  }
, toggle: function(i){
    this.setChecked(i, !this.isChecked(i));
  }
, collection: function(){return this.get('watchCollection');}
});
