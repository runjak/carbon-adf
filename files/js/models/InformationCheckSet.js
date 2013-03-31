InformationCheckSet = Backbone.Model.extend({
  defaults: { watchCollection: null
            , checkedSet: {}}
, initialize: function(){
    if(this.get('watchCollection') === null)
      this.set({watchCollection: window.App.collectedInformations});
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
, collection: function(){return this.get('watchCollection');}
});
