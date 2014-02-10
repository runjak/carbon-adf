ItemCollection = ExtendedCollection.extend({
  model: Item
, sortByCreation: function(){
    this.comparator = function(a){
      return a.get('creation');
    };
  }
});
