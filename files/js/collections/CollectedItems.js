/**
  A collection of Items where the itemIds are syncronized
  to localStorage, so that they survive different sessions
  without bothering the server.
*/
CollectedItems = ItemCollection.extend({
  initialize: function(){
    var l = this.loadLocal();
    if(l.length){
      this.fetchAndReset(_.map(l, function(i){
        return new Item({id: i});
      }));
    }
    this.bind('reset add remove', this.saveLocal);
  }
, loadLocal: function(){
    var data = localStorage.collectedItems;
    if(typeof(data) === 'undefined')
      data = '[]';
    return $.parseJSON(data);
  }
, saveLocal: function(){
    var data = this.map(function(a){return a.get('id');});
    localStorage.collectedItems = JSON.stringify(data);
  }
});
