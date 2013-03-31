CollectedInformations = InformationCollection.extend({
  initialize: function(){
    var d = this.getData();
    if(d.length){
      var elems = _.map(d, function(id){
        var i = new Information({id: id});
        i.fetch();
        return i;
      });
      this.reset(elems);
    }
    this.bind('reset add remove', this.saveData);
  }
, getData: function(){return $.parseJSON(localStorage.collectedInformations);}
, saveData: function(){
    var data = this.map(function(i){return i.get('id');});
    localStorage.collectedInformations = JSON.stringify(data);
  }
});
