SingleResultViewVotes = Backbone.View.extend({
  initialize: function(){
    //Copying setResult from SingleResultViewGraph:
    this.setResult = SingleResultViewGraph.prototype.setResult;
  }
, render: function(){
    var target   = this.$('tbody').html('')
      , maxVotes = this.model.maxResultVotes;
    this.model.results.each(function(r){
      var rid    = r.get('id')
        , rTypes = r.get('resultType').join(', ')
        , set    = r.showSet()
        , votes  = r.get('votes')
        , score  = (maxVotes === 0) ? 0
                 : votes / maxVotes * 100;
      // FIXME WIP here!
      target.append(
          '<tr><td>'
        + rid
        + '</td><td>'
        + set
        + '</td></tr>'
      );
    });
  }
});
