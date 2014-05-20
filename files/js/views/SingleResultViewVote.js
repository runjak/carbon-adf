SingleResultViewVote = Backbone.View.extend({
  render: function(){
    var target   = this.$('tbody').html('')
      , maxVotes = this.model.maxResultVotes
      , voted    = (function(voteMap){
        var login = window.App.login;
        if(!login.get('loggedIn'))
          return true; // Cannot vote.
        return voteMap[login.get('id')] || false;
      })(this.model.voteMap);
    var chkBox = voted
      ? function(){return 'You\'re not allowed to vote. Sorry.';}
      : function(x){
        return '<label class="checkbox">'
             + '<input type="checkbox" data-rid="'+x+'">'
             + '</label>';
      };
    var idNameMap = this.model.getResultIdNameLookup();
    this.model.results.each(function(r){
      //Definitions for each row:
      var rid    = r.get('id')
        , rTypes = r.get('resultType').join(', ')
        , set    = r.showSet(idNameMap)
        , votes  = r.get('votes')
        , score  = (maxVotes === 0) ? 0
                 : votes / maxVotes * 100
        , color  = (function(){
          if(votes === maxVotes)
            return 'progress-success';
          if(score >= 2 / 3 * 100)
            return 'progress-info';
          if(score >= 1 / 3 * 100)
            return 'progress-warning';
          return 'progress-danger';
        })();
      //Building and appending the row:
      target.append(
          '<tr><td>'
        + set
        + '</td><td>'
        + rTypes
        + '</td><td>'
        + votes
        + '<div style="min-width: 150px;" class="progress '
        + color
        + '"><div class="bar" style="width: '
        + score
        + '%;"></div></div>'
        + '</td><td>'
        + chkBox(rid)
        + '</td></tr>'
      );
    });
    //FIXME add listeners for checkboxes!
  }
});
//Copying setResult from SingleResultViewGraph:
SingleResultViewVote.prototype.setResult = SingleResultViewGraph.prototype.setResult;
