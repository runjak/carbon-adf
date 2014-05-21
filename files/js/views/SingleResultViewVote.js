SingleResultViewVote = Backbone.View.extend({
  events: {
    "click #SingleResultViewVoteSubmit": "vote"
  }
, render: function(){
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
        , votes  = r.get('votes') || 0
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
          '<tr>'
        + '<td>'+set+'</td>'
        + '<td>'+rTypes+'</td>'
        + '<td><div style="min-width: 150px;" class="progress '+color+'">'
        + '<div class="bar" style="width: '+score+'%;">Votes: '+votes+'</div>'
        + '</div></td>'
        + '<td>'+chkBox(rid)+'</td>'
        + '</tr>'
      );
    });
    //Show/Hide of submit button:
    if(!voted){
      this.$('tfoot').removeClass('hide');
    }else{
      this.$('tfoot').addClass('hide');
    }
  }
/*
  The vote method prepares the model to be saved, and performs the change.
  This method needs not to check if the user may vote,
  because the server needs to check anyway,
  and this method will only be called if the GUI thinks the user may vote.
  This method has the following tasks:
  1.: Update the resultSet model according to the vote decision
  2.: Update the model on the server based on our representation
  3.: Update the local model and trigger rendering
*/
, vote: function(){
    //Elements to work with:
    var boxes  = this.$('tbody input[type="checkbox"]')
      , button = this.$('');
    //Disabling button and boxes :
    button.attr('disabled', 'disabled')
          .removeClass('btn-primary')
          .addClass('btn-danger');
    boxes.attr('disabled', 'disabled');
    //Finding rIds of checked votes:
    var checked = {}; // rId -> true
    boxes.each(function(){
      if($(this).is(':checked')){
        var rId = $(this).data('rid');
        checked[rId] = true;
      }
    });
    //Incrementing votes:
    this.model.results.each(function(r){
      if(r.get('id') in checked){
        var v = r.get('votes') + 1;
        r.set({votes: v})
      }
    });
    //Marking current user as voted:
    var uId = window.App.login.get('id');
    this.model.voteMap[uId] = true;
    //Updating button to show progress:
    button.removeClass('btn-danger').addClass('btn-warning');
    //Saving:
    var view = this;
    this.model.saveResultSet().done(function(d){
      console.log('saveResultSet.done with data:');
      console.log(d);
    }).fail(function(f){
      console.log('Problem occured in SingleResultViewVote.vote:\n' + f);
      view.render();
    });
  }
});
//Copying setResult from SingleResultViewGraph:
SingleResultViewVote.prototype.setResult = SingleResultViewGraph.prototype.setResult;
