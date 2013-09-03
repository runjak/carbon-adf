/**
  model is expected to be a Discussion.
*/
DiscussionResultView = Backbone.View.extend({
  initialize: function(){
    this.results = {};
    this.tbody   = this.$('tbody');
    if(this.model)
      this.setModel(this.model);
  }
, events: {'click #SingleDiscussionViewResultsVote': 'vote'}
, render: function(){
    console.log('DiscussionResultView.render();');
    if(this.model && this.model.results.hasResults()){
      var canVote = false;
      if(u = window.App.login.findInDiscussion(this.model)){
        canVote = !u.get('voted');}
      var tRows = '';
      this.model.results.each(function(result){
        //The content to build:
        var row = {
          id:   result.get('id')
        , type: result.get('type')
        , articles: ''
        , votes:    ''
        , choice:   ''
        };
        //Filling the articles
        var subsets = result.getSubsets(this.model.articles);
        var lis = '';
        _.each(subsets, function(set, key){
          var label = 'In'
            , state = 'btn-success';
          if(key === 'udecs'){
            label = 'Udec';
            state = 'btn-warning';
          }else if(key === 'outs'){
            label = 'Out';
            state = 'btn-danger';
          }
          _.each(set, function(article){
            var head = article.get('headline')
              , desc = article.get('description')
              , id   = article.get('id');
            lis += '<li><a class="btn btn-mini ' + state+'" '
                  + 'href="#/article/' + id + '" '
                  + 'title="' + desc + '">'
                  + label + ': ' + head
                  + '</a></li>';
          });
        });
        row.articles = '<ul class="articleSet">' + lis + '</ul>';
        //Filling the votes progress:
        var votes = result.get('votes')
        var ratio = votes / this.model.results.MaxVotes * 100
          , state = 'progress-success'
          , title = ' title="' + ratio + '% (' + votes + ')"';
        if(ratio <=  0) ratio = 1;
        if(ratio <= 75) state =    'progress-info';
        if(ratio <= 50) state = 'progress-warning';
        if(ratio <= 25) state =  'progress-danger';
        row.votes = '<div class="votebar progress ' + state + '"' + title + '>'
                  + '<div class="bar" style="width: ' + ratio + '%"></div>'
                  + '</div>';
        //Setting the choice:
        var disabled = canVote ? '' : ' disabled="disabled"'
          , checked  = result.get('like') ? ' checked="checked"' : ''
          , data     = ' data-rid="' + result.get('id') + '"';
        row.choice = '<label class="checkbox">'
                   + '<input type="checkbox"' + data + disabled + checked + '>'
                   + 'vote up</label>';
        //Adding the row:
        tRows += '<tr>'
               + '<td>' + row.id       + '</td>'
               + '<td>' + row.type     + '</td>'
               + '<td>' + row.articles + '</td>'
               + '<td>' + row.votes    + '</td>'
               + '<td>' + row.choice   + '</td>'
               + '</tr>';
      }, this);
      this.tbody.html(tRows);
      if(canVote) this.$('#SingleDiscussionViewResultsVoteArea').show();
      else        this.$('#SingleDiscussionViewResultsVoteArea').hide();
      //Eventlisteners for Results:
      _.each(this.results, function(r){
        r.off(null, null, this);
      }, this);
      this.results = {};
      this.model.results.each(function(r){
        r.on('change:like', this.changeLike, this);
        this.results[r.get('id')] = r;
      },this);
      var results = this.results;
      this.$('tbody input[type="checkbox"]').change(function(){
        var rid = $(this).data('rid')
          , l   = $(this).is(':checked');
        results[rid].set({like: l});
      });
    }else{
      this.tbody.html('<tr><td colspan="5">Sorry, there are currently no results to display.</td></tr>');
      this.$('#SingleDiscussionViewResultsVoteArea').hide();
    }
  }
, setModel: function(m){
    if(this.model){
      this.model.results.off(null, null, this);
      this.model.participants.off(null, null, this);
    }
    this.model = m;
    if(this.model){
      this.model.results.on('change', this.render, this);
      this.model.participants.on('reset add remove', this.render, this);
    }
    this.render();
  }
, vote: function(){
    var rids = [], view = this;
    this.$('tbody input[type="checkbox"]:checked').each(function(){
      rids.push($(this).data('rid'));
    });
    this.model.vote(rids).done(function(){
      alert('The vote has been registered! :)');
    }).fail(function(error){
      alert('There was a problem casting the vote, the server replied:\n' + JSON.stringify(error));
    }).always(function(){
      view.render();
    });
  }
, changeLike: function(result){
    var rid  = result.get('id')
      , like = result.get('like')
      , sel  = 'tbody input[data-rid="'+rid+'"]';
    this.$(sel).prop('checked', like);
  }
});
