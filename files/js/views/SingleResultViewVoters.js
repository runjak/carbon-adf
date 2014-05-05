SingleResultViewVoters = Backbone.View.extend({
  setResult: function(r){
    if(this.model){
      this.model.voters.off(null, null, this);
    }
    this.model = r;
    if(r){
      this.model.voters.on('add remove reset', this.render, this);
    }
  }
, render: function(){
    var target = this.$('tbody').html('')
      , vMap   = this.model.voteMap;
    this.model.voters.each(function(u){
      var uid   = u.get('id')
        , name  = u.get('username')
        , voted = vMap[uid] || false
        , btn   = voted ? 'btn-success'   : 'btn-danger'
        , state = voted ? 'Already voted' : 'Has not yet voted.';
      target.append(
        '<tr><td>'
      + uid
      + '</td><td><a href="#/user/'+uid+'">'
      + name
      + '</a></td><td><div class="btn '
      + btn
      + '">'
      + state
      + '</div></td></tr>'
      );
    });
  }
});
