DiscussionParticipantView = Backbone.View.extend({
  initialize: function(){
    this.setModel(this.model);
  }
, events: {
    "click #JoinDiscussion":  "joinDiscussion"
  , "click #LeaveDiscussion": "leaveDiscussion"
  }
, render: function(){
    if(!this.model) return;
    //Buttons to change if user is a participant:
    if(window.App.login.get('loggedIn')){
      if(window.App.login.findInDiscussion(this.model)){
        this.$('#LeaveDiscussion').show();
        this.$('#JoinDiscussion').hide();
      }else{
        this.$('#JoinDiscussion').show();
        this.$('#LeaveDiscussion').hide();
      }
    }else this.$('form > label').hide();
    //List of current participants:
    var el = this.$('tbody').empty();
    this.model.participants.map(function(p){
      var id = p.get('id');
      var un = p.get('username');
      el.append('<tr><td><a href="#/user/'+id+'">'+un+'</a></td></tr>');
    });
  }
, setModel: function(d){
    if(this.model){
      this.model.participants.off(null, null, this);
    }
    this.model = d;
    if(d){
      this.model.participants.on('reset add remove', this.render, this);
    }
    this.render();
  }
, joinDiscussion: function(e){
    e.preventDefault();
    this.model.setParticipant(true);
  }
, leaveDiscussion: function(e){
    e.preventDefault();
    this.model.setParticipant(false);
  }
});
