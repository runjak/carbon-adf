DiscussionDlFileView = Backbone.View.extend({
  initialize: function(){
    var f = this.$('form').get(0);
    var i = this.$('iframe').get(0);
    f.target = i.id;
    this.div  = this.$('#SingleDiscussionViewFilesCurrentDl');
    this.code = this.$('#SingleDiscussionViewFilesCurrentDl pre');
    this.setModel(this.model);
  }
, events: {
    "click #SingleDiscussionViewFilesButton": "upload"
  }
, upload: function(){
    var file = this.$('#SingleDiscussionViewFilesInstance').val();
    if(file !== ''){
      var view = this;
      this.$('iframe').load(function(){
        view.$('iframe').unbind('load');
        if(view.model)
          view.model.fetch();
        view.fetchAcs();
      });
      this.$('form').submit();
    }else alert('You need to select a file first.');
  }
, render: function(acs){
    if(acs){
      this.div.show();
      this.code.html(acs);
    }else{
      this.div.hide();
      this.code.html('');
    }
  }
, fetchAcs: function(){
    if(!this.model) return;
    var id = this.model.get('id');
    var t = this;
    $.get('discussion/'+id+'/acs').done(function(acs){
      t.render(acs[acs.length - 1]);
    });
  }
, setModel: function(m){
    if(this.model){
      this.model.articles.off( null, null, this);
      this.model.relations.off(null, null, this);
    }
    this.model = m;
    if(m){
      this.model.articles.on( "change", this.fetchAcs, this);
      this.model.relations.on("change", this.fetchAcs, this);
      var id = m.get('id');
      this.$('form').attr('action',
        'discussion/'+id+'/fitinstance');
      this.fetchAcs();
    }
    this.render();
  }
});
