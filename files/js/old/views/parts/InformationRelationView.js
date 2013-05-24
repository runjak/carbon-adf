InformationRelationView = Backbone.View.extend({
  initialize: function(){
    this.relations = {sources: null, targets: null};
    this.$el.accordion();
    this.listenTo(this.model, "change", this.fetchRelations);
  }
, fetchRelations: function(){
    this.$el.html('');
    var t = this;
    this.model.fetchRelations().done(function(r){
      t.relations = r;
      t.readyRelations();
    });
  }
, readyRelations: function(){
    var sources = this.relations.sources;
    var targets = this.relations.targets;
    var queue = [];
    if(sources) sources.each(function(r){
      queue.push(r.fetchTarget());
    });
    if(targets) targets.each(function(r){
      queue.push(r.fetchSource());
    });
    var t = this;
    $.when.apply($, queue).done(function(){ t.render(); });
  }
, render: function(){
    this.$el.accordion("destroy");
    var t = this;
    var rSource = function(r){return t.renderRelation(r, r.getTargetI());};
    var rTarget = function(r){return t.renderRelation(r, r.getSourceI());};
    var data = {}; 
    var sources = this.relations.sources;
    if(sources){
      data.Children = sources.where({type: 'Parent'}).map(rSource);
      data.Victims  = sources.where({type: 'Attack'}).map(rSource);
      data.Protegee = sources.where({type: 'Defense'}).map(rSource);
    }
    var targets = this.relations.targets;
    if(targets){
      data.Parents   = targets.where({type: 'Parent'}).map(rTarget);
      data.Attackers = targets.where({type: 'Attack'}).map(rTarget);
      data.Supporter = targets.where({type: 'Defense'}).map(rTarget);
    }
    $.each(data, function(i, e){
      if(e.length === 0) return;
      e = _.reduce(e, function(m, e){return m+e;}, '');
      t.$el.append('<h3>' + i + '</h3><div><dl class="relationList">' + e + '</dl></div>');
    });
    this.$el.accordion();
    this.$('a').click(function(){
      var target = $(this).attr('href');
      window.App.router.navigate(target, {trigger: true});
    });
  }
, renderRelation: function(r, i){
    var id    = i.get('id')
      , desc  = i.get('description')
      , title = i.get('title');
    var dt = "<dt><a href='#view/"+ id + "' title='" + desc + "'>" + title + "</a></dt>";
    var dd = "<dd>" + r.getComment() + "</dd>";
    return dt+dd;
  }
});
