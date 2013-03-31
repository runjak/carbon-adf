InformationRelationView = Backbone.View.extend({
  initialize: function(){
    this.relations = {sources: null, targets: null};
    this.$el.accordion();
    this.listenTo(this.model, "change", this.fetchRelations);
  }
, fetchRelations: function(){
    var t = this;
    this.model.fetchRelations(function(relations){
      t.relations = relations;
      t.readyRelations();
    });
  }
, readyRelations: function(){
    var t    = this;
    var cAnd = new CallbackAnd({callback: function(){t.render();}});
    var sources = this.relations.sources;
    var targets = this.relations.targets;
    if(sources) sources.each(function(r){
      cAnd.bump();
      r.fetchTarget(cAnd.getCall());
    });
    if(targets) targets.each(function(r){
      cAnd.bump();
      r.fetchSource(cAnd.getCall());
    });
  }
, render: function(){
    //Clearing the old data:
    this.$el.accordion("destroy").html('');
    //Filling in new data:
    var t = this;
    var rSource = function(r){return t.renderRelation(r, r.fetchTarget());};
    var rTarget = function(r){return t.renderRelation(r, r.fetchSource());};
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
      t.$el.append('<h3>' + i + '</h3><div><dl class="relationList">' + e + '</dl></div>');
    });
    this.$el.accordion();
  }
, renderRelation: function(r, i){
//getSource, getTarget, getType, getComment, getCreated, getDeleted
    var id    = i.get('id')
      , desc  = i.get('description')
      , title = i.get('title');
    var dt = "<dt><a href='#view/"+ id + "' title='" + desc + "'>" + title + "</a></dt>";
    var dd = "<dd>" + r.getComment() + "</dd>";
    return dt+dd;
  }
});
