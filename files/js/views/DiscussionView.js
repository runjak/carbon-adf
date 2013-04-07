DiscussionView = Backbone.View.extend({
  initialize: function(){
    this.$('#DiscussionType').buttonset();
    this.checkset = new InformationCheckSet({
      watchCollection: window.App.collectedInformations
    });
    this.listenTo(this.checkset, 'change', this.render);
  }
, render: function(){
    console.log('DiscussionView:render()');
    var table = this.$('#DiscussionArgumentTable').html('');
    //Generating content:
    var set = this.checkset;
    set.collection().each(function(i){
      var checked = set.isChecked(i) ? ' checked="checked"' : '';
      table.append('<tr data-iid="' + i.get('id') + '">'
                  +'<td><input type="checkbox" ' + checked + '></td>'
                  +'<td><a href="#view/' + i.get('id') + '">' + i.get('title') + '</a></td>'
                  +'<td>' + i.get('description') + '</td>'
                  +'<td><button title="Remove from collection" class="uncollect">'
                  +'<span class="ui-icon ui-icon-close"></span></button></td>'
                  +'</tr>');
    });
    //Binding events:
    table.find('a').click(function(){
      window.App.router.navigate($(this).attr('href'), {trigger: true});
    });
    set.collection().each(function(i){
      var tr = table.find('tr[data-iid="' + i.get('id') + '"]');
      tr.find('input[type="checkbox"]').click(function(){ set.toggle(i); });
      tr.find('.uncollect').click(function(){ set.collection().remove(i); });
    });
  }
});
