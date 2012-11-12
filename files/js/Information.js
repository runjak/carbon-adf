function initInformation(){
  //Selecting an Information as the Profilepage:
  $('dd#InformationMakeProfile > img').click(function(){
    var q = {
      informationId: $('h1.InformationTitle').attr('data-InformationId')
    };
    q.informationId = q.informationId.match(/.*IId \(Id (.*)\)$/)[1];
    $.post("/action/edit/profile", q, function(reply){
      alert(reply);
    });
  });
  /*
    For Relations:
    Using h2 to toggle visibility of content below it:
  */
  $('div#InformationRelations > div > h2').toggle(
    function(){$(" ~ ul.RelationList", this).hide();}
  , function(){$(" ~ ul.RelationList", this).show();}
  );
  /*
    Collecting Information:
  */
  var iCol = new InformationCollection();
  if(!iCol.working)
    alert('Your browser doesn\'t support localstorage - so some parts of the website won\'t work. :(');
  //Displaying collected Informations via Menu:
  $('li#MenuListCollected > img').click(function(){iCol.display();});
  //Collecting Informations:
  $('dd#InformationBookmark > img').click(function(){
    iCol.addInf($('.InformationTitle').attr('data-InformationId'));
  });
  //Selecting Informations in custom collection:
  var selectionCallback = null;
  $('ul.InformationList > li.selectable').click((function(){
    var oddClick = true;
    return function(){
      if(selectionCallback != null){
        selectionCallback(this);
        selectionCallback = null;
      }else if(oddClick){
        $(this).addClass('selected');
        oddClick = false;
      }else{
        $(this).removeClass('selected');
        oddClick = true;
      }
    };
  })());
  //Selecting relations:
  var selectRelation = function(item, button){
    item = $('.InformationTitle', item);
    button.val(item.attr('data-InformationId'));
    button.text(item.text());
  };
  //Selecting a Source relation:
  var selectSourceButton = $('button#InformationAddRelationSelectSource');
  selectSourceButton.click(function(){
    selectionCallback = function(source){
      selectRelation(source, selectSourceButton);
    };
  });
  //Selecting a Target relation:
  var selectTargetButton = $('button#InformationAddRelationSelectTarget');
  selectTargetButton.click(function(){
    selectionCallback = function(target){
      selectRelation(target, selectTargetButton);
    };
  });
  //Creating a new Relation:
  $('button#InformationAddRelationCreate').click(function(){
    //Fetching source and target Id:
    var sIid = selectSourceButton.val();
    var tIid = selectTargetButton.val();
    if(undefined == sIid || undefined == tIid){
      alert('Select Target- and SourceInformation first.');
      return;
    }
    sIid = sIid.match(iCol.iidRegex)[1];
    tIid = tIid.match(iCol.iidRegex)[1];
    if(sIid == tIid){
      alert('Source and Target must be different Information.');
      return;
    }
    //The query:
    var q = {
      source:  sIid
    , target:  tIid
    , type:    $('#InformationAddRelationRelationType').val()
    , comment: $('#InformationAddRelationComment').val()
    };
    $.post("/action/relation/addRelation", q, function(data){
      alert(data);
    });
  });
  //Removing selected Information:
  $('img#InformationRemove').click(function(e){
    var iid = $(this).parent().attr('data-InformationId');
    iCol.delInf(iid);
    $(this).closest('li.selectable').remove();
    e.preventDefault();
  });
  //Deleting a Relation:
  $('dd.removeRelation').click(function(){
    var t = this;
    var rid = $(this).parent().attr('data-relationid');
    var q = {relationId: rid.match(/^RId \(Id (.*)\)$/)[1]};
    $.post("/action/relation/deleteRelation", q, function(data){
      //Check if deleteRelation worked:
      if(/^FAIL.*/.test(data)){
        alert(data);
        return;
      }
      //Delete elements:
      var li = $(t).closest('.RelationDescription').parent();
      var ul = li.parent();
      if(ul.find('li').length < 2){
        //List would be empty, remove the whole thing.
        ul.parent().remove();
      }else{
        li.remove();
      }
    });
  });
};
