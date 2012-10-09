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
  $('ul.InformationList > li.selectable').toggle(
    function(){$(this).addClass('selected');}
  , function(){$(this).removeClass('selected');}
  );
  //Removing selected Information:
  $('div#InformationRemoveSelected').click(function(){
    $('ul.InformationList > li.selected').each(function(){
      var iid = $('.InformationTitle', this).attr('data-InformationId');
      iCol.delInf(iid);
      $(this).remove();
    });
  });
};
