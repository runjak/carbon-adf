function initInformation(){
  //Using h2 to toggle visibility of content below it:
  $('div#InformationRelations > div > h2').toggle(
    function(){$(" ~ ul.RelationList", this).hide();}
  , function(){$(" ~ ul.RelationList", this).show();}
  );
};
