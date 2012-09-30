function initMarked(){
  //Setting marked options:
  marked.setOptions({
    gfm:      true
  , pedantic: false
  , sanitize: true
  });
  //Function to render MarkdownPreview:
  var render = function(){
    var input = $('#InformationContent').val();
    if(input == "")
      input = "Nothing there to preview.";
    $('#MarkdownPreview').html(marked(input));
  };
  //Looking for InformationContent to render:
  $('div.InformationContent').each(function(i, v){
    $(v).html(marked($(v).text()));
  });
  //Installing the editor if possible:
  if($('#MarkdownEditor').length == 1){  
    //Initial preview:
    render();
    //Preview on change of content:
    $('#InformationContent').keyup(function(){
      render();
    });
    var getRq = function(){
      return {
        title:          $('form#MarkdownEditor input#InformationTitle').val()
      , description:    $('form#MarkdownEditor textarea#InformationDescription').val()
      , content:        $('form#MarkdownEditor textarea#InformationContent').val()
      , informationId:  $('form#MarkdownEditor').attr("data-iid")
      };
    };
    var gotoIid = function(reply){
      if(/^FAIL/.test(reply)){
        alert(reply);
      }else{
        var iid = reply.match(/.*IId \(Id (.*)\)$/);
        document.location.href = "/information.html?display=" + iid[1];
      }
    };
    //Savebutton of the Editor:
    $('form#MarkdownEditor a#EditorSave').click(function(){
      var rq = getRq();
      if(rq.informationId){
        $.post("/action/edit/update", rq, function(reply){
          gotoIid(reply);
        });
      }else{
        $.post("/action/edit/create", rq, function(reply){
          gotoIid(reply);
        });
      }
    });
    //Addbutton of the Editor:
    if(getRq().informationId){
      $('form#MarkdownEditor a#EditorAdd').click(function(){
        var rq    = getRq();
        rq.split  = "True";
        $.post("/action/edit/update", getRq(), function(reply){
          gotoIid(reply);
        });
      });
    }else{ //Hiding the Addbutton if not editing an existing Information:
      $('form#MarkdownEditor a#EditorAdd').parent().hide();
    }  
  }
};
