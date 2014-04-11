/*
  Big parts of this are taken from the springy.js file,
  and adjusted to my special needs.
*/
SpringyRenderer = Backbone.View.extend({
  colors: {
    accepted: '#00FF00'
  , undecided: '#FFFF00'
  , rejected: '#FF0000'
  , PSScintillaOfEvidence: '#FF8000'
  , PSPreponderanceOfEvidence: '#CBCB3C'
  , PSBeyondResonableDoubt: '#A5DC6F'
  , RelationAttack: '#FF0000'
  , RelationSupport: '#0000FF'
  , RelationCustom: '#000000'
  }
, springySetup: function(){
    var view = this;
    this.$canvas = this.$('#graphCanvas');
    this.canvas = this.$canvas.get(0);
    this.ctx = this.canvas.getContext("2d");
    this.graph = new Springy.Graph();
    this.nodeFont = "16px Verdana, sans-serif";
    this.edgeFont = "8px Verdana, sans-serif";
    this.nodeSelected = null;
    this.selected = null;
    this.nearest = null;
    this.dragged = null;
    this.layout = new Springy.Layout.ForceDirected(this.graph, 400.0, 400.0, 0.5);
    this.currentBB = this.layout.getBoundingBox();
    //Drag&Drop with Springy:
    jQuery(this.canvas).mousemove(function(e){
      view.mousemove(e, this);
    }).dblclick(function(e){
      view.dblclick(e, this);
    }).mousedown(function(e){
      view.mousedown(e, this);
    });
    jQuery(window).bind('mouseup',function(e){
      view.dragged = null;
    });
    //Extending Springy.Node:
    Springy.Node.prototype.getWidth = function() {
      var ctx = view.ctx;
      //Finding the longest text; we don't care that id might be longer than label.
      var text = _.max([this.data.label, this.id, this.data.formula], function(t){
        if(typeof(t) === 'undefined')
          return 0;
        if(typeof(t) !== 'string')
          t = ''+t;
        return t.length;
      });
      //Checking if the width was already calculated:
      if(this._width && this._width[text])
        return this._width[text];
      //Calculating the width:
      ctx.save();
      ctx.font = (this.data.font !== undefined) ? this.data.font : view.nodeFont;
      var width = ctx.measureText(text).width + 10;
      ctx.restore();
      //Saving the calculation:
      this._width || (this._width = {});
      this._width[text] = width;
      //Finished.
      return width;
    };
    Springy.Node.prototype.getHeight = function() {
      if(this.data.formula)
        return 40;
      return 20;
    };
    //Starting springy:
    this.renderer = new Springy.Renderer(
        this.layout
        , function(){return view.clear();}
        , function(edge, p1, p2){return view.drawEdge(edge, p1, p2);}
        , function(node, p){return view.drawNode(node, p);});
    Springy.requestAnimationFrame(function (){return view.adjustBoundingBox();});
    this.renderer.start();
}
// Executes a custom render cycle in case the renderer is stopped.
, render: function(){
    if(this.layout._started)
      return this;
    var t = this;
    t.clear();
    t.layout.eachEdge(function(edge, spring){
        t.drawEdge(edge, spring.point1.p, spring.point2.p);
    });
    t.layout.eachNode(function(node, point){
      t.drawNode(node, point.p);
    });
    return this;
  }
// Springy related functions:
, clear: function(){
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
  }
,	drawEdge: function(edge, p1, p2){
    var ctx = this.ctx;
    var s1 = this.toScreen(p1), s2 = this.toScreen(p2);
    var x1 = s1.x, y1 = s1.y, x2 = s2.x, y2 = s2.y;

    var direction = new Springy.Vector(x2-x1, y2-y1);
    var normal = direction.normal().normalise();

    var from = this.graph.getEdges(edge.source, edge.target);
    var to = this.graph.getEdges(edge.target, edge.source);

    var total = from.length + to.length;

    // Figure out edge's position in relation to other edges between the same nodes
    var n = 0;
    for(var i=0; i<from.length; i++){
      if(from[i].id === edge.id){
        n = i;
      }
    }

    //change default to  10.0 to allow text fit between edges
    var spacing = 12.0;

    // Figure out how far off center the line should be drawn
    var offset = normal.multiply(-((total - 1) * spacing)/2.0 + (n * spacing));

    var s1 = this.toScreen(p1).add(offset);
    var s2 = this.toScreen(p2).add(offset);

    var boxWidth = edge.target.getWidth();
    var boxHeight = edge.target.getHeight();

    var intersection = this.intersect_line_box(s1, s2, {x: x2-boxWidth/2.0, y: y2-boxHeight/2.0}, boxWidth, boxHeight);
    if(!intersection){
      intersection = s2;
    }

    var stroke = (edge.data.color !== undefined) ? edge.data.color : '#000000';

    var arrowWidth;
    var arrowLength;

    var weight = (edge.data.weight !== undefined) ? edge.data.weight : 1.0;

    ctx.lineWidth = Math.max(weight *  2, 0.1);
    arrowWidth = 1 + ctx.lineWidth;
    arrowLength = 8;

    var directional = (edge.data.directional !== undefined) ? edge.data.directional : true;

    // line
    var lineEnd;
    if(directional){
      lineEnd = intersection.subtract(direction.normalise().multiply(arrowLength * 0.5));
    }else{
      lineEnd = s2;
    }
    ctx.strokeStyle = stroke;
    ctx.beginPath();
    ctx.moveTo(s1.x, s1.y);
    ctx.lineTo(lineEnd.x, lineEnd.y);
    ctx.stroke();
    // arrow
    if(directional){
      ctx.save();
      ctx.fillStyle = stroke;
      ctx.translate(intersection.x, intersection.y);
      ctx.rotate(Math.atan2(y2 - y1, x2 - x1));
      ctx.beginPath();
      ctx.moveTo(-arrowLength, arrowWidth);
      ctx.lineTo(0, 0);
      ctx.lineTo(-arrowLength, -arrowWidth);
      ctx.lineTo(-arrowLength * 0.8, -0);
      ctx.closePath();
      ctx.fill();
      ctx.restore();
    }
    // label
    if(edge.data.label !== undefined){
      text = edge.data.label
      ctx.save();
      ctx.textAlign = "center";
      ctx.textBaseline = "top";
      ctx.font = (edge.data.font !== undefined) ? edge.data.font : this.edgeFont;
      ctx.fillStyle = stroke;
      var textPos = s1.add(s2).divide(2).add(normal.multiply(-8));
      ctx.translate(textPos.x, textPos.y);
      ctx.rotate(Math.atan2(s2.y - s1.y, s2.x - s1.x));
      ctx.fillText(text, 0,-2);
      ctx.restore();
    }
  }
, drawNode: function(node, p){
    var ctx = this.ctx, selected = this.selected, nearest = this.nearest;
    var s = this.toScreen(p);
    ctx.save();
    //Node size:
    var boxWidth = node.getWidth();
    var boxHeight = node.getHeight();
    //Clear background
    ctx.clearRect(s.x - boxWidth/2, s.y - boxHeight/2, boxWidth, boxHeight);
    //Filling Background:
    if(node.data.color){
      ctx.fillStyle = node.data.color;
    }else if(selected !== null && selected.node !== null && selected.node.id === node.id){
      ctx.fillStyle = "#FFFFE0";
    }else if (nearest !== null && nearest.node !== null && nearest.node.id === node.id){
      ctx.fillStyle = "#EEEEEE";
    }else{
      ctx.fillStyle = "#FFFFFF";
    }
    ctx.fillRect(s.x - boxWidth/2, s.y - boxHeight/2, boxWidth, boxHeight);
    //Drawing text:
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.font = (node.data.font !== undefined) ? node.data.font : this.nodeFont;
    ctx.fillStyle = "#000000";
    var text = (node.data.label !== undefined) ? node.data.label : node.id;
    if(f = node.data.formula){
      ctx.fillText(text, s.x - boxWidth/2 + 5, s.y - 18);
      ctx.fillText(f, s.x - boxWidth/2 + 5, s.y - 4);
    }else{
      ctx.fillText(text, s.x - boxWidth/2 + 5, s.y - 8);
    }
    //Done
    ctx.restore();
  }
, toScreen: function(p){
    var currentBB = this.currentBB, canvas = this.canvas;
		var size = currentBB.topright.subtract(currentBB.bottomleft);
		var sx = p.subtract(currentBB.bottomleft).divide(size.x).x * canvas.width;
		var sy = p.subtract(currentBB.bottomleft).divide(size.y).y * canvas.height;
		return new Springy.Vector(sx, sy);
  }
, fromScreen: function(s){
    var canvas = this.canvas;
    var currentBB = this.currentBB;
		var size = currentBB.topright.subtract(currentBB.bottomleft);
		var px = (s.x / canvas.width) * size.x + currentBB.bottomleft.x;
		var py = (s.y / canvas.height) * size.y + currentBB.bottomleft.y;
		return new Springy.Vector(px, py);
  }
, intersect_line_line: function(p1, p2, p3, p4){
		var denom = ((p4.y - p3.y)*(p2.x - p1.x) - (p4.x - p3.x)*(p2.y - p1.y));

		// lines are parallel
		if (denom === 0) {
			return false;
		}

		var ua = ((p4.x - p3.x)*(p1.y - p3.y) - (p4.y - p3.y)*(p1.x - p3.x)) / denom;
		var ub = ((p2.x - p1.x)*(p1.y - p3.y) - (p2.y - p1.y)*(p1.x - p3.x)) / denom;

		if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
			return false;
		}

		return new Springy.Vector(p1.x + ua * (p2.x - p1.x), p1.y + ua * (p2.y - p1.y));
	}
, intersect_line_box: function(p1, p2, p3, w, h){
		var tl = {x: p3.x, y: p3.y};
		var tr = {x: p3.x + w, y: p3.y};
		var bl = {x: p3.x, y: p3.y + h};
		var br = {x: p3.x + w, y: p3.y + h};

		var result;
		if (result = this.intersect_line_line(p1, p2, tl, tr)) { return result; } // top
		if (result = this.intersect_line_line(p1, p2, tr, br)) { return result; } // right
		if (result = this.intersect_line_line(p1, p2, br, bl)) { return result; } // bottom
		if (result = this.intersect_line_line(p1, p2, bl, tl)) { return result; } // left

		return false;
	}
, adjustBoundingBox: function(){
    var t = this;
		var targetBB = this.layout.getBoundingBox()
    var currentBB = this.currentBB;
		// current gets 20% closer to target every iteration
		currentBB = {
			bottomleft: currentBB.bottomleft.add( targetBB.bottomleft.subtract(currentBB.bottomleft)
				.divide(10)),
			topright: currentBB.topright.add( targetBB.topright.subtract(currentBB.topright)
				.divide(10))
		};
    this.currentBB = currentBB;
		Springy.requestAnimationFrame(function(){return t.adjustBoundingBox();});
  }
, mousedown: function(e, elem){
		var pos = jQuery(elem).offset();
		var p = this.fromScreen({x: e.pageX - pos.left, y: e.pageY - pos.top});
		this.selected = this.nearest = this.dragged = this.layout.nearest(p);

		if(this.selected.node !== null){
			this.dragged.point.m = 10000.0;

			if(this.nodeSelected){
				this.nodeSelected(this.selected.node);
			}
		}
    this.render();
  }
, dblclick: function(e, elem){
		var pos = jQuery(elem).offset();
		var p = this.fromScreen({x: e.pageX - pos.left, y: e.pageY - pos.top});
		this.selected = this.layout.nearest(p);
		var node = this.selected.node;
		if(node && node.data && node.data.ondoubleclick){
			node.data.ondoubleclick();
		}
  }
, mousemove: function(e, elem){
		var pos = jQuery(elem).offset();
		var p = this.fromScreen({x: e.pageX - pos.left, y: e.pageY - pos.top});
		this.nearest = this.layout.nearest(p);
    var dragged = this.dragged;

		if (dragged !== null && dragged.node !== null) {
			dragged.point.p.x = p.x;
			dragged.point.p.y = p.y;
		}
    this.render();
  }
});
