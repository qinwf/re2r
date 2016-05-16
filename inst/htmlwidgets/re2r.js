
HTMLWidgets.widget({

  name: "re2r",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    // create our sigma object and bind it to the element
    var parseError = false;
    var dd  = document.createElement("div");
    var to_remove = document.getElementById("svg-container-base");
    if (to_remove){
    	to_remove.parentNode.removeChild(to_remove);
    }
    dd.setAttribute("style","display: none;");
    dd.setAttribute("id","svg-container-base");

    var progress_ = document.createElement("div");
    progress_.appendChild(document.createElement("div"));
    progress_.setAttribute("class","progress");
    var svg_ = document.createElement("div");
    svg_.setAttribute("class","svg svg-container");
    var svg_svg = document.createElement("svg");
    svg_svg.setAttribute("class","regexp-svg");
    svg_svg.setAttribute("id",el.id+"regexp-svg");
    svg_svg.setAttribute("preserveAspectRatio","xMinYMin meet");
    svg_.appendChild(svg_svg);
    dd.appendChild(svg_);
    dd.appendChild(progress_);
    document.body.appendChild(dd);
    var ress = document.createElement("div");
    ress.setAttribute("id",el.id+"svg");
    document.getElementById(el.id).appendChild(ress);

        console.log("parsing");
    var sig = new Parser(document.getElementById(el.id+"svg"));
    sig.re2r_id = el.id;

    return {
      renderValue: function(x) {
        datastring = x.data;
        this.datastring = x.data;
        console.log(datastring);

        sig
        // Parse the expression.
        .parse(datastring)
        // Display any error messages from the parser and abort the render.
        .catch(function(message){
          this.state = 'has-error';
          console.log(message);
          parseError = true;

          throw message;
        }.bind(this))
        // When parsing is successful, render the parsed expression.
        .then(function(parser){
          return parser.render();
        }.bind(this))
        .then(function(){
        this.state = 'has-results';
      }.bind(this))
      // Handle any errors that happened during the rendering pipeline.
      // Swallows parse errors and render cancellations. Any other exceptions
      // are allowed to continue on to be tracked by the global error handler.
      .catch(function(message){
        if (message === 'Render cancelled') {
          this.state = '';
        } else if (parseError) {
        } else {
          console.log(message);
          throw message;
        }
      }.bind(this))
      // Finally, mark rendering as complete (and pass along any exceptions
      // that were thrown).
      .then(
        function(){
          this.running = false;
          var temps = document.getElementById(el.id+"regexp-svg");
          sig.re2r_width_ = temps.getAttribute("width") / width ;
          sig.re2r_ratio_ = temps.getAttribute("height") / temps.getAttribute("width") ;

          temps.setAttribute("viewBox","0,0," + temps.getAttribute("width") +"," + temps.getAttribute("height"));
          
          temps.setAttribute("height","100%");
          temps.setAttribute("width","100%");
        }.bind(this),
        function(message){
          this.running = false;
          throw message;
        }.bind(this)
      ).catch(function(message){console.log(message)});
      


      },
      
      resize: function(width, height) {

          var temps = document.getElementById(sig.re2r_id + ".regexp-svg");
          to_wid =  sig.re2r_width_ * width ;
          to_hi  = to_wid * sig.re2r_ratio_;

          temps.setAttribute("viewBox","0,0," +  to_wid  +"," + to_hi );
        
      }.bind(this),
      
      // Make the sigma object available as a property on the widget
      // instance we're returning from factory(). This is generally a
      // good idea for extensibility--it helps users of this widget
      // interact directly with sigma, if needed.
      s: sig,
      datastring: "",
    };
  }
});
