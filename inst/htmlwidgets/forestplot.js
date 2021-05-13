HTMLWidgets.widget({

  name: 'forestplot',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {


        x.data = HTMLWidgets.dataframeToD3(x.data);
        forestPlot(x.data, el, x.settings);
        

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
