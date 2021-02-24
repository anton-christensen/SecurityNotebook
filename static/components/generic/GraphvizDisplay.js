export default {
  template: `
  <div class="generic-component-graphviz">
    <ion-icon v-on:click="resetZoom" class="icon shownOnHover hideInPrint" name="refresh-outline"></ion-icon>
    <div ref="graph" class="graph" style="text-align: center;"></div>
  </div>`,
  props: ['dot', 'negpadding', 'scale'],
  data: () => Object({
    graphviz: null
  }),
  watch: { 
    dot: function(newVal, oldVal) {
      var self = this;
      self.graphviz.renderDot(newVal)
        .on("end", function () {
          self.resetZoom();
        });
    }
  },
  methods: {
    resetZoom: function() {
      this.graphviz.resetZoom(d3.transition().duration(250));
    },
    attributer: function(datum, index, nodes) {
      d3.select(this);
      if (datum.tag == "svg") {
        var dim = this.$refs.graph.getBoundingClientRect();
        datum.attributes.width = dim.width
        var viewbox = datum.attributes.viewBox.split(' ');
        var negpad = this.negpadding ? this.negpadding : 0;
        var scale = this.scale ? this.scale : 1;
        var invScale = 1/scale;
        viewbox = viewbox.map((x) => parseFloat(x))
        datum.attributes.height = parseInt(viewbox[3])// dim.height;
        var vb = {x: viewbox[0]+negpad, y: viewbox[1]+negpad, w: viewbox[2]-negpad*2, h: viewbox[3]-negpad*2};
        
        datum.attributes.viewBox = vb.x + " " + vb.y + " " + vb.w + " " + vb.h;
        
      }
    },
  },
  mounted: function() {
    var self = this;
    this.graphviz = d3.select(this.$refs.graph).graphviz().attributer(this.attributer);
  },
}
