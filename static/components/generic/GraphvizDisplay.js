export default {
  template: `<div ref="graph" class="graph" style="text-align: center;"></div>`,
  props: ['dot'],
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
        datum.attributes.width = dim.width;
        datum.attributes.height = dim.height;
      }
    },
  },
  mounted: function() {
    var self = this;
    this.graphviz = d3.select(this.$refs.graph).graphviz().attributer(this.attributer);
  },
}