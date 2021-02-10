
export default {
  template: `
  <div ref="analysis" class="analysisComponent component">
    <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event"></ComponentRef>
    <pre v-bind:class="{red: !textref}">{{text}}</pre>
  </div>
  `,
  props: ['others'],
  computed: {
    text: function() {
      return this.textref ? this.textref.text : "error";
    }
  },
  data: () => Object({
    textref: null,
  }),
  methods: {
    getType: () => "TextOutput",
    getState: function() {
      return {binding: this.$refs.binding.getState()};
    },
    setState: function(s) {
      this.$refs.binding.setState(s.binding);
    },
  },
  mounted: function() {
  },
  
}
