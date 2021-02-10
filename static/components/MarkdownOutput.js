
export default {
  template: `
  <div ref="markdownOutput" class="markdownOutputComponent component">
    <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event"></ComponentRef>
    <MarkdownDisplay v-bind:mdtext="text"></MarkdownDisplay>
  </div>
  `,
  props: ['others'],
  computed: {
    text: function() {
      return this.textref ? this.textref.text : "error";
    }
  },
  data: () => Object({
    inputNumber: null,
    textref: null,
  }),
  methods: {
    getType: () => "MarkdownOutput",
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
