
export default {
  template: `
  <div ref="markdownOutput" class="markdownOutputComponent component">
    <div class="notebook-component-header">
      <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event">Input</ComponentRef>
    </div>
    <MarkdownDisplay v-bind:mdtext="text"></MarkdownDisplay>
  </div>
  `,
  props: ['others'],
  watch: {
    others: function(newval, oldval) {console.log("updated others");}
  },
  computed: {
    text: function() {
      return this.textref ? this.textref.text : "<span class='red'>missing component reference</span>";
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
