
export default {
  template: `
  <div class="textInputComponent component">
    <CodeInput v-bind:text="text" v-on:change="textChange"></CodeInput>
  </div>
  `,
  data: () => Object({
    text: "",
  }),
  methods: {
    getType: () => "TextInput",
    getState: function() {
      return {text: this.text};
    },
    setState: function(s) {
      var self = this;
      self.text = s.text;
    },

    textChange: function(newVal) {
      this.text = newVal;
    },
  },
  mounted: function() {
    var self = this;
  },
  
}
