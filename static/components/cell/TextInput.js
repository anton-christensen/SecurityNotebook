
export default {
  template: `
  <div class="textInputComponent component">
    <CodeInput v-bind:text="initialText" v-on:change="textChange" v-bind:disabled="self.locked"></CodeInput>
  </div>
  `,
  props: ['self'],
  data: () => Object({
    initialText: "",
    text: "",
  }),
  methods: {
    getType: () => "TextInput",
    getState: function() {
      return {text: this.text};
    },
    setState: function(s) {
      this.initialText = this.text = s.text;
    },

    textChange: function(newVal) {
      this.text = newVal;
    },
  },
  mounted: function() {
  },
  
}
