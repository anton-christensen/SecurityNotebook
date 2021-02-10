
export default {
  template: `
  <div class="textInputComponent component">
    <span class="hideToggle" v-on:click="hidden = !hidden" v-bind:class="{hidden: hidden, shown: !hidden}">
      <i class="fas fa-eye-slash hidden-icon" ></i>
      <i class="fas fa-eye shown-icon"></i>
    </span>
    <div v-bind:class="{hide: hidden}">
      <CodeInput v-bind:text="text" v-on:change="textChange"></CodeInput>
    </div>
  </div>
  `,
  data: () => Object({
    text: "",
    hidden: false,
  }),
  methods: {
    getType: () => "TextInput",
    getState: function() {
      return {text: this.text, hidden: this.hidden};
    },
    setState: function(s) {
      var self = this;
      self.text = s.text;
      setTimeout(() => {
        self.hidden = s.hidden;
      }, 1);
    },

    
    textChange: function(newVal) {
      this.text = newVal;
    },
  },
  mounted: function() {
    var self = this;
  },
  
}
