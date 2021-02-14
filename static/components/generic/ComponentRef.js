
export default {
  template: `<input class="component-ref visibleOnComponentHover op100" ref="input" placeholder="reference" v-bind:class="{red: !outref}" type="text" v-bind:value="text" v-on:input="bindInput" />`,
  props: ['refs', 'type'],
  data: () => Object({
    text: null,
    outref: null,
  }),
  methods: {
    getState: function() {
      return this.text;
    },
    setState: function(n) {
      this.$refs.input.value = n;
      this.$refs.input.dispatchEvent(new Event('input'));
    },
    
    bindInput: function(e) {
      var val = e.target.value;
      var n = parseInt(val.trim());
      if( n !== NaN &&
        this.refs[val] && 
        this.refs[val][0].getType && 
        this.refs[val][0].getType() == this.type
        ) {
          this.outref = this.refs[val][0];
      }
      else {
        var ref = Object.values(this.refs).find( (r) => 
          r.length && 
          r.length > 0 && 
          r[0].$attrs.alias && 
          r[0].$attrs.alias.trim() &&  // tests for empty string
          r[0].$attrs.alias.trim() == val && 
          r[0].getType && 
          r[0].getType() == this.type
        );
        this.outref = ref ? ref[0] : null;
      }
      this.$emit('ref', this.outref);
      this.text = val;
    }
  },
}
  