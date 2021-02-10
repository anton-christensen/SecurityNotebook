
export default {
  template: `<input ref="input" v-bind:class="{red: !outref}" type="number" v-bind:value="inputNumber" v-on:input="bindInput" />`,
  props: ['refs', 'type'],
  data: () => Object({
    inputNumber: null,
    outref: null,
  }),
  methods: {
    getState: function() {
      return this.inputNumber;
    },
    setState: function(n) {
      this.$refs.input.value = n;
      this.$refs.input.dispatchEvent(new Event('input'));
    },
    
    bindInput: function(e) {
      var val = e.target.value;
      if( this.refs[val] && 
        this.refs[val][0].getType && 
        this.refs[val][0].getType() == this.type
        ) {
          this.outref = this.refs[val][0];
        }
        else {
          this.outref = null;
        }
        this.$emit('ref', this.outref);
        this.inputNumber = val;
      }
      
      
    },
  }
  