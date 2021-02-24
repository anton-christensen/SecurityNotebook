
export default {
  template: `
    <span class="hideInPrint">
      <label><slot></slot></label>
      <input class="component-ref minimalistInput" v-bind:class="{ red: !outref }" ref="input" placeholder="reference" type="text" v-model="text" />
    </span>`,
  props: ['refs', 'type'],
  data: () => Object({
    text: "",
  }),
  computed: {
    outref: function() {
      var outref = null;
      var val = this.text;
      var n = parseInt(val.trim());
      if( n !== NaN &&
        this.refs[val] && 
        this.refs[val][0].getType && 
        this.refs[val][0].getType() == this.type
        ) {
          outref = this.refs[val][0];
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
        outref = ref ? ref[0] : null;
      }
      this.$emit('ref', outref);
      return outref
    }
  },
  methods: {
    getState: function() {
      return this.text;
    },
    setState: function(n) {
      this.$refs.input.value = n;
      this.$refs.input.dispatchEvent(new Event('input'));
    },
  },
}
  