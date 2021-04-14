
export default {
  template: `
  <div ref="markdownOutput" class="markdownOutputComponent component">
    <div class="notebook-component-header">
      <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event">Input</ComponentRef>
    </div>
    <div ref="markdown">
      <MarkdownDisplay v-bind:mdtext="text"></MarkdownDisplay>
    </div>
  </div>
  `,
  props: ['others'],
  watch: {
    others: function(newval, oldval) { }
  },
  computed: {
    text: function() {
      var self = this;
      setTimeout(function() {
        let codeElms = self.$refs.markdown.getElementsByTagName('code');
        for(var i = 0; i < codeElms.length; i++) {
          let refName = codeElms[i].getAttribute('cellref');
          if(refName) {
            // stolen from componentRef
            var outref = null;
            var val = refName;
            var n = parseInt(val.trim());
            if( n !== NaN &&
              self.others[val] && 
              self.others[val][0].getType && 
              self.others[val][0].getType() == "TextInput"
              ) {
                outref = self.others[val][0];
            }
            else {
              var ref = Object.values(self.others).find( (r) => 
                r.length && 
                r.length > 0 && 
                r[0].$attrs.alias && 
                r[0].$attrs.alias.trim() &&  // tests for empty string
                r[0].$attrs.alias.trim() == val && 
                r[0].getType && 
                r[0].getType() == "TextInput"
              );
              outref = ref ? ref[0] : null;
            }
            // stolen from componentRef
            if(outref) {
              let text = "";
              if(codeElms[i].getAttribute('startline')) {
                let fstln = parseInt(codeElms[i].getAttribute('startline'));
                let lastln = parseInt(codeElms[i].getAttribute('endline'));
                text = outref.text.split('\n').splice(fstln-1,lastln-fstln+1).join('\n');
              }
              else {
                text = outref.text;
              }
              codeElms[i].textContent = text;
            }
            else {
              codeElms[i].innerHTML=`<span class="red">Invalid cell reference</span>`
            }
          }
        }
      }, 0);

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
