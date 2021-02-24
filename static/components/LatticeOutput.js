
export default {
    template: `
    <div ref="analysisOutput" class="analysisOutputComponent component">
      <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event">Input</ComponentRef>
      <br>
      <pre ref="dot" class="dot" v-bind:class="{red: !response.success}">{{response.msg}}</pre>
      
      <GraphvizDisplay v-bind:dot="response.dot" v-bind:negpadding="60"></GraphvizDisplay>
    </div>
    `,
    props: ['others'],
    computed: {
      text: function() {
        return this.textref ? this.textref.text : "error";
      }
    },
    watch: {
      text: function(newVal, oldVal) {
        this.text = newVal;
        this.update();
      }
    },
    data: () => Object({
      textref: null,

      response: {
        success: false,
        dot: "digraph G {}",
        msg: "",
      },
    }),
    methods: {
      getType: () => "LatticeOutput",
      getState: function() {
        return {
          binding: this.$refs.binding.getState(),
        };
      },
      setState: function(s) {
        this.$refs.binding.setState(s.binding);
        this.update();
      },
  
      update: function() {
        var self = this;
  
        axios.post(
          '/api/lattice', 
          this.text
        )
        .then(function (response) {
          var data = response.data;
        
          self.response.success = data.startsWith("digraph");
          if(self.response.success) {
            self.response.msg = "";
            self.response.dot = data;
          }
          else {
            self.response.msg = data;
            self.response.dot = "digraph G {}";
          }
        })
        .catch(function (error) {
          console.log(error);
        });
      },
    },
    mounted: function() {
      this.update();
    },
    
  }
  