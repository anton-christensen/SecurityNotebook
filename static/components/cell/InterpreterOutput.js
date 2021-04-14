export default {
  template: `


  <div ref="interpreterOutput" class="interpreterOutputComponent component">
    <div class="notebook-component-header">
      <ComponentRef ref="codeBinding" v-bind:refs="others" type="TextInput" v-on:ref="coderef = $event">Source code input</ComponentRef>
      <ComponentRef ref="inputBinding" v-bind:refs="others" type="TextInput" v-on:ref="inputref = $event">Program Input</ComponentRef>
    </div>
    <pre ref="output" class="output" v-bind:class="{red: !response.success}">{{response.message}}</pre>
  </div>
  `,
  props: ['others', 'self'],
  computed: {
    code: function() {
      return this.coderef ? this.coderef.text : "";
    },
    input: function() {
      return this.inputref ? this.inputref.text : "";
    },
  },
  watch: {
    code: function(newVal, oldVal) {
      this.req.programCode = newVal;
      // this.reset(); // ? maybe this
      this.update();
    },
    input: function(newVal, oldVal) {
      this.req.inputFile = newVal;
      // this.reset(); // ? maybe this
      this.update();
    }
  },
  data: () => Object({
    coderef: null,
    inputref: null,

    req: {
      programCode: ``,
      inputFile: 'While-dummy',
      // stepCount: 0,
    },
    response: {
      success: false,
      message: "",
    },
    // maxSteps: Infinity,
  }),
  methods: {
    getType: () => "AnalysisOutput",
    getState: function() {
      return {
        req: JSON.parse(JSON.stringify(this.req)),
        codeBinding: this.$refs.codeBinding.getState(),
        inputBinding: this.$refs.inputBinding.getState(),
      };
    },
    setState: function(s) {
      this.req = s.req;
      this.$refs.codeBinding.setState(s.codeBinding);
      this.$refs.inputBinding.setState(s.inputBinding);
      this.update();
    },

    reset: function() {
      // this.req.stepCount = 0;
      // this.maxSteps = Infinity;
    },

    update: function() {
      var self = this;

      axios.post(
        '/api/while/execute', 
        JSON.stringify(this.req)
      )
      .then(function (response) {
        var data = response.data;
        self.response.success = data.success;
        self.response.message = data.message;
        if(data.success == true) {
          self.response.message = "Time: "+data.stepsTaken + "\n" + self.response.message;
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
  