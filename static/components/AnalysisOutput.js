
export default {
  template: `
  <div ref="analysisOutput" class="analysisOutputComponent component">
    <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event"></ComponentRef>
    <br>
    <button v-on:click="req.stepCount  = 0; update()" v-bind:disabled="req.stepCount <= 0"><<</button>
    <button v-on:click="req.stepCount -= 1; update()" v-bind:disabled="req.stepCount <= 0"><</button>
    <select v-model="req.analysisName" v-on:change="reset(); update()">
      <option v-for="analysis in analyses" v-bind:value="analysis.value">
        {{ analysis.text }}
      </option>
    </select>
    <button v-on:click="req.stepCount += 1; update()" v-bind:disabled="req.stepCount >= maxSteps" >></button>
    <button v-on:click="req.stepCount = -1; update()" v-bind:disabled="req.stepCount >= maxSteps" >>></button>
    <br>
    <span>step {{ req.stepCount }}</span>
    <pre ref="dot" class="dot" v-bind:class="{red: !response.success}">{{response.msg}}</pre>
    
    <GraphvizDisplay v-bind:dot="response.dot"></GraphvizDisplay>
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
      this.req.programCode = newVal;
      // this.reset(); // ? maybe this
      this.update();
    }
  },
  data: () => Object({
    textref: null,

    req: {
      programCode: ``,
      analysisName: 'While-dummy',
      stepCount: 0,
    },
    response: {
      success: false,
      dot: "digraph G {}",
      msg: "",
    },
    maxSteps: Infinity,

    analyses: [
      {value: "TinyARM-reachingDefinitions", text: "TinyARM Reaching definitions analysis"},
      {value: "TinyARM-liveness", text: "TinyARM Liveness analysis"},
      {value: "While-dummy", text: "While CFG display for testing purposes"},
      {value: "While-taint", text: "While taint analysis"},
      
    ],
  }),
  methods: {
    getType: () => "AnalysisOutput",
    getState: function() {
      return {
        req: JSON.parse(JSON.stringify(this.req)),
        binding: this.$refs.binding.getState(),
      };
    },
    setState: function(s) {
      console.log("analysis set state",s);
      this.req = s.req;
      this.$refs.binding.setState(s.binding);
      this.update();
    },


    textChange: function(newVal) {
    },
    reset: function() {
      this.req.stepCount = 0;
      this.maxSteps = Infinity;
    },

    update: function() {
      var self = this;

      axios.post(
        '/api', 
        JSON.stringify(this.req)
      )
      .then(function (response) {
        var data = response.data;

        self.response.success = data.success;
        if(data.success) {
          self.response.msg = "";
          self.response.dot = data.dot;
          if(data.worklist == "[]") {
            self.maxSteps = self.req.stepCount = data.stepsTaken;
          }
        }
        else {
          self.response.msg = data.message;
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
