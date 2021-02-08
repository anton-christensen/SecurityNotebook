
export default {
  template: `
    <div ref="analysis" class="analysisComponent component">
      <CodeInput v-bind:text="req.programCode" v-on:change="textChange"></CodeInput>
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
      <pre ref="dot" class="dot" v-bind:style="{color: response.success ? 'black' : 'red'}">{{response.msg}}</pre>

      <GraphvizDisplay v-bind:dot="response.dot"></GraphvizDisplay>
    </div>
  `,
  data: () => Object({
    req: {
      programCode: `
    ldr r1, [sp, #0]
loop:
    cmp r1, #0
    b_eq end
    sub r1, r1, #1
    b loop
end:
    mov r2, r1
`,
      analysisName: 'TinyARM-reachingDefinitions',
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
    ],
  }),
  methods: {
    textChange: function(newVal) {
      this.req.programCode = newVal;
      this.update();
    },
    reset: function() {
      this.req.stepCount = 0;
      this.maxSteps = Infinity;
    },

    update: function() {
      var self = this;
      $.post( "/api", JSON.stringify(this.req), function( data ) {
        data = JSON.parse(data);

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
      });
    },
  },
  mounted: function() {
    var self = this;

    this.update();

    // fix this acting on all components
    // document.addEventListener("keyup", function(e) {
    //     if(e.key == "ArrowLeft" && self.req.stepCount > 0) {
    //       self.req.stepCount -= e.ctrlKey ? self.req.stepCount : 1;
    //       self.update();
    //     }
    //     if(e.key == "ArrowRight") {
    //       self.req.stepCount += e.ctrlKey ? -(self.req.stepCount+1) : 1;
    //       self.update();
    //     }
    // });
  },

}
