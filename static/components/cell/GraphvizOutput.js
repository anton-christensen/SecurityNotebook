
export default {
    template: `
    <div ref="graphvizOutput" class="graphvizOutputComponent component">
      <ComponentRef ref="binding" v-bind:refs="others" type="TextInput" v-on:ref="textref = $event">Input</ComponentRef>
      <br>
      <pre v-show="error" class="red">{{error}}</pre>

      <GraphvizDisplay v-bind:dot="text" v-on:error="error = $event"></GraphvizDisplay>
    </div>
    `,
    props: ['others'],
    computed: {
      text: function() {
        return this.textref ? this.textref.text : "";
      }
    },
    watch: {
        text: function(oldVal, newVal) {
            this.error = null;
        }
    },
    data: () => Object({
      textref: null,
      error: null
    }),
    methods: {
      getType: () => "GraphvizOutput",
      getState: function() {
        return {
          binding: this.$refs.binding.getState(),
        };
      },
      setState: function(s) {
        this.$refs.binding.setState(s.binding);
      },
    },
  }
  