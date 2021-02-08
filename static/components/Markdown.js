export default {
    template: `
      <div ref="markdown" class="markdownComponent component">
        <CodeInput v-bind:text="initialText" v-on:change="textChange"></CodeInput>
        <MarkdownDisplay v-bind:mdtext="input"></MarkdownDisplay>
      </div>
    `,
    data: () => Object({
      input: "",
      initialText: `# Hello title my ol' friend
*It's good to see $\\LaTeX$ again*
`,
    }),
    methods: {
      textChange: function(newVal) {
        this.input = newVal;
      }
    },
  }
  