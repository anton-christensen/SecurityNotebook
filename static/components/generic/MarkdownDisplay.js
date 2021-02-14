export default {
    template: `<div class="markdownDisplay" v-html="markdown" ref="mdout"></div>`,
    props: ['mdtext'],
    data: function() {
      return {
        markdown: marked(this.mdtext),
      }
    },
    watch: { 
      mdtext: function(newVal, oldVal) {
        this.markdown = marked(newVal);
      }
    },
    updated: function() {
      MathJax.Hub.Typeset();
    },
    mounted: function() {
      var self = this;

      MathJax.Hub.Config({
        tex2jax: {
          inlineMath: [ ['$','$'], ["\(","\)"] ],
          displayMath: [ ['$$','$$'], ["\[","\]"] ],
          // processEscapes: true,
          // processEnvironments: true
        },
        messageStyle: "none", // hides loading messages

        // Center justify equations in code and markdown cells. Elsewhere
        // we use CSS to left justify single line equations in code cells.
        displayAlign: 'center',
        "HTML-CSS": {
          styles: {'.MathJax_Display': {"margin": 0}},
          linebreaks: { automatic: true }
        }
      });
      MathJax.Hub.Queue(["Typeset", MathJax.Hub, self.$refs.mdout]);
      MathJax.Hub.Typeset();
    },
  }
  