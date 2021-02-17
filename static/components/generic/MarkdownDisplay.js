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
    },
    mounted: function() {
      var self = this;
    },
  }
  