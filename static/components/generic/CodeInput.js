export default {
    template: `<textarea ref="code" class="code">{{internalText}}</textarea>`,
    props: ['text'],
    data: function() { 
      return {
        internalText: this.text,
        editor: null,
      };
    },
    watch: { 
      text: function(newVal, oldVal) {  
        var cursorPosition = this.editor.getCursor();
        this.editor.setValue(newVal);
        this.editor.setCursor(cursorPosition);
      }
    },

    mounted: function() {
      var self = this;
      self.editor = CodeMirror.fromTextArea(self.$refs.code, {
        lineNumbers: true,
        viewportMargin: Infinity
      });
      self.editor.on('change', function(cm) {
        self.internalText = cm.getValue();
        self.$emit('change', self.internalText);
      });
      self.$emit('change', self.internalText);
    },
  }
  