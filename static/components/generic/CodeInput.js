export default {
    template: `
      <textarea ref="code" class="code">{{internalText}}</textarea>
    `,
    props: ['text', 'disabled'],
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
      },
      disabled: function(newVal, oldVal) {
        this.editor.setOption('readOnly', newVal);
        if(newVal) {
          this.editor.getWrapperElement().classList.add('disabled');
        }
        else {
          this.editor.getWrapperElement().classList.remove('disabled');
        }
      }
    },

    mounted: function() {
      var self = this;
      self.editor = CodeMirror.fromTextArea(self.$refs.code, {
        lineNumbers: true,
        viewportMargin: Infinity,
      });

      if(this.disabled) {
        this.editor.setOption('readOnly', true);
        this.editor.getWrapperElement().classList.add('disabled');
      }
      
      self.editor.on('change', function(cm) {
        self.internalText = cm.getValue();
        self.$emit('change', self.internalText);
      });
      self.$emit('change', self.internalText);
    },
  }
  