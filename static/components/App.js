

export default {
  el: '#app',
  template: `
    <div id="app">
      <button v-on:click="saveDocument">Save</button>
      <button v-on:click="loadDocument">Load</button>
      <button v-on:click="exportDocument">Export</button>
      <button v-on:click="importDocument">Import</button>

      <!-- hidden elements to trigger download and file dialog -->
      <a ref="downloadAnchor" style="display:none"></a>
      <input ref="importFile" type="file" style="display:none"></input>
      
      <div id="notebookElements">
        <div class="notebook-component" v-for="element in elements">
          <span class="name">#{{element.id}} - {{ element.name }}</span>
          <span class="deleteComponent" v-on:click="deleteComponent(element.id)"><i class="fas fa-trash"></i></span>
          <component 
            :key="element.id" 
            :ref="element.id" 
            :is="element.name"   
            v-bind:others="$refs"
          ></component>
        </div>
      </div>

      <span>Add component</span>
      <select v-model="selected">
        <option v-for="analysis in analyses" v-bind:value="analysis.value">
          {{ analysis.text }}
        </option>
      </select>
    </div>
  `,
  data: {
    analyses: [
      {value: "", text: "-- Select new component --"},
      // {value: "markdown", text: "Markdown"},
      // {value: "analysis", text: "Analysis"},
      {value: "text-input", text: "Text Input"},
      {value: "text-output", text: "Text Output"},
      {value: "markdown-output", text: "Markdown Output"},
      {value: "analysis-output", text: "Analysis Output"},
    ],
    selected: '',
    id: 0,

    elements: [],
  },
  watch: {
    selected: function(newVal) {
      if(newVal) {
        this.addComponent(newVal);
        this.selected = "";
      }
    }
  },
  methods: {
    addComponent: function(componentName) {
      var id = this.id++;
      this.elements.push(
        {
          id: id,
          name: componentName, 
        }
      );
    },
    deleteComponent: function(id) {
      var doc = this.getDocument();
      doc.elements = doc.elements.filter(elm => elm.id != id);
      this.setDocument(doc);
    },

    getDocument: function() {
      var doc = {
        nextID: this.id,
        elements: JSON.parse(JSON.stringify(this.elements)),
      };
      
      for(var i = 0; i < doc.elements.length; i++) {
        var component = this.$refs[doc.elements[i].id][0];
        if(component.getState) {
          doc.elements[i].state = component.getState();
        }
      }
      return doc;
    },
    setDocument: function(doc) {
      var self = this;
      this.elements = [];
      
      this.id = doc.nextID;
      for(var i = 0; i < doc.elements.length; i++) {
        this.elements.push({
          id: doc.elements[i].id,
          name: doc.elements[i].name
        });
      }
      
      setTimeout(function() {
        for(var i = 0; i < doc.elements.length; i++) {
          self.$refs[doc.elements[i].id][0].setState(doc.elements[i].state);
        }
      }, 1);
    },
    saveDocument: function() {
      localStorage.setItem('doc', JSON.stringify(this.getDocument()));
    },
    loadDocument: function() {
      var doc = JSON.parse(localStorage.getItem('doc'));
      this.setDocument(doc);
    },
    exportDocument: function() {
      var dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(this.getDocument()));
      var dlAnchorElem = this.$refs.downloadAnchor;
      dlAnchorElem.setAttribute("href",     dataStr     );
      dlAnchorElem.setAttribute("download", "notebook.json");
      dlAnchorElem.click();
    },
    importDocument: function() {
      var self = this;
      var importFileElm = this.$refs.importFile;
      importFileElm.onchange = () => {
        if(importFileElm.value) {
          var fr = new FileReader();
          fr.onload = () => {
            self.setDocument(JSON.parse(fr.result))
          }
          fr.readAsText(importFileElm.files[0]);
        }
      }
      importFileElm.click();
    },
  },
  mounted: function() {
    var self = this;
  },
};

