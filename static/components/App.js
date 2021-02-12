

export default {
  el: '#app',
  template: `
    <div id="app">
      <!-- hidden elements to trigger download and file dialog -->
      <a ref="downloadAnchor" style="display:none"></a>
      <input ref="importFile" type="file" style="display:none"></input>
      
      <div class="header">
        <div class="main-container">
          <div class="fl">
            <h1 contenteditable v-on:input="onTitleInput" ref="title">Security Analysis Notebook</h1>
          </div>
          <div class="fr">
            <button v-on:click="saveDocument">Save</button>
            <button v-on:click="loadDocument">Load</button>
            <button v-on:click="exportDocument">Export</button>
            <button v-on:click="importDocument">Import</button>
          </div>
        </div>
      </div>

      
      <div class="main-container">
        <div id="notebookElements">
          <div class="notebook-component" v-for="element in elements">
            <div class="notebook-component-shoulder leftof">
              <div class="notebook-component-fold hideToggle" v-on:click="element.hidden = !element.hidden" v-bind:class="{hidden: element.hidden, shown: !element.hidden}">
                <i class="fas fa-eye-slash hidden-icon" ></i>
                <i class="fas fa-eye shown-icon"></i>
              </div>
              <div class="notebook-component-add before"></div>
              <div class="notebook-component-add after"></div>
            </div>
            <div class="notebook-component-shoulder rightof">
              <span class="notebook-component-remove" v-on:click="deleteComponent(element.id)"><i class="fas fa-trash"></i></span>
            </div>
            
            <span class="name">#{{element.id}} - </span>
            <input class="alias" v-model="element.alias" placeholder="Alias" />
            <span class="name fr">{{ element.type }}</span>
            <div class="notebook-component-content" v-bind:class="{hide: element.hidden}">
              <component 
                :key="element.id" 
                :alias="element.alias"
                :ref="element.id" 
                :is="element.type"   
                v-bind:others="$refs"
              ></component>
            </div>
          </div>
        </div>

        <span>Add component</span>
        <select v-model="selected">
          <option v-for="analysis in analyses" v-bind:value="analysis.value">
            {{ analysis.text }}
          </option>
        </select>
      </div>
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
      {value: "lattice-output", text: "Lattice Output"},
    ],
    selected: '',
    id: 0,
    title: 'Security Analysis Notebook',

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
    onTitleInput: function(e) {
      this.title = e.target.textContent;

    },
    addComponent: function(componentName) {
      var id = this.id++;
      this.elements.push(
        {
          id: id,
          alias: "",
          type: componentName,
          hidden: false,
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
        title: this.title,
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
      this.title = doc.title;
      this.$refs.title.textContent = doc.title;
      for(var i = 0; i < doc.elements.length; i++) {
        this.elements.push ({
          id: doc.elements[i].id,
          alias: doc.elements[i].alias,
          type: doc.elements[i].type,
          hidden: false,
        });
        setTimeout((function() {
          self.elements[this.i].hidden = this.h;
        }).bind({i: this.elements.length-1,h: doc.elements[i].hidden}), 10);
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
      var filename = this.title.trim().replaceAll(' ', '-');
      dlAnchorElem.setAttribute("href",     dataStr     );
      dlAnchorElem.setAttribute("download", filename+".json");
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

