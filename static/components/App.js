

export default {
  el: '#app',
  template: `
    <div id="app">
      <!-- hidden elements to trigger download and file dialog -->
      <a ref="downloadAnchor" style="display:none"></a>
      <input ref="importFile" type="file" style="display:none"></input>
      <div class="context-menu add-component" v-bind:class="{displayNone: contextMenuHidden}" ref="componentAddContextMenu">
        <ul>
          <li v-for="type in componentAddMenuItems" v-on:click="addComponent(type.name)">
            {{ type.text }}
          </li>
        </ul>
      </div>

      <div class="header">
        <div class="main-container">
          <div class="fl">
            <h1 contenteditable v-on:input="onTitleInput" ref="title">Security Analysis Notebook</h1>
          </div>
          <div class="fr" style="margin-top: 0.5rem">
            <button v-on:click="saveDocument">Save</button>
            <button v-on:click="loadDocument">Load</button>
            <button v-on:click="exportDocument">Export</button>
            <button v-on:click="importDocument">Import</button>
          </div>
        </div>
      </div>

      <div class="main-container">
      
        <div id="notebookElements">
          <div class="notebook-component"><!-- for faking an extra border for the first child -->
            <div v-if="elements.length == 0" class="notebook-component-add pointer shownOnHover" v-on:click="openContextMenuAdd($event, 0)" data-insertindex="0" style="right: initial; left: -24px"><ion-icon name="add-outline"></ion-icon></div>
          </div>

          <div class="notebook-component" v-for="(element, index) in elements">
            <div class="notebook-shoulder-padding leftof"></div>
            <div class="notebook-shoulder-padding rightof"></div>
            <div class="notebook-component-header visibleOnComponentHover op100">
              <div class="notebook-component-shoulder leftof">
                <div class="notebook-drag-handle shownOnHover" draggable>
                  <!--<ion-icon name="reorder-two-outline"></ion-icon>-->
                </div>
                <div class="notebook-component-fold shownOnHover hideToggle" v-on:click="element.hidden = !element.hidden" v-bind:class="{hidden: element.hidden, shown: !element.hidden}">
                  <ion-icon class="hidden-icon" name="eye-off-outline"></ion-icon>
                  <ion-icon class="shown-icon" name="eye-outline"></ion-icon>
                </div>
                <div class="notebook-component-add shownOnHover before" v-on:click="openContextMenuAdd($event, index)" data-insertindex="{{index}}"><ion-icon name="add-outline"></ion-icon></i></div>
                <div class="notebook-component-add shownOnHover after" v-on:click="openContextMenuAdd($event, index+1)" data-insertindex="{{index+1}}"><ion-icon name="add-outline"></ion-icon></div>
              </div>
              <div class="notebook-component-shoulder rightof">
                <span class="notebook-component-remove shownOnHover" v-on:click="deleteComponent(element.id)">
                  <!--<i class="fas fa-trash"></i>-->
                  <ion-icon name="close-outline"></ion-icon>
                </span>
              </div>
              
              <span class="name">#{{element.id}} - </span>
              <input class="alias" v-model="element.alias" placeholder="Alias" />
              <span class="name fr">{{ element.type }}</span>
              
            </div><!-- notebook-component-header -->
            <div class="notebook-component-content" v-bind:class="{hide: element.hidden}">
              <component 
                :key="element.id" 
                :alias="element.alias"
                :ref="element.id" 
                :is="element.type"   
                v-bind:others="$refs"
              ></component>
            </div><!-- notebook-component-content -->

          </div><!-- notebook-component -->
        </div><!-- notebookElements --> 

      </div>
    </div>
  `,
  data: {
    componentTypes: [
      {name: "", text: "-- Select new component --"},
      {name: "text-input", text: "Text Input"},
      {name: "text-output", text: "Text Output"},
      {name: "markdown-output", text: "Markdown Output"},
      {name: "analysis-output", text: "Analysis Output"},
      {name: "lattice-output", text: "Lattice Output"},
    ],
    selected: '',

    componentAddMenuItems: [
      {name: "text-input", text: "Text Input"},
      {name: "text-output", text: "Text Output"},
      {name: "markdown-output", text: "Markdown Output"},
      {name: "analysis-output", text: "Analysis Output"},
      {name: "lattice-output", text: "Lattice Output"},
    ],
    contextMenuHidden: true,
    contextMenuInsertIndex: 0,

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

    openContextMenuAdd: function(e, insertIndex) {
      var self = this;
      let getAbsOffset = (target) => {
        if(target.offsetParent == null) {
          return {x: target.offsetTop, y: target.offsetLeft};
        }
        let offset = getAbsOffset(target.offsetParent);
        return {x: offset.x + target.offsetLeft, y: offset.y + target.offsetTop};
      }
      let offset = getAbsOffset(e.target);

      this.$refs.componentAddContextMenu.style.right = document.body.clientWidth - (offset.x - 10);
      this.$refs.componentAddContextMenu.style.top = (offset.y + (e.target.clientHeight/2));
      this.$refs.componentAddContextMenu.classList.remove('hidden');
      this.contextMenuInsertIndex = insertIndex;

      setTimeout(() => self.contextMenuHidden = false, 0); // wait for the other click event handler to execute
      
    },

    addComponent: function(componentName) {
      var id = this.id++;
      var newElement = {
          id: id,
          alias: "",
          type: componentName,
          hidden: false,
        };
      if(this.contextMenuHidden == false) {
        this.contextMenuHidden = true;
        this.elements.splice(this.contextMenuInsertIndex, 0, newElement);
      }
      else {
        this.elements.push(newElement);
      }
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
    window.addEventListener('click', function(e) {
      if(self.contextMenuHidden == false && !self.$refs.componentAddContextMenu.contains(e.target)) {
        self.contextMenuHidden = true;
      }
    });
  },
};

