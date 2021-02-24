

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
            <button v-on:click="saveDocument">Save{{(this.documentChanged ? " *" : "")}}</button>
            <button v-on:click="loadDocument">Load</button>
            <button v-on:click="exportDocument">Export</button>
            <button v-on:click="importDocument">Import</button>
          </div>
        </div>
      </div>

      <div class="main-container">
        <div id="notebookElements">
          <div class="notebook-component"><!-- for faking an extra border for the first child -->
            <div class="notebook-component-add shownOnHover" v-on:click="openContextMenuAdd($event, 0)" data-insertindex="0" style="right: initial; left: -24px"><ion-icon name="add-outline"></ion-icon></div>
          </div>

          <div class="notebook-component" v-on:drop="onDrop" v-on:dragover="onDragOver" v-on:dragleave="onDragLeave" v-for="(element, index) in elements" :key="element.id" v-bind:data-id="element.id">
            <div class="notebook-shoulder-padding leftof"></div>
            <div class="notebook-shoulder-padding rightof"></div>
            <div class="notebook-component-header">
              <div class="notebook-component-shoulder leftof">
                <div class="notebook-drag-handle shownOnHover" v-bind:data-id="element.id" draggable v-on:dragstart="onDragStart">
                  <ion-icon name="reorder-two-outline"></ion-icon>
                </div>
                <div class="notebook-lock-component toggle shownOnHover" v-on:click="element.locked = !element.locked" v-bind:class="{enabled: element.locked, disabled: !element.locked}">
                  <ion-icon class="enabled-icon" name="lock-closed-outline" style="color:#B5A642;"></ion-icon>
                  <ion-icon class="disabled-icon"  name="lock-open-outline"></ion-icon>
                </div>
                <div class="notebook-component-fold toggle hideToggle shownOnHover" v-on:click="element.hidden = !element.hidden" v-bind:class="{disabled: element.hidden, enabled: !element.hidden}">
                  <ion-icon class="enabled-icon"  name="eye-outline"></ion-icon>
                  <ion-icon class="disabled-icon" name="eye-off-outline"></ion-icon>
                </div>
                <div class="notebook-component-add shownOnHover after" v-on:click="openContextMenuAdd($event, index+1)" data-insertindex="{{index+1}}"><ion-icon name="add-outline"></ion-icon></div>
              </div>
              <div class="notebook-component-shoulder rightof">
                <span class="notebook-component-remove shownOnHover" v-on:click="deleteComponent(element.id)">
                  <ion-icon name="close-outline"></ion-icon>
                </span>
              </div>
              
              <span class="name">#{{element.id}} - </span>
              <input class="alias" v-model="element.alias" placeholder="Alias" v-bind:disabled="element.locked"/>
              <span class="name fr">{{ element.type }}</span>
              
            </div><!-- notebook-component-header -->
            <div class="notebook-component-content" v-bind:class="{hide: element.hidden}">
              <component 
                :key="element.id" 
                :alias="element.alias"
                :ref="element.id" 
                :is="element.type"
                v-bind:others="$refs"
                v-bind:self="element"
              ></component>
            </div><!-- notebook-component-content -->

          </div><!-- notebook-component -->
        </div><!-- notebookElements --> 

      </div>
    </div>
  `,
  data: {
    componentAddMenuItems: [
      {name: "text-input", text: "Text Input"},
      {name: "markdown-output", text: "Markdown Output"},
      {name: "analysis-output", text: "Analysis Output"},
      {name: "lattice-output", text: "Lattice Output"},
    ],
    contextMenuHidden: true,
    contextMenuInsertIndex: 0,

    id: 0,
    title: 'Security Analysis Notebook',

    elements: [],

    dragginID: -1,

    documentChanged: false
  },
  watch: {
    title: function(newVal) {
      document.title = (this.documentChanged ? "* " : "") + newVal;
    },
    documentChanged: function(newVal) {
      document.title = (newVal ? "* " : "") + this.title;
    }
  },
  methods: {
    onDragStart: function(e) {
      this.dragginID = e.target.getAttribute('data-id');
    },
    onDragOver: function(e) {
      e.preventDefault();
      let targetComponent = e.target.closest(".notebook-component");
      let rect = targetComponent.getBoundingClientRect();
      
      if(Math.abs(e.y-rect.top) < Math.abs(e.y-rect.bottom)) {
        targetComponent.classList.remove('paste-below');
        targetComponent.classList.add('paste-above');
      }
      else {
        targetComponent.classList.remove('paste-above');
        targetComponent.classList.add('paste-below');
      }
    },
    onDragLeave: function(e) {
      e.preventDefault();
      let targetComponent = e.target.closest(".notebook-component");
      targetComponent.classList.remove('paste-below');
      targetComponent.classList.remove('paste-above');
    },
    onDrop: function(e) {
      e.preventDefault();
      let targetComponent = e.target.closest(".notebook-component");
      let dropTargetID = targetComponent.getAttribute('data-id');
      let dropSourceID = this.dragginID;
      
      let source = this.getElementByID(dropSourceID);
      let target = this.getElementByID(dropTargetID);
      
      if(targetComponent.classList.contains('paste-above')) {
        this.elements.splice(target.index, 0, source.element);
        this.elements.splice(source.index + (target.index < source.index ? 1 : 0), 1);
      }
      else if(targetComponent.classList.contains('paste-below')) {
        this.elements.splice(target.index+1, 0, source.element);
        this.elements.splice(source.index + (target.index+1 < source.index ? 1 : 0), 1);
      }
      targetComponent.classList.remove('paste-above');
      targetComponent.classList.remove('paste-below');
    },

    getElementByID: function(id) {
      let index = this.elements.findIndex( (elm) => elm.id == id );
      return index == -1 ? null : {
        index,
        element: this.elements.find( (elm) => elm.id == id )
      }
    },

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
      this.$refs.componentAddContextMenu.style.right = (document.body.clientWidth - (offset.x - 10)) + "px" ;
      this.$refs.componentAddContextMenu.style.top = (offset.y + (e.target.clientHeight/2)) + "px";
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
          locked: false
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
      this.elements = this.elements.filter(elm => elm.id != id);
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
          locked: doc.elements[i].locked ? true : false,
          hidden: false
        });
        setTimeout((function() {
          self.elements[this.i].hidden = this.h ? true : false;
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

    // Hide add component menu if click outside it
    window.addEventListener('click', function(e) {
      if(self.contextMenuHidden == false && !self.$refs.componentAddContextMenu.contains(e.target)) {
        self.contextMenuHidden = true;
      }
    });

    // check if document is different from saved document
    let checkChanged = function() {
      var savedDoc = localStorage.getItem('doc');
      var currentDoc = JSON.stringify(self.getDocument());
      self.documentChanged = savedDoc != currentDoc;
    }
    setInterval(checkChanged, 1500);
    checkChanged();

    // Keyboard shortcut handlers
    document.addEventListener('keydown', function(e) {
      if (e.key === "s" && (e.ctrlKey)) { // save
        e.preventDefault(); // prevents "Save Page" from getting triggered.
        self.saveDocument();
        checkChanged();
      }
      if (e.key === "e" && (e.ctrlKey)) { // export
        e.preventDefault(); // prevents browser from handling the event
        self.exportDocument();
      }
      if (e.key === "i" && (e.ctrlKey)) { // import
        e.preventDefault(); // prevents browser from handling the event
        self.importDocument();
        checkChanged();
      }
      if (e.key === "l" && (e.ctrlKey)) { // load
        e.preventDefault(); // prevents browser from handling the event
        self.loadDocument();
        setTimeout(checkChanged, 100); // give document time to load
      }
    });
  },
};

