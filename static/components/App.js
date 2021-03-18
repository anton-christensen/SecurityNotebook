export default {
  el: '#app',
  template: `
    <div id="app" v-bind:class="{displayMode: !editMode}">
      <!-- hidden elements to trigger download and file dialog -->
      <a ref="downloadAnchor" style="display:none" download></a>
      <input ref="importFile" type="file" style="display:none"></input>

      <div class="context-menu add-component" v-bind:class="{displayNone: contextMenuHidden}" ref="componentAddContextMenu">
        <ul>
          <li v-for="type in componentAddMenuItems" v-on:click="addComponent(type.name)">
            {{ type.text }}
          </li>
        </ul>
      </div>

      <div class="app-panes-wrapper">
        <!-- to be turned into a menu component -->
        <FileExplorer ref="fileExplorer" v-on:file-open="openFileFromFileExplorer"></FileExplorer>

        <!-- main pane -->
        <div class="app-body">
          <div class="header">
            <div class="main-container">
              <div class="fl">
                <h1 contenteditable v-on:input="onTitleInput" ref="title">Security Analysis Notebook</h1>
              </div>
              <div v-if="filesystemSupported" class="fr hideInPrint" style="margin-top: 0.5rem">
                <button v-on:click="editMode = !editMode">{{editMode ? 'Display Mode' : 'Edit Mode'}}</button>
                <button v-on:click="newDocument">New</button>
                <button v-on:click="saveDocument" v-bind:disabled="filesystemMode == 'browser'">Save{{(documentChanged ? " *" : "")}}</button>
                <button v-on:click="saveDocumentAs">Save as</button>
                <button v-on:click="importDocument">Open</button>
              </div>
              <div v-else class="fr hideInPrint" style="margin-top: 0.5rem">
                <button v-on:click="editMode = !editMode">{{editMode ? 'Display Mode' : 'Edit Mode'}}</button>
                <button v-on:click="newDocument">New</button>
                <button v-on:click="importDocument">Open</button>
                <button v-on:click="exportDocument">Export</button>
              </div>
            </div>
          </div>

          <div class="main-container">
            <div id="notebookElements">
              <div class="notebook-component hideInPrintAndDisplay"><!-- for faking an extra border for the first child -->
                <div class="notebook-component-add shownOnHover" v-on:click="openContextMenuAdd($event, 0)" data-insertindex="0" style="right: initial; left: -24px"><ion-icon name="add-outline"></ion-icon></div>
              </div>

              <div v-bind:id="'cell-' + element.alias" class="notebook-component" v-on:drop="onDrop" v-on:dragover="onDragOver" v-on:dragleave="onDragLeave" v-for="(element, index) in elements" :key="element.id" v-bind:data-id="element.id">
                <div class="notebook-shoulder-padding leftof"></div>
                <div class="notebook-shoulder-padding rightof"></div>
                <div class="notebook-component-header hideInPrintAndDisplay">
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
                    <div class="notebook-component-add shownOnHover after" v-on:click="openContextMenuAdd($event, index+1)" v-bind:data-insertindex="index+1"><ion-icon name="add-outline"></ion-icon></div>
                  </div>
                  <div class="notebook-component-shoulder rightof">
                    <span class="notebook-component-remove shownOnHover" v-on:click="deleteComponent(element.id)">
                      <ion-icon name="close-outline"></ion-icon>
                    </span>
                  </div>
                  
                  <span class="name">#{{element.id}} - </span>
                  <input class="alias minimalistInput" v-model="element.alias" placeholder="Alias" v-bind:disabled="element.locked"/>
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

          </div><!-- main-container -->
        </div><!-- app-body -->
      </div><!-- flex -->
    </div><!-- app -->
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
    editMode: true,

    dragginID: -1,

    documentChanged: false,
    filesystemMode: "browser", // this or "disk" 
    filesystemSupported: false,
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
        editMode: this.editMode,
        elements: JSON.parse(JSON.stringify(this.elements)),
      };
      
      for(var i = 0; i < doc.elements.length; i++) {
        if(this.$refs[doc.elements[i].id]) {
          var component = this.$refs[doc.elements[i].id][0];
          if(component.getState) {
            doc.elements[i].state = component.getState();
          }
        }
      }
      return doc;
    },
    setDocument: function(doc) {
      var self = this;
      this.elements = [];
      
      this.id = doc.nextID;
      this.title = doc.title;
      this.editMode = doc.editMode == undefined ? true : doc.editMode;
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
      
      // wait till components are created, then set their respective states
      setTimeout(function() {
        for(var i = 0; i < doc.elements.length; i++) {
          self.$refs[doc.elements[i].id][0].setState(doc.elements[i].state);
        }
      }, 1);
      
      // Prevent from scrolling to middle of page after load
      document.getElementById('app').classList.add('preventScroll');
      setTimeout(function() {
        document.getElementById('app').classList.remove('preventScroll');
      }, 0);
    },
    newDocument: function() {
      if(confirm("You have unsaved changes!\nDo you want to continue without saving?")) {
        localStorage.setItem('doc', "");
        this.$refs.fileExplorer.deselect();
        this.contextMenuHidden = true;
        this.id = 0;
        this.title = 'Security Analysis Notebook';
        this.editMode = true;
        this.$refs.title.textContent = "Security Analysis Notebook";
        this.elements = [];
        this.documentChanged = false;
        this.filesystemMode = "browser";
      }
    },
    saveDocument: function() {
      if(this.filesystemMode == "browser") {
        localStorage.setItem('doc', JSON.stringify(this.getDocument()));
      }
      else if(this.filesystemMode == "disk") {
        var doc = JSON.stringify(this.getDocument());
        localStorage.setItem('lastfiledoc', doc);
        this.documentChanged = false;
        this.$refs.fileExplorer.saveDoc(doc);
      }
      else {
        console.log("unknown filesystem mode!");
      }
    },
    saveDocumentAs: function() {
      var doc = JSON.stringify(this.getDocument());
      this.$refs.fileExplorer.saveDocAs(doc);
    },
    openFileFromFileExplorer: function(doc) {
      if(this.elements.length > 0 && this.documentChanged && !confirm("You have unsaved changes!\nDo you want to continue without saving?")) {
        return;
      }
      else {
        this.setDocument(doc);
        this.filesystemMode = "disk";
        localStorage.setItem('lastfiledoc', JSON.stringify(doc));
        localStorage.setItem('doc', "");
      }
    },
    loadDocument: function() {
      this.filesystemMode = "browser";
      var doc = JSON.parse(localStorage.getItem('doc'));
      this.setDocument(doc);
      this.$refs.fileExplorer.deselect();
    },
    exportDocument: function() {
      var dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(this.getDocument()));
      var dlAnchorElem = this.$refs.downloadAnchor;
      var filename = this.title.trim().replaceAll(' ', '-');
      dlAnchorElem.setAttribute("href",     dataStr     );
      dlAnchorElem.setAttribute("download", filename+".sec.json");
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
    checkChanged: function() {
      // check if document is different from saved document
      if(this.filesystemMode == "browser") {
        this.saveDocument();
        this.documentChanged = false;
      }
      else {
        var savedDoc = localStorage.getItem("lastfiledoc");
        var currentDoc = JSON.stringify(this.getDocument());
        this.documentChanged = savedDoc != currentDoc && this.elements.length > 0;
      }
    }
  },
  mounted: function() {
    var self = this;
    this.filesystemSupported = this.$refs.fileExplorer.supported;

    // Hide add component menu if click outside it
    window.addEventListener('click', function(e) {
      if(self.contextMenuHidden == false && !self.$refs.componentAddContextMenu.contains(e.target)) {
        self.contextMenuHidden = true;
      }
    });

    if(localStorage.getItem("doc")) this.loadDocument();
    setInterval(this.checkChanged, 1500); // do so every 1.5seconds
    this.checkChanged();

    // Keyboard shortcut handlers
    document.addEventListener('keydown', function(e) {
      if (e.key === "s" && (e.ctrlKey) && self.filesystemSupported) {
        e.preventDefault();
        self.saveDocument();
        self.checkChanged();
      }
      if (e.key === "S" && e.shiftKey && e.ctrlKey && self.filesystemSupported) {
        e.preventDefault();
        self.saveDocumentAs();
      }
      if (e.key === "o" && e.ctrlKey) {
        e.preventDefault(); 
        self.importDocument();
      }
      if (e.key === "n" && e.ctrlKey) {
        e.preventDefault(); 
        self.newDocument();
      }
      if (e.key === "e" && e.ctrlKey && !self.filesystemSupported) {
        e.preventDefault();
        self.exportDocument();
      }
    });
  },
};
