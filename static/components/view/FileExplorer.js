function FileExplorerFolder(handle, parent) {
  if(!(handle instanceof FileSystemDirectoryHandle)) {
    throw "Provided handle is not FileSystemDirectoryHandle";
  }
  this.type = "folder";
  this.parent = parent ? parent : null;
  this.handle = handle;
  this.path = (parent ? parent.path : '') + handle.name + "/";
  this.name = handle.name;
  this.open = parent ? false : true;
  this.content = [];
  var self = this;

  this.walk = function() {
    var oldContent = self.content;
    var newContent = [];
    var itt = handle.values();
    var entry = {done: false};
    
    let handleWalk = function(data) {
      if(data.done) {

        self.content = newContent.sort((a,b) => a.name.localeCompare(b.name)); 
        return; 
      }

      entry = data.value;
      if(entry instanceof FileSystemDirectoryHandle) {
        let child = new FileExplorerFolder(entry, self);
        let old = oldContent.find((e) => e.path == child.path);
        
        if(old) {
          if(old.open) old.walk();
          newContent.push(old);
        }
        else {
          newContent.push(child);
        }
      }
      else if(entry instanceof FileSystemFileHandle) {
        let child = new FileExplorerFile(entry, self);
        let old = oldContent.find((e) => e.path == child.path);
        
        if(old) {
          newContent.push(old);
        }
        else {
          newContent.push(child);
        }
      }

      itt.next().then(handleWalk);

    };

    itt.next().then(handleWalk);
  }
}
function FileExplorerFile(handle, parent) {
  if(!(handle instanceof FileSystemFileHandle)) {
    throw "Provided handle is not FileSystemFileHandle";
  }
  this.type = "file";
  this.parent = parent ? parent : null;
  this.handle = handle;
  this.path = (parent ? parent.path : '') + handle.name;
  this.name = handle.name;

  this.valid = this.name.endsWith(".sec.json");

  this.walk = function() {};
}

/*Make resizable div by Hung Nguyen*/
function makeResizableDiv(div) {
  const element = document.querySelector(div);
  const resizer = document.querySelector(div + ' .resizer')
  const minimum_width = 20;
  let original_width = 0;
  let original_x = 0;
  let original_mouse_x = 0;

  resizer.addEventListener('mousedown', function(e) {
    e.preventDefault()
    original_width = parseFloat(getComputedStyle(element, null).getPropertyValue('width').replace('px', ''));
    original_x = element.getBoundingClientRect().left;
    original_mouse_x = e.pageX;
    window.addEventListener('mousemove', resize)
    window.addEventListener('mouseup', stopResize)
  })
    
  function resize(e) {
    const width = original_width + (e.pageX - original_mouse_x);
    if (width >= minimum_width) {
      element.style.width = width + 'px'
    }
  }
  
  function stopResize() {
    window.removeEventListener('mousemove', resize)
  }
}

Vue.component('FolderEntry', {
  props: ['entry', 'selectedPath'],
  template: `
    <ul v-if="entry && entry.type == 'folder' && !entry.parent">
      <span style="text-decoration: underline">{{entry.name}}</span>
      <FolderEntry v-for="child in Object.values(entry.content)" :entry="child" v-bind:selectedPath="selectedPath" :key="child.path" v-on:file-open="bubbleFileOpened"></FolderEntry>
    </ul>
    
    <li v-else-if="entry && entry.type == 'folder'" v-on:click="toggleFolder">
      <span>
        <ion-icon name="folder-open-outline" v-if="entry.open"></ion-icon>
        <ion-icon name="folder-outline" v-else></ion-icon>
      {{entry.name}}/
      </span>
      <ul v-if="entry.open">
        <FolderEntry v-for="child in Object.values(entry.content)" v-if="child.type" :entry="child" :key="child.path" v-on:file-open="bubbleFileOpened"></FolderEntry>
      </ul>
    </li>

    <ul v-else-if="entry && entry.type == 'file' && !entry.parent">
      <li v-on:click="openFile" v-bind:class="{open: entry.path == selectedPath}">
        <span>
          <ion-icon v-if="entry.valid" name="reader-outline"></ion-icon>
          <ion-icon v-else name="document-outline"></ion-icon>
          {{entry.name}}
        </span>
      </li>
    </ul>
    
    <li v-else-if="entry && entry.type == 'file'" v-on:click="openFile" v-bind:class="{open: entry.path == selectedPath}">
      <span>
        <ion-icon v-if="entry.valid" name="reader-outline"></ion-icon>
        <ion-icon v-else name="document-outline"></ion-icon>
        {{entry.name}}
      </span>
    </li>
  `,
  methods: {
    toggleFolder: function(e) {
      e.stopPropagation();
      this.entry.open = !this.entry.open;
      if(this.entry.open) this.entry.walk();
    },
    bubbleFileOpened: function(e) {
      this.$emit('file-open', e);
    },
    openFile: function(e) {
      e.stopPropagation();
      this.entry.handle.getFile().then(function(f) {
        return f.text();
      }).then((txt) => {
        try {
          let data = JSON.parse(txt);
          this.entry.valid = true;
          this.$emit('file-open', {fileHandle: this.entry.handle, path: this.entry.path, doc: data});
        }
        catch(e) {
          this.entry.invalid = true;
        }
      });

    },
  },
});

export default {
    template: `
      <div class="menu hideInPrint">
        <div class="menu-list">
          <div v-if="supported" class="menu-toggle shownOnHover" v-on:click="toggleMenu('file-explorer')" v-bind:class="{open: openMenu == 'file-explorer'}">
            <ion-icon name="documents-outline"></ion-icon>
            <!-- <ion-icon name="folder-open-outline"></ion-icon> -->
          </div>
        </div><!-- menu-list -->

        <div class="menu-expanded file-explorer" v-show="openMenu == 'file-explorer'">
          <div class='resizer'></div>

          <button v-on:click="selectFolder">Open folder</button>
          <hr />

          <FolderEntry v-if="directoryRoot" v-bind:selectedPath="openedPath" v-bind:entry="directoryRoot" v-on:file-open="bubbleFileOpened"></FolderEntry>
        </div><!-- menu-expanded -->
      </div><!-- file-explorer -->
    `,
    props: [],
    data: () => Object({
      supported: !!window.showDirectoryPicker,
      directoryRoot: null,
      openedPath: "",
      openedFileHandle: null,
      openMenu: "",
    }),
    methods: {
      toggleMenu: function(menu) {
        this.openMenu = this.openMenu == menu ? "" : menu;
        localStorage.setItem('menu.openMenu', this.openMenu);
      },
      deselect: function() {
        this.openedPath = "";
        this.openedFileHandle = null;
      },
      saveDocAs: async function(str) {
        let fh = await window.showSaveFilePicker(
          {
            types: [{
              description: 'Security Notebook',
              accept: {'application/json': ['.sec.json']},
            }],
          }
        );
        if(fh) {
          if(this.directoryRoot == null) {
            this.directoryRoot = new FileExplorerFile(fh);
            this.directoryRoot.valid = true;
            this.openedPath = fh.path;
            this.openedFileHandle = fh;
            this.$emit('file-open', JSON.parse(str));
          } 
          fh.createWritable().then(async (writable) => {
            await writable.write(str);
            await writable.close();
          });
        }
      },
      saveDoc: async function(str) {
        if(this.openedFileHandle) {
          this.openedFileHandle.createWritable().then(async (writable) => {
            await writable.write(str);
            await writable.close();
          });
        }
        else {
          console.log("Failed to save to disk: no valid filehandle selected");
        }
      },
      bubbleFileOpened: function(e) {
        this.openedPath = e.path;
        this.openedFileHandle = e.fileHandle;
        this.$emit('file-open', e.doc);
      },
      selectFolder: function() {
        var self = this;
        window.showDirectoryPicker().then((dir) => {
          dir.requestPermission({mode: "readwrite"});
          self.directoryRoot = new FileExplorerFolder(dir);
          self.directoryRoot.walk();
        }).catch((err) => {
          console.log("Failed to open dir");
          console.log(err);
          self.directoryRoot = null;
        });
      },
    },
    mounted: function() {
      var self = this;
      this.openMenu = localStorage.getItem('menu.openMenu');

      let checkChanged = function() {
        if(self.directoryRoot) self.directoryRoot.walk();
      }
      setInterval(checkChanged, 3000); // do so every 1.5seconds

      makeResizableDiv('.file-explorer');
    }
  }
    