
import App from './components/App.js';

import FileExplorer from './components/view/FileExplorer.js';

import TextInput from './components/cell/TextInput.js';
import AnalysisOutput from './components/cell/AnalysisOutput.js';
import LatticeOutput from './components/cell/LatticeOutput.js';
import MarkdownOutput from './components/cell/MarkdownOutput.js';
import GraphvizOutput from './components/cell/GraphvizOutput.js';
import InterpreterOutput from './components/cell/InterpreterOutput.js';

import GraphvizDisplay from './components/generic/GraphvizDisplay.js';
import MarkdownDisplay from './components/generic/MarkdownDisplay.js';
import CodeInput from './components/generic/CodeInput.js';
import ComponentRef from './components/generic/ComponentRef.js';


Vue.component('FileExplorer', FileExplorer);

// Cell components
Vue.component('TextInput', TextInput);
Vue.component('AnalysisOutput', AnalysisOutput);
Vue.component('LatticeOutput', LatticeOutput);
Vue.component('MarkdownOutput', MarkdownOutput);
Vue.component('GraphvizOutput', GraphvizOutput);
Vue.component('InterpreterOutput', InterpreterOutput);

// Generic components
Vue.component('GraphvizDisplay', GraphvizDisplay);
Vue.component('MarkdownDisplay', MarkdownDisplay);
Vue.component('CodeInput', CodeInput);
Vue.component('ComponentRef', ComponentRef);

// Plugs code highlighting into markdown pipeline
marked.setOptions({
  langPrefix: "",
  highlight: function(code, lang) {
    if(lang == "plaintext")
      return code;
    if(hljs.listLanguages().indexOf(lang) == -1)
      return hljs.highlightAuto(code).value;
    return hljs.highlightAuto(code,[lang]).value;
  }
});

// plugs latex math rendering into markdown pipeline
const markdownLatexRenderer = {
  code(code, lang) {
    if(lang == "katex") {
      return katex.renderToString(code, {throwOnError: false, output: "html", displayMode: true});
    }
    if(lang == "cellRef") {
      var {alias,fstln,lastln} = code;
      if(fstln)
        return `<pre><code cellref="${alias}" startline="${fstln}" endline="${lastln}"></code></pre>`;
      else 
        return `<pre><code cellref="${alias}"></code></pre>`;
    }
    return false;
  },

  codespan(code) {
    if(code.startsWith("§KaTeXInLiNe§")) {
      code = code.substr("§KaTeXInLiNe§".length);
      return katex.renderToString(code, {throwOnError: false, output: "html", displayMode: false});
    }
    else {
      return false;
    }
  },
}
const markdownLatexTokenizer = {
  fences(src) { // ``` backticked code block ```
    const cellRef = /^\@([^\[]+)(\[(\d+):(\d+)\])?\n/.exec(src);
    const latexMath = /^ {0,3}(\${2})(?:|([\s\S]*?))(?: {0,3}\1 *(?:\n+|$)|$)/.exec(src);
    if (cellRef) {
      return {
        type: 'code',
        lang: 'cellRef',
        raw: cellRef[0],
        text: {alias: cellRef[1].trim(), fstln: cellRef[3], lastln: cellRef[4]},
      }
    }
    if (latexMath) {
      return {
        type: 'code',
        lang: 'katex',
        raw: latexMath[0],
        text: latexMath[2]
      };
    }
    // return false to use original tokenizer
    return false;
  },
  codespan(src) {
    const cap = /^(\${1,2})([^\$]|[^\$][\s\S]*?[^\$])\1(?!\$)/.exec(src);
    if (cap) {
      return {
        type: 'codespan',
        raw: cap[0],
        text: "§KaTeXInLiNe§"+cap[2]
      };
    }
    return false;
  },
  inlineText(src, inRawBlock, smartypants) {
    const cap = /^(`+|[^`])(?:(?= {2,}\n)|[\s\S]*?(?:(?=[\$\\<!\[`*_]|\b_|$)|[^ ](?= {2,}\n)))/.exec(src);
    if (cap) {
      let text;
      text = cap[0];
      return {
        type: 'text',
        raw: cap[0],
        text
      };
    }
    return false;
  }
};
marked.use({ tokenizer: markdownLatexTokenizer, renderer: markdownLatexRenderer });



// Handle image paste in codemirror field
document.onpaste = function(event){
  // only if active element is a descendant of a codemirror element
  if(document.activeElement.closest('.CodeMirror') == null)
    return;

  var items = (event.clipboardData || event.originalEvent.clipboardData).items;
  for (var index in items) {
    var item = items[index];
    if (item.kind === 'file') {
      var blob = item.getAsFile();
      var reader = new FileReader();
      reader.onload = function(event) {
        var dataURL = event.target.result;
        if(!dataURL.startsWith('data:image/')) {
          console.log("pasted non-image file: ", dataURL);
          return;
        }
        var markdownValue = "![altText]("+dataURL+")\n";
        var inputElm = document.activeElement;
        // insert at current cursor position
        //IE support
        if (document.selection) {
          inputElm.focus();
          sel = document.selection.createRange();
          sel.text = markdownValue;
        }
        //MOZILLA and others
        else if (inputElm.selectionStart || inputElm.selectionStart == '0') {
          var startPos = inputElm.selectionStart;
          var endPos = inputElm.selectionEnd;
          inputElm.value = inputElm.value.substring(0, startPos)
              + markdownValue
              + inputElm.value.substring(endPos, inputElm.value.length);
        } else {
          inputElm.value += markdownValue;
        }
        
      };
      reader.readAsDataURL(blob);
    } 
  }
}

const app = new Vue(App);
