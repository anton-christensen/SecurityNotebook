
import App from './components/App.js';

import TextInput from './components/TextInput.js';
import TextOutput from './components/TextOutput.js';
import AnalysisOutput from './components/AnalysisOutput.js';
import LatticeOutput from './components/LatticeOutput.js';
import MarkdownOutput from './components/MarkdownOutput.js';


import GraphvizDisplay from './components/generic/GraphvizDisplay.js';
import MarkdownDisplay from './components/generic/MarkdownDisplay.js';
import CodeInput from './components/generic/CodeInput.js';
import ComponentRef from './components/generic/ComponentRef.js';


Vue.component('TextInput', TextInput);
Vue.component('TextOutput', TextOutput);
Vue.component('AnalysisOutput', AnalysisOutput);
Vue.component('LatticeOutput', LatticeOutput);
Vue.component('MarkdownOutput', MarkdownOutput);

Vue.component('GraphvizDisplay', GraphvizDisplay);
Vue.component('MarkdownDisplay', MarkdownDisplay);
Vue.component('CodeInput', CodeInput);
Vue.component('ComponentRef', ComponentRef);

// Vue.use(hljs.vuePlugin);

marked.setOptions({
    langPrefix: "",
    highlight: function(code, lang) {
        if(lang == "plaintext")
            return code;
        if(hljs.listLanguages().indexOf(lang) == -1)
            return hljs.highlightAuto(code).value;
        return hljs.highlightAuto(code,[lang]).value;
        
    }
})

const app = new Vue(App);
