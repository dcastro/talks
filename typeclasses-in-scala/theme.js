import { future } from 'mdx-deck/themes'
import vsDark from "prism-react-renderer/themes/vsDark"

// By default, `mdx-deck` uses `react-syntax-highlighter` for code highlighting, which supports Scala out of the box:
// https://yduman.github.io/blog/mdx-deck/
// https://github.com/jxnblk/mdx-deck/blob/master/docs/theming.md#syntax-highlighting
// https://github.com/conorhastings/react-syntax-highlighter/blob/master/AVAILABLE_LANGUAGES_PRISM.MD
//
// However, we're using code-surfer, which uses `prism-react-renderer` instead, which does not support Scala.
// But since Prism.js itself *does* support Scala, we can hack it like this:
// https://github.com/FormidableLabs/prism-react-renderer/issues/22
import Prism from 'prism-react-renderer/prism';
(typeof global !== 'undefined' ? global : window).Prism = Prism;
require('prismjs/components/prism-scala');

export default {
  ...future,

  codeSurfer: {
    ...vsDark,
    showNumbers: false
  },
}

