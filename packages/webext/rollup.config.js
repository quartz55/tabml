import resolve from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";
import postcss from "rollup-plugin-postcss";
import replace from "@rollup/plugin-replace";

import {
  chromeExtension,
  simpleReloader
} from "rollup-plugin-chrome-extension";
import { emptyDir } from "rollup-plugin-empty-dir";

export default {
  input: "manifest.json",
  output: {
    dir: "dist",
    format: "esm"
  },
  plugins: [
    // emptyDir(),
    chromeExtension(),
    // simpleReloader(),
    replace({
      "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV)
    }),
    resolve(),
    commonjs(),
    postcss({})
  ]
};
