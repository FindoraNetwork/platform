sed -E -i '' '/\s*\* @param {.+} ([a-z]|[A-Z]|_|[0-9])+\s*$/d' pkg/wasm.js
jsdoc -c conf.json pkg/wasm.js
