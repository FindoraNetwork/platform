#!/bin/bash
WASM_DIR="/build/wasm-js-bindings"

cd ./src/components/wasm || exit 1
wasm-pack build --target nodejs --out-dir "${WASM_DIR}/nodejs"
rm "${WASM_DIR}/nodejs/.gitignore"
wasm-pack build --target web --out-dir "${WASM_DIR}/web"
rm "${WASM_DIR}/web/.gitignore"
wasm-pack build --target bundler --out-dir "${WASM_DIR}/bundler"
rm "${WASM_DIR}/bundler/.gitignore"
