#!/usr/bin/env node

var fs = require('fs');

var arch = process.arch;
var platform = process.platform;

if (arch === 'ia32') {
  arch = 'x86';
}

if (platform === 'win32') {
  platform = 'win';
}

copyBinary('ppx-properties-' + platform + '-' + arch + '.exe', 'ppx');

function copyBinary(filename, destFilename) {
  if (process.env.IS_PPX_PROPERTIES_UI_PPX_CI) {
    console.log(
      'ppx_properties: IS_PPX_PROPERTIES_UI_PPX_CI has been set, skipping moving binary in place',
    );
    process.exit(0);
  }

  var supported = fs.existsSync(filename);

  if (!supported) {
    console.error('Does not support this platform :' + platform + '/' + arch,);

    if (!process.env.IS_PPX_PROPERTIES_UI_PPX_CI) {
      process.exit(1);
    }
  }

  if (!fs.existsSync(destFilename)) {
    copyFileSync(filename, destFilename);
    fs.chmodSync(destFilename, 0755);
  }

  var destFilenameExe = destFilename;
  if (!fs.existsSync(destFilenameExe)) {
    copyFileSync(filename, destFilenameExe);
    fs.chmodSync(destFilenameExe, 0755);
  }
}

function copyFileSync(source, dest) {
  if (typeof fs.copyFileSync === 'function') {
    fs.copyFileSync(source, dest);
  } else {
    fs.writeFileSync(dest, fs.readFileSync(source));
  }
}