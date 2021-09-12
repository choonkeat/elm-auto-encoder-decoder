#!/usr/bin/env node

const fs = require('fs')
const path = require('path')

global.XMLHttpRequest = require('xhr2')
const { Elm } = require('./build/Main.js')
const app = Elm.Main.init({ flags: {} })
const watching = (process.env.WATCHING !== 'false')
// optional configs
const extraImport = process.env.EXTRA_IMPORT
const generatedSrc = process.env.GENERATED_SRC

var throttle = {}

if (app.ports.exit && !watching) app.ports.exit.subscribe(process.exit)

if (app.ports.writeFile) {
  app.ports.writeFile.subscribe(({ filename, encoding, data }) => {
    if (generatedSrc) {
      filename = path.join(generatedSrc, innerPath(filename, []) + path.extname(filename))
    }
    fs.mkdir(path.dirname(filename), { recursive: true }, function (err) {
      if (err) return app.ports.onFileWritten.send({ filename, encoding, err })
      fs.writeFile(filename, data, { encoding: encoding }, (err) => {
        app.ports.onFileWritten.send({ filename, encoding, err })
      })
    })
  })
}

function readAndWrite (filename, encoding, autoModules) {
  throttle[filename] = (new Date()).getTime()                    // lock to throttle
  fs.readFile(filename, { encoding: encoding }, (err, data) => {
    app.ports.onFileContent.send({ filename, encoding, extraImport, autoModules, err, data })
    setTimeout(function () { delete throttle[filename] }, 2000)  // remove throttle after 2 seconds
  })
}

function innerPath(filepath, resultpath) {
  const parts = path.parse(filepath)
  if ((!parts.name) || parts.name === 'src' || parts.name.endsWith('-src')) {
    return resultpath.join(path.sep)
  }
  return innerPath(parts.dir, [parts.name, ...resultpath])
}

const autoModules = process.argv.slice(2).map((filepath) => innerPath(filepath, []).split(path.sep).join('.'))

process.argv.slice(2).forEach((filepath) => {
  var encoding = 'utf8'
  fs.stat(filepath, function (err, stats) {
    if (err) throw err
    readAndWrite(filepath, 'utf8', autoModules)
    if (!watching) return // the end

    console.log('watching', filepath, '...')
    var dirname = stats.isDirectory() ? filepath : path.dirname(filepath)
    fs.watch(filepath, { recursive: true }, (eventType, basename) => {
      var filename = path.join(dirname, basename)
      if (throttle[filename]) return
      readAndWrite(filename, encoding, autoModules)
    })
  })
})
