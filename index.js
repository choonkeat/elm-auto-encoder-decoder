#!/usr/bin/env node

const fs = require('fs')
const path = require('path')

global.XMLHttpRequest = require('xhr2')
const { Elm } = require('./build/Main.js')
const app = Elm.Main.init({ flags: {} })

var throttle = {}

app.ports.writeFile.subscribe(({ filename, encoding, data }) => {
  fs.mkdir(path.dirname(filename), { recursive: true }, function (err) {
    if (err) return app.ports.onFileWritten.send({ filename, encoding, err })

    fs.writeFile(filename, data, { encoding: encoding }, (err) => {
      app.ports.onFileWritten.send({ filename, encoding, err })
    })
  })
})

process.argv.slice(2).forEach((filepath) => {
  var encoding = 'utf8'
  fs.stat(filepath, function (err, stats) {
    if (err) throw err
    console.log('watching', filepath, '...')
    var dirname = stats.isDirectory() ? filepath : path.dirname(filepath)
    fs.watch(filepath, { recursive: true }, (eventType, basename) => {
      var filename = path.join(dirname, basename)
      if (throttle[filename]) return

      console.log(eventType, filename)
      throttle[filename] = (new Date()).getTime()
      fs.readFile(filename, { encoding: encoding }, (err, data) => {
        app.ports.onFileContent.send({ filename, encoding, err, data })
        setTimeout(function () { delete throttle[filename] }, 2000) // ignoring file changes for next 2 seconds
      })
    })
  })
})
