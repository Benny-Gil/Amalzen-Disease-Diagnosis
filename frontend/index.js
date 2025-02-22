const express = require('express')
const path = require('path')
const controller = require('./controller.js')

const app = express()

const publicDir = path.join(__dirname,'public')

app.use('/css',express.static(path.join(publicDir, 'css')))
app.use('/imgs', express.static(path.join(publicDir, 'imgs')))
app.use('/js', express.static(path.join(publicDir, 'js')))

app.use(express.json())


app.get('/', controller(publicDir))

app.listen(8080, () => {
    console.log("Running on port 8080")
})