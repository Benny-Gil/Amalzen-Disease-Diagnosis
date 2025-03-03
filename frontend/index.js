const express = require('express')
const path = require('path')
const controller = require('./controller.js')

const app = express()

const publicDir = path.join(__dirname,'public')


app.use('/css',express.static(path.join(publicDir, 'css')))
app.use('/imgs', express.static(path.join(publicDir, 'imgs')))
app.use('/js', express.static(path.join(publicDir, 'js')))

app.use(express.json())

app.get('/health', (req, res) => {
    res.send('OK')
}
)

app.get('/', controller(publicDir))

app.get('/symptom_selection', (req,res)=>{
    res.sendFile(path.join(publicDir,'views','symptoms_selection.html'))
})

app.get('/possible_diseases', (req,res)=>{
    res.sendFile(path.join(publicDir,'views','possible_diseases.html'))
})

app.get('/symptom_details', (req,res)=>{
    res.sendFile(path.join(publicDir,'views','symptom_details.html'))
})


app.listen(8080, () => {
    console.log("Running on port 8080")
})