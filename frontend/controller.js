const express = require('express')
const path = require('path')

function controller(publicDir){
    return (req,res) =>{
        res.sendFile((path.join(publicDir,"index.html")))
    }
}

module.exports = controller