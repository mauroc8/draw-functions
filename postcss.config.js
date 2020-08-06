
const tailwind = require('tailwindcss')
const purgecss = require('@fullhuman/postcss-purgecss')

const plugins = [ tailwind ]

const is_dev = process.env.NODE_ENV === 'development'

if (!is_dev)
{

}

module.exports = { plugins }
