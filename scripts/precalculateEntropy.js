const { valid_words } = require("../output/Words")
const { calculate_entropy } = require("../output/Logic")

console.time()
JSON.stringify(calculate_entropy(valid_words))
console.timeEnd()
