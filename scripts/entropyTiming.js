const { valid_words } = require("../output/Words")
const { calculate_entropy } = require("../output/Logic")

const time = n => {
  const label = `Length ${n}`
  console.time(label)
  calculate_entropy(valid_words.slice(0, n))
  console.timeEnd(label)
}

const lengths = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

for (n of lengths) {
  time(n)
}

/*
Output:

Length: 10: 70.959ms
Length: 20: 151.741ms
Length: 30: 148.412ms
Length: 40: 285.636ms
Length: 50: 548.096ms
Length: 60: 814.596ms
Length: 70: 1.157s
Length: 80: 1.665s
Length: 90: 2.569s
Length: 100: 3.823s
 */
