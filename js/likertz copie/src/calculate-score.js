
export default function calculateScore(taskName, data) {

  // get regex for item prefix based on task name
  const rawItemRegEx = RegExp(taskName + "_")

  // add each item's value to new object
  let recodedData = {}
  Object.keys(data).map((item) => {
    if (rawItemRegEx.test(item)){
      recodedData[item.replace(rawItemRegEx, "")] = data[item].value
    }
  })
  // recode the items that need to be reverse coded
  const recodeItems = [1, 5, 6, 9, 10, 15, 16, 19, 20]
  recodeItems.map((item) => {
      const rawValue = data[taskName + '_' + item].value
      const recodedValue = 5 - rawValue

      recodedData[item] = recodedValue
    }
  )

  // create new object for summary values
  let lonelySummary = {plotdata: {low: 28.5, high: 46.58, average: 40.08}}
  const totalScore = Object.values(recodedData).reduce((a,b) => { return parseInt(a, 10) + parseInt(b, 10) })
  lonelySummary.plotdata.score = totalScore

  const fullResult = Object.assign({}, data, lonelySummary)
  return {[taskName]: fullResult}
}