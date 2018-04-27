// @flow
import React from "react"
import lonelinessItems from "./loneliness-items.json"
import LikertQuestionnaire from "./likert-questionnaire"

const options = ["Absolutely Untrue", "Mostly Untrue", "Somewhat Untrue", "Can't Say True or False", "Somewhat True", "Mostly True", "Absolutely True"]

const prompt = `Please take a moment to think about what makes your life and existence feel important and significant to you.  Please respond to the following statements as truthfully and accurately as you can, and also please remember that these are very subjective questions and that there are no right or wrong answers.\n`

export default function Loneliness(props: {endTask: (string, string) => void}) {
  window.scrollTo(0,0)
  return (
    <LikertQuestionnaire items={lonelinessItems}
                         taskName={"lonely"}
                         options={options}
                         page1Prompt={prompt}
                         continuedPrompt={prompt}
                         itemsPerPage={4}
                         endTask={props.endTask}
    />
  )
}
