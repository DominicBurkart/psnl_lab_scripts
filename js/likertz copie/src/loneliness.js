// @flow
import React from "react"
import lonelinessItems from "./loneliness-items.json"
import LikertQuestionnaire from "./likert-questionnaire"

const options = ["Never", "Rarely", "Sometimes", "Always"]

export default function Loneliness(props: {endTask: (string, string) => void}) {
  window.scrollTo(0,0)
  return (
    <LikertQuestionnaire items={lonelinessItems}
                         taskName={"lonely"}
                         options={options}
                         page1Prompt={"The following statements describe how people sometimes feel." +
                         " For each statement, please indicate how often you feel the way described.\n"}
                         continuedPrompt={"The following statements describe how people sometimes feel" +
                         " For each statement, please indicate how often you feel the way described.\n"}
                         itemsPerPage={4}
                         endTask={props.endTask}
    />
  )
}

