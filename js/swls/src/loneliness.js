// @flow
import React from "react"
import lonelinessItems from "./loneliness-items.json"
import LikertQuestionnaire from "./likert-questionnaire"

const options = ["strongly disagree", "disagree", "slightly disagree", "neither agree nor disagree", "slightly agree", "agree", "strongly agree"]

const prompt = `Below are five statements with which you may agree or disagree. Be open and honest in your responding.`

export default function Loneliness(props: {endTask: (string, string) => void}) {
  window.scrollTo(0,0)
  return (
    <LikertQuestionnaire items={lonelinessItems}
                         taskName={"lonely"}
                         options={options}
                         page1Prompt={prompt}
                         continuedPrompt={prompt}
                         itemsPerPage={5}
                         endTask={props.endTask}
    />
  )
}
