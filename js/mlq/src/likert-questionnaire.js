// @flow
import React from "react"
import {styled} from "styletron-react"
import LikertScale from "./likert-scale"
import {GlobalButton, globalColors} from "./globals"

const MainPrompt = styled('div', {
  margin: "2em",
  textAlign: "center",
  maxWidth: "700px"
})

type likertQProps = {
  endTask: (string, Object) => void,
  taskName: string,
  options: Array<string>,
  items: Array<string>,
  itemsPerPage: number,
  page1Prompt: string,
  continuedPrompt: string,
  RT: boolean
}
type likertQState = {
  pageIndex: number,
  highlightItems: Array<string>
}

class LikertQuestionnaire extends React.Component<likertQProps, likertQState> {
  selectOption: (string, Object) => void
  pages: Array<*>
  data: Object
  nextPage: () => void
  
  static defaultProps = {
    RT: false
  }

  constructor(props: likertQProps) {
    super(props)
    this.selectOption = this.selectOption.bind(this)
    this.nextPage = this.nextPage.bind(this)

    this.state = {
      pageIndex: 0,
      highlightItems: [],
    }

    const keyedItems = this.props.items.reduce((obj, cur, i) =>{ return {
      ...obj, [this.props.taskName + '_' + (i+1).toString()]: cur}}, {})

    // make some pages with 4 items each
    this.pages = []
    let index = 0
    for (let key in keyedItems) {
      if (keyedItems.hasOwnProperty(key)) {
        // create an empty array in the current pages if it doesn't exist yet (for 1st page)
        if (!this.pages[index]) {
          this.pages[index] = []
        // move on to a new page if we're at items per page on the current one
        } else if (this.pages[index].length === this.props.itemsPerPage) {
          index = index + 1
          this.pages[index] = []
        }
        this.pages[index].push(
           <LikertScale key={key}
                        name={key}
                        options={this.props.options}
                        prompt={keyedItems[key]}
                        returnSelected={this.selectOption}
                        RT={this.props.RT}
           />
          )
      }
    }
    this.data = {}
    window.scrollTo(0,0)
  }

  selectOption(item: string, value: Object) {
    this.data[item] = value
  }

  nextPage() {
    let itemsOnCurrentPage = this.pages[this.state.pageIndex].map(value => value.props.name)
    if (itemsOnCurrentPage.every((element) => {return (element in this.data)})) {
      if (this.state.pageIndex !== this.pages.length - 1) {
        this.setState((prevState) => {
          return {
            pageIndex: prevState.pageIndex + 1
          }
        })
        window.scrollTo(0,0)
      } else {
        this.props.endTask(this.props.taskName, this.data)
      }
    }
    let unansweredItems = itemsOnCurrentPage.filter((element) => {return ! (element in this.data) })
    this.setState({highlightItems: unansweredItems})
  }

  render() {
    return (
      <div style={{display: "flex", flexDirection: "column", alignItems: "center", height: "100%", maxWidth: "700px", margin: "0 auto"}}>
        <MainPrompt>
          {this.state.pageIndex === 0 ? (
            this.props.page1Prompt
          ):(
            this.props.continuedPrompt
          )}

        </MainPrompt>
        {this.pages[this.state.pageIndex].map((item) => {return (
          <div key={item.props.name}
               style={(this.state.highlightItems.indexOf(item.props.name) >= 0) ? {
                 border: "medium solid " + globalColors.emphasisColor,
                 width: "100%"} : {}}
          >
            {item}
          </div>)})}
          <div style={{alignSelf: "flex-end",
                       display: "flex",
                       alignItems: "center",
                       width: "100%",
                       justifyContent: "space-between"}}>
            <div style={{flex: "0 0 150px"}}/>
            <div style={{color: globalColors.emphasisColor}}>
              {this.state.highlightItems.length > 0 ? "Please complete missing items" : null}
            </div>
            <GlobalButton onClick={this.nextPage}
                          buttonText={(this.state.pageIndex === this.pages.length - 1) ? (
                            "Submit"
                          ):(
                            "Next Page")}/>
          </div>
      </div>
        )
  }
}

export default LikertQuestionnaire