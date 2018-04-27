// @flow
import * as React from "react"
// ***** set global color scheme ***** //
/**
 * Use this color scheme throughout app (add inline style in react, not CSS)
 */
const globalColors: {[string]: string} = {
  mainColor: '#5074C2',
  emphasisColor: '#F3B022',
  darkColor: '#262D46',
  lightColor: '#F2F4F3',
  otherColor: '#84717F'
}

// ***** define basic button **** //
// Styles for regular button, emphasis button, both with hover option
const buttonStyles = {
  buttonBase: {
    borderRadius: "4px",
    border: "none",
    margin: "1em",
    height: "50px",
    width: "150px",
    whiteSpace: "nowrap",
    fontSize: "90%",
    outline: "none",
    backgroundColor: globalColors.mainColor,
    color: globalColors.lightColor,
    },
  emphasisChange: {
    backgroundColor: globalColors.emphasisColor,
  },
  hoverChange: {
    border: "medium solid " + globalColors.emphasisColor
  },
  hoverEmphasisChange: {
    border: "medium solid " + globalColors.mainColor
  }
}
// Flow types
type globalButtonType = {
  buttonText: string,
  emphasis: boolean,
  style?: Object,
  onClick: () => mixed,
}
type buttonState = {hover: boolean}

// Button element (has to be a class instead of function to handle hovering)
class GlobalButton extends React.Component<globalButtonType, buttonState> {
  hoverOn: () => mixed
  hoverOff: () => mixed
  baseStyle: Object
  static defaultProps = {
    buttonText: "Continue",
    emphasis: false,
    style: {}
  }
  constructor(props: globalButtonType){
    super(props)
    this.hoverOn = this.hoverOn.bind(this)
    this.hoverOff = this.hoverOff.bind(this)
    this.state = {hover: false}
  }
  hoverOn() {
    this.setState({hover: true})
  }
  hoverOff() {
    this.setState({hover: false})
  }
  render() {
    let baseStyle = this.props.emphasis ? (
      Object.assign({}, buttonStyles.buttonBase, buttonStyles.emphasisChange)
    ) : (
      buttonStyles.buttonBase
    )
    let hoverStyle = this.props.emphasis ? (
      buttonStyles.hoverEmphasisChange
    ) : (
      buttonStyles.hoverChange
    )

    let styleThis = this.state.hover ? (
      Object.assign({}, baseStyle, hoverStyle, this.props.style)
    ) : (
      Object.assign({}, baseStyle, this.props.style)
    )
    return (
      <button
        onClick={this.props.onClick}
        style={styleThis}
        onMouseOver={this.hoverOn}
        onMouseLeave={this.hoverOff}
        onFocus={this.hoverOn}
        onBlur={this.hoverOff}
      >
        {this.props.buttonText}
      </button>
    )
  }
}


// ***** define footer bar ***** //
// Styles
const lowerBarStyles = {
  barContainer: {
    alignSelf: "flex-end",
    display: "flex",
    alignItems: "center",
    width: "100%",
    textAlign: "center",
    backgroundColor: globalColors.darkColor,
    minHeight: "80px",
    color: globalColors.emphasisColor
  },
  leftContent: {
    display: "inline-block",
    verticalAlign: "middle",
    textAlign: "center",
    float: "left",
    width: "25%",
  },
  rightContent: {
    display: "inline-block",
    verticalAlign: "middle",
    textAlign: "center",
    float: "right",
    width: "25%",
  },
  centerContent: {
    display: "inline-block",
    verticalAlign: "middle",
    textAlign: "center",
    width: "50%",
    margin: "0 auto"
  }
}
// Flow type
type globalLowerBarType = {
  barStyle?: { [string]:string },
  leftStyle?: { [string]:string },
  leftContent?: React.Node,
  centerStyle?: { [string]:string },
  centerContent?: React.Node,
  rightStyle?: { [string]:string },
  rightContent?: React.Node,
}
/**
 * Footer bar with three sections (use in flexbox)
 */
function GlobalLowerBar(props: globalLowerBarType) {
  let styleBar = Object.assign({}, lowerBarStyles.barContainer, props.barStyle)
  let styleLeft = Object.assign({}, lowerBarStyles.leftContent, props.leftStyle)
  let styleRight = Object.assign({}, lowerBarStyles.leftContent, props.rightStyle)
  let styleCenter = Object.assign({}, lowerBarStyles.centerContent, props.centerStyle)
  return (
    <div className="globalLowerBar" style={styleBar}>
      <div className="globalLowerBarLeft" style={styleLeft}>
        {props.leftContent}
      </div>
      <div className="globalLowerBarCenter" style={styleCenter}>
        {props.centerContent}
      </div>
      <div className="globalLowerBarRight" style={styleRight}>
        {props.rightContent}
      </div>
    </div>
  )
}
GlobalLowerBar.defaultProps = {
  leftContent: "",
  leftStyle: {},
  centerContent: "",
  centerStyle: {},
  rightContent: "",
  rightStyle: {},
}

// ***** define long input text box (invisible) ***** //
type globalLongTextInputType = {
  autoFocus: boolean,
  className?: string,
  displayValue: string,
  onChangeFx: (SyntheticEvent<HTMLTextAreaElement>) => mixed,
  style?: Object
}
const globalLongTextInputStyle = {
  //height: "60%",
  width: "100%", /*same width as parent */
  boxSizing: "border-box",
  fontSize: "1.2em",
  fontFamily: "'Cousine', 'Courier New', serif",
  textAlign: "center",
  background: "transparent", /*make the box itself invisible*/
  border: "0", /*no border*/
  outline: "none", /*no outline when active (blue glow)*/
  resize: "none", /*no resize handle*/
  overflow: "auto", /* hide IE scroll bar */
  color: globalColors.otherColor
}
/**
 * Full page text box without border or background (use in flexbox).
 */
function GlobalLongTextInput(props: globalLongTextInputType) {
  return (
    <textarea
      autoFocus={props.autoFocus}
      className={props.className}
      value={props.displayValue}
      onChange={props.onChangeFx}
      style={{...globalLongTextInputStyle, ...props.style}}
    />
  )
}
GlobalLongTextInput.defaultProps = {
  autoFocus: true,
  displayValue: "",
}

const globals = {globalColors, globalButton: GlobalButton, globalLowerBar: GlobalLowerBar, globalLongTextInput: GlobalLongTextInput}

export default globals

export {
  globalColors,
  GlobalButton,
  GlobalLowerBar,
  GlobalLongTextInput
}