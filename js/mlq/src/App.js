import React, { Component } from 'react';
import Loneliness from "./loneliness"
import calculateScore from "./calculate-score"
import find from 'lodash/find'
import 'whatwg-fetch'
import $ from "jquery"

const userID = document.getElementById('user_id').innerHTML
const studyID = document.getElementById('study_id').innerHTML

class App extends Component {
  constructor() {
    super()
    this.endTask = this.endTask.bind(this)
    this.state = {isFinished: false}
  }
  endTask(name, data) {
    this.finalResult = calculateScore(name, data)
    const postData = Object.assign({}, this.finalResult['lonely'].plotdata, {user_id: userID,
      study_id: studyID})
    $.post({
      url: "/studies.json",
      headers: {
        'X-CSRF-Token': this.getCSRFToken(),
      },
      dataType: 'json',
      data: {
        study: postData
      }
    })
      .then(returnValue => window.location = returnValue.redirect_url)
    /*const postData = {
      method: 'POST',
      headers: {
        'X-CSRF-Token': this.getCSRFToken()
      },
      dataType: 'json',
      data: {
        study: {
          user_id: userID,
          study_id: studyID,
          custom_study_results: this.finalResult,
          score: this.finalResult.lonely.plotdata.score
        }
      }
    }
    fetch('/studies.json', postData)
      .then(() => console.log(postData, this.getCSRFToken()))*/

    this.setState({isFinished: true})
  }
  getCSRFToken() {
    return find(document.getElementsByTagName("meta"), (meta) => {
      return meta.name === "csrf-token"
    }).content
  }
  render() {
    return (
      this.state.isFinished ? (
        <div>
          Questionnaire completed. <br /> JSON data:
          <br />
          <pre>{JSON.stringify(this.finalResult, null, 2)}</pre>
        </div>
      ) : (
        <Loneliness endTask={this.endTask}/>
      )
    );
  }
}

export default App;
